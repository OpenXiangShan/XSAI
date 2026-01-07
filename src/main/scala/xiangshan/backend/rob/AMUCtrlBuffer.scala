package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import difftest._
import org.chipsalliance.cde.config.Parameters
import utility._
import utils.DebugMem
import xiangshan._
import xiangshan.backend.BackendParams
import xiangshan.backend.Bundles.DynInst
import xiangshan.backend.fu.matrix.Bundles._

class AmuCtrlBufferIO()(implicit val p: Parameters, val params: BackendParams) extends Bundle with HasXSParameter {
  // rob enq
  class EnqBundle()(implicit val p: Parameters) extends Bundle with HasXSParameter {
    val valid = Bool()  // Req valid and canEnqueue
    val reqValid = Bool()  // Req valid
    val allocPtr = new RobPtr
    val needAMU = Bool()
    val pc = Option.when(env.EnableDifftest && HasMatrixExtension)(UInt(VAddrBits.W))
    val pcTransType = Option.when(env.EnableDifftest && HasMatrixExtension)(new AddrTransType)
  }
  val enq = Input(Vec(RenameWidth, new EnqBundle))
  val canEnqueue = Output(Bool())
  val canEnqueueForDispatch = Output(Bool())

  // rob wb
  val wb = Flipped(params.genWrite2CtrlBundles)

  // Commit
  class DeqCommitBundle extends Bundle {
    val ptr = new RobPtr
    val valid = Bool()
  }
  val deqCommit = Input(Vec(CommitWidth, new DeqCommitBundle))

  // Redirect (no need to walk)
  class RedirectBundle extends Bundle {
    val valid = Bool()
    val all = Bool()
    val begin = UInt(log2Up(RobSize).W)
    val end = UInt(log2Up(RobSize).W)
  }
  val redirect = Input(new RedirectBundle)

  // To Amu
  val toAMU = Vec(CommitWidth, DecoupledIO(new AmuCtrlIO))

  // If we enable difftest, there will be hartid
  val hartId = Option.when(env.EnableDifftest && HasMatrixExtension)(Input(UInt(hartIdLen.W)))
}


class AmuCtrlEntry(implicit p: Parameters) extends XSBundle {
  val valid = Bool()
  val needAMU = Bool()
  val writebacked = Bool()
  val committed = Bool()
  val amuCtrl = new AmuCtrlIO
  val canDeq = Bool()

  def amuReqValid = valid && needAMU && writebacked && committed && !canDeq

  def checkSanity(name: String) = {
    // Check valid sequence: canDeq -> committed -> writebacked -> valid
    assert(!canDeq || committed, s"$name: committed must be valid when canDeq is valid")
    assert(!committed || (!needAMU || writebacked), s"$name: writebacked must be valid when committed is valid") 
    assert(!committed || valid, s"$name: valid must be valid when committed is valid")
    assert(!needAMU || valid, s"$name: valid must be valid when needAMU is valid")
    assert(!writebacked || valid, s"$name: valid must be valid when writebacked is valid")
  }
}

object AmuCtrlEntry {
  def apply()(implicit p: Parameters) = {
    new AmuCtrlEntry
  }

  def zero(implicit p: Parameters) = {
    val default_zero = 0.U.asTypeOf(new AmuCtrlEntry)
    assert(default_zero.getWidth > 0)
    default_zero
  }
}

/**
 * AMU Control Buffer
 *
 * Purpose:
 * A dedicated buffer parallel to ROB for storing AMU control signals, avoiding storing them in ROB directly.
 * The buffer size matches ROB size.
 *
 * Operation:
 * 1. Instructions enter this buffer synchronously when entering ROB
 * 2. When buffer is full, it will backpressure ROB enqueue
 * 3. Synchronizes with ROB writeback and flush, no need for walking to rebuild context
 * 4. After ROB commit, starts handshake with AMU and dequeues after handshake fires
 */
class AmuCtrlBuffer()(implicit override val p: Parameters, val params: BackendParams) extends XSModule
  with HasXSParameter with HasCircularQueuePtrHelper {

  val io = IO(new AmuCtrlBufferIO)

  def connectROB(robImp: RobImp) = {
    for (i <- 0 until RenameWidth) {
      io.enq(i).valid := robImp.io.enq.req(i).valid && robImp.canEnqueue(i) && !robImp.io.redirect.valid
      io.enq(i).reqValid := robImp.io.enq.req(i).valid
      io.enq(i).allocPtr := robImp.allocatePtrVec(i)
      io.enq(i).needAMU := robImp.io.enq.req(i).bits.needAmuCtrl

      if (env.EnableDifftest && HasMatrixExtension) {
        io.enq(i).pc.get := robImp.io.enq.req(i).bits.pc
        io.enq(i).pcTransType.get := robImp.io.debugInstrAddrTransType
      }
    }
    io.wb := robImp.io.writeback
    io.deqCommit.zip(robImp.deqPtrVec).zip(robImp.io.commits.commitValid) foreach { case ((deq, ptr), valid) =>
      deq.ptr := ptr
      deq.valid := valid && robImp.io.commits.isCommit
    }
    io.redirect.valid := robImp.redirectValidReg
    io.redirect.all := robImp.redirectAll
    io.redirect.begin := robImp.redirectBegin
    io.redirect.end := robImp.redirectEnd
    robImp.io.amuCtrl <> io.toAMU
    if (env.EnableDifftest && HasMatrixExtension) {
      io.hartId.get := robImp.io.hartId
    }
  }

  val amuCtrlEntries = RegInit(VecInit.fill(RobSize)(AmuCtrlEntry.zero))

  // Difftest
  val dt_pc = Option.when(env.EnableDifftest && HasMatrixExtension)(DebugMem(RobSize, UInt(VAddrBits.W)))
  val dt_pcTransType = Option.when(env.EnableDifftest && HasMatrixExtension)(Mem(RobSize, new AddrTransType))

  // Calculate number of valid entries and new entries to be enqueued
  val numValidEntries = PopCount(amuCtrlEntries.map(_.valid))
  val numNewEntries = PopCount(io.enq.map(_.reqValid))

  // Check if there's enough space in the queue
  val canEnqueue = GatedValidRegNext(
    numValidEntries + numNewEntries <= (RobSize - RenameWidth).U,
    true.B
  )
  val canEnqueueForDispatch = GatedValidRegNext(
    numValidEntries + numNewEntries <= (RobSize - RenameWidth * 2).U,
    true.B
  )
  io.canEnqueue := canEnqueue
  io.canEnqueueForDispatch := canEnqueueForDispatch

  // Enqueue (Sync with outer ROB)
  // DynInst does not carry amuCtrl info,
  // so we only need to mark valid for entries who need amuCtrl.
  for (i <- 0 until RobSize) {
    val indexMatch = io.enq.map(_.allocPtr.value === i.U)
    val enqOH = VecInit(io.enq.zip(indexMatch).map(x => x._1.valid && x._2))
    val needOH = VecInit(io.enq.zip(enqOH).map(x => x._1.needAMU && x._2))
    val entry = amuCtrlEntries(i)
    when (enqOH.asUInt.orR) {
      entry := AmuCtrlEntry.zero
      entry.valid := true.B
      entry.needAMU := needOH.asUInt.orR
      if (env.EnableDifftest && HasMatrixExtension) {
        val pcData = Mux1H(
          io.enq.zip(indexMatch).map { case (enq, imatch) => imatch && enq.valid },
          io.enq.map(_.pc.get)
        )
        val pcTransTypeData = Mux1H(
          io.enq.zip(indexMatch).map { case (enq, imatch) => imatch && enq.valid },
          io.enq.map(_.pcTransType.get)
        )
        
        dt_pc.get(i) := pcData
        dt_pcTransType.get(i) := pcTransTypeData
      }
    }
  }

  // Writeback (Sync with outer ROB)
  val amuCtrlWb = io.wb.filter(_.bits.amuCtrl.nonEmpty).toSeq
  for (i <- 0 until RobSize) {
    val amu_data = amuCtrlWb.map{ wb =>
      val valid_match = wb.valid && wb.bits.robIdx.value === i.U
      Mux(valid_match, wb.bits.amuCtrl.get.asUInt, 0.U)
    }.reduce(_ | _)

    val entry = amuCtrlEntries(i)
    assert(!amu_data.orR || entry.valid, s"AMUCtrl entry $i is invalid but has amu_data")
    when (amu_data.orR) {
      entry.writebacked := true.B
      entry.amuCtrl := amu_data.asTypeOf(new AmuCtrlIO)
    }
  }

  // Commit (Sync with outer ROB)
  for (i <- 0 until RobSize) {
    val deqValid = io.deqCommit.map(x => x.ptr.value === i.U && x.valid)
    val commitCond = deqValid.reduce(_ || _)
    when (commitCond) {
      assert(amuCtrlEntries(i).valid, s"AMUCtrlBuffer: amuCtrlEntries[$i] is invalid but will commit")
      amuCtrlEntries(i).committed := true.B
      when (!amuCtrlEntries(i).needAMU) {
        amuCtrlEntries(i).canDeq := true.B
      }
    }
  }

  // Redirect (Sync with outer ROB)
  for (i <- 0 until RobSize) {
    val needFlush = io.redirect.valid &&
      Mux((io.redirect.end > io.redirect.begin) && !io.redirect.all,
        (i.U > io.redirect.begin) && (i.U < io.redirect.end),
        (i.U > io.redirect.begin) || (i.U < io.redirect.end)
    )

    when (needFlush) {
      amuCtrlEntries(i) := AmuCtrlEntry.zero
    }
  }

  // To AMU
  val deqPtr = RegInit(0.U.asTypeOf(new RobPtr))
  val deqEntries = (0 until CommitWidth).map(i => amuCtrlEntries((deqPtr + i.U).value))

  class AmuCtrlTableEntry extends Bundle {
    val op = UInt(2.W)
    val mma = new AmuMmaIO
    val mls = new AmuLsuIO
    val mrelease = new AmuReleaseIO2CUTE
    val arith = new AmuArithIO
  }
  val amuCtrl_table = ChiselDB.createTable(s"AMUCtrl_table", new AmuCtrlTableEntry, basicDB = true)

  io.toAMU.zipWithIndex.foreach { case (amuCtrl, i) =>
    val deqEntry = deqEntries(i)
    deqEntry.checkSanity(s"AMUCtrlBuffer: deqEntry[$i]")
    amuCtrl.valid := deqEntry.amuReqValid
    amuCtrl.bits := Mux(deqEntry.amuReqValid, deqEntry.amuCtrl, 0.U.asTypeOf(new AmuCtrlIO))
    when (amuCtrl.fire) {
      assert(!deqEntry.canDeq, s"AMUCtrlBuffer: deqEntry[$i] is already set to canDeq")
      deqEntry.canDeq := true.B
    }
    val mmaio = amuCtrl.bits.data.asTypeOf(new AmuMmaIO)
    val mlsio = amuCtrl.bits.data.asTypeOf(new AmuLsuIO)
    val mreleaseio = amuCtrl.bits.data.asTypeOf(new AmuReleaseIO2CUTE)
    val arithio = amuCtrl.bits.data.asTypeOf(new AmuArithIO)
    val amuCtrl_table_entry = Wire(new AmuCtrlTableEntry)
    amuCtrl_table_entry.op := amuCtrl.bits.op
    when(amuCtrl.bits.isMma()) {
      amuCtrl_table_entry.mma := mmaio
    }.otherwise {
      amuCtrl_table_entry.mma := 0.U.asTypeOf(new AmuMmaIO)
    }
    when(amuCtrl.bits.isMls()) {
      amuCtrl_table_entry.mls := mlsio
    }.otherwise {
      amuCtrl_table_entry.mls := 0.U.asTypeOf(new AmuLsuIO)
    }
    when(amuCtrl.bits.isRelease()) {
      amuCtrl_table_entry.mrelease := mreleaseio
    }.otherwise {
      amuCtrl_table_entry.mrelease := 0.U.asTypeOf(new AmuReleaseIO2CUTE)
    }
    when(amuCtrl.bits.isArith()) {
      amuCtrl_table_entry.arith := arithio
    }.otherwise {
      amuCtrl_table_entry.arith := 0.U.asTypeOf(new AmuArithIO)
    }
    amuCtrl_table.log(
      data = amuCtrl_table_entry,
      en = amuCtrl.fire,
      site = s"AMUCtrl_table",
      clock = clock,
      reset = reset
    )

    // Difftest
    if (env.EnableDifftest && HasMatrixExtension) {
      val difftestPC = dt_pc.get((deqPtr + i.U).value)
      val difftestAmuCtrl = DifftestModule(new DiffAmuCtrlEvent, delay = 3, dontCare = true)
      difftestAmuCtrl.valid  := amuCtrl.fire
      difftestAmuCtrl.op     := amuCtrl.bits.op
      val pcTransType = dt_pcTransType.get((deqPtr + i.U).value)
      difftestAmuCtrl.pc     := Mux(pcTransType.shouldBeSext, SignExt(difftestPC, XLEN), difftestPC)
      difftestAmuCtrl.coreid := io.hartId.get
      difftestAmuCtrl.index  := i.U
      when (amuCtrl.bits.isMma()) {
        // difftestAmuCtrl.md       := mmaio.md
        // difftestAmuCtrl.sat      := mmaio.sat
        // difftestAmuCtrl.isfp     := mmaio.isfp
        // difftestAmuCtrl.issigned := mmaio.issigned
        // difftestAmuCtrl.ms1      := mmaio.ms1
        // difftestAmuCtrl.ms2      := mmaio.ms2
        // difftestAmuCtrl.mtilem   := mmaio.mtilem
        // difftestAmuCtrl.mtilen   := mmaio.mtilen
        // difftestAmuCtrl.mtilek   := mmaio.mtilek
        // difftestAmuCtrl.types    := mmaio.types
        // difftestAmuCtrl.typed    := mmaio.typed

        difftestAmuCtrl.rm       := mmaio.rm
        difftestAmuCtrl.md       := mmaio.md
        difftestAmuCtrl.sat      := mmaio.sat
        difftestAmuCtrl.ms1      := mmaio.ms1
        difftestAmuCtrl.ms2      := mmaio.ms2
        difftestAmuCtrl.mtilem   := mmaio.mtilem
        difftestAmuCtrl.mtilen   := mmaio.mtilen
        difftestAmuCtrl.mtilek   := mmaio.mtilek
        difftestAmuCtrl.types1   := mmaio.types1
        difftestAmuCtrl.types2   := mmaio.types2
        difftestAmuCtrl.typed    := mmaio.typed
        difftestAmuCtrl.isfp     := mmaio.isfp
      }
      when (amuCtrl.bits.isMls()) {
        difftestAmuCtrl.ms        := mlsio.ms
        difftestAmuCtrl.ls        := mlsio.ls
        difftestAmuCtrl.transpose := mlsio.transpose
        difftestAmuCtrl.types1    := mlsio.isacc
        difftestAmuCtrl.base      := mlsio.baseAddr
        difftestAmuCtrl.stride    := mlsio.stride
        difftestAmuCtrl.row       := mlsio.row
        difftestAmuCtrl.column    := mlsio.column
        difftestAmuCtrl.widths    := mlsio.widths
      }
      when (amuCtrl.bits.isRelease()) {
        difftestAmuCtrl.tokenRd := mreleaseio.tokenRd
      }
      when (amuCtrl.bits.isArith()) {
        difftestAmuCtrl.ms     := arithio.md
        difftestAmuCtrl.opType := arithio.opType
      }
    }
  }

  val deqCondSeq = deqEntries.map(x => ~x.canDeq)
  val deqCount = PriorityEncoder(deqCondSeq)
  // Update deqPtr and invalidate entries
  deqPtr := deqPtr + deqCount
  for (i <- 0 until CommitWidth) {
    val curr_deq_ptr = deqPtr + i.U
    when(amuCtrlEntries(curr_deq_ptr.value).canDeq && i.U < deqCount) {
      amuCtrlEntries(curr_deq_ptr.value) := AmuCtrlEntry.zero
    }
  }

  XSPerfAccumulate("stall_by_matrix_fire", !io.canEnqueue && deqPtr =/= io.deqCommit(0).ptr)
}