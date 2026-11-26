package xiangshan.backend.decode

import chisel3._
import chiseltest._
import cute.{CuteParams, CuteParamsKey, MatrixIsaParams}
import freechips.rocketchip.rocket.Instructions.{MLA, MLB}
import org.chipsalliance.cde.config.Parameters
import utility.{LogUtilsOptions, LogUtilsOptionsKey}
import xiangshan.{ExceptionNO, MldstOpType, XSCoreParamsKey, XSTester}
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.matrix.Bundles.{AmuLsuIO, AmuMmaIO, AmuReleaseIO2XS}
import xiangshan.backend.fu.wrapper.Mma
import xiangshan.mem.MlsUnit
import chisel3.reflect.DataMirror
import chisel3.util._
import chiseltest.experimental.expose
import freechips.rocketchip.diplomacy.LazyModule
import xiangshan.{CSROpType, DebugOptionsKey, RedirectLevel}
import xiangshan.backend.rob.{AmuCtrlBuffer, Rob}
import xiangshan.backend.fu.wrapper.CSR
import xiangshan.backend.fu.Fence
import xiangshan.FenceOpType
import freechips.rocketchip.rocket.CSRs
import xiangshan.backend.fu.wrapper.VCVT
import xiangshan.backend.fu.vector.Bundles.VConfig
import xiangshan.backend.exu.Dispatcher
import scala.collection.mutable

class MxfpProducerSpec extends XSTester {
  if (sys.env.get("CUTE_TEST_VERILATOR").contains("1")) {
    testAnnos = testAnnos ++ Seq(VerilatorBackendAnnotation,
      chiseltest.simulator.VerilatorCFlags(Seq("-DWData=uint32_t")))
  }
  // Test-only wiring around the real owners; no copied MINIT state machine.
  private def tieInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(tieInputs)
    case vector: Vec[_] => vector.foreach(tieInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input =>
      leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }

  private def vectorEncodeParams(enabled: Boolean): Parameters = {
    val core = config(XSCoreParamsKey).copy(ExperimentalMxfp4VectorEncode = enabled)
    config.alterPartial {
      case XSCoreParamsKey => core
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false, enablePerf = false, fpgaPlatform = false, enableXMR = false)
    }
  }

  private class VectorEncodeDecodeProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val inst = Input(UInt(32.W))
      val sew = Input(UInt(2.W))
      val lmul = Input(UInt(3.W))
      val vstart = Input(UInt(8.W))
      val vill = Input(Bool())
      val fsOff = Input(Bool())
      val vsOff = Input(Bool())
      val badFrm = Input(Bool())
      val illegal = Output(Bool())
      val complex = Output(Bool())
      val flagsWrite = Output(Bool())
      val uops = Output(UInt(8.W))
      val op = Output(UInt(9.W))
    })
    val decoder = Module(new DecodeUnit()(p))
    tieInputs(decoder.io)
    decoder.io.enq.ctrlFlow.instr := io.inst
    decoder.io.enq.vtype.vsew := io.sew
    decoder.io.enq.vtype.vlmul := io.lmul
    decoder.io.enq.vtype.illegal := io.vill
    decoder.io.enq.vstart := io.vstart
    decoder.io.fromCSR.illegalInst.fsIsOff := io.fsOff
    decoder.io.fromCSR.illegalInst.vsIsOff := io.vsOff
    decoder.io.fromCSR.illegalInst.frm := io.badFrm
    val exceptions = Module(new VecExceptionGen()(p))
    exceptions.io.inst := io.inst
    exceptions.io.decodedInst := decoder.io.deq.decodedInst
    exceptions.io.vtype := decoder.io.enq.vtype
    exceptions.io.vstart := io.vstart
    io.illegal := decoder.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr) ||
      exceptions.io.illegalInst
    io.complex := decoder.io.deq.isComplex
    io.flagsWrite := decoder.io.deq.decodedInst.wfflags
    io.uops := decoder.io.deq.uopInfo.numOfUop
    io.op := decoder.io.deq.decodedInst.fuOpType
  }

  it should "R37 lint the experimental vector encoding against all CPU and Rocket patterns" in {
    def overlaps(a: BitPat, b: BitPat): Boolean =
      ((a.value ^ b.value) & a.mask & b.mask) == 0
    val candidate = Mxfp4VectorDecode.ENCODE
    val cpuStatic = (XDecode.table ++ FpDecode.table ++ BitmanipDecode.table ++
      ScalarCryptoDecode.table ++ XSDebugDecode.table ++ CBODecode.table ++
      SvinvalDecode.table ++ HypervisorDecode.table ++ VecDecoder.table(hasVfexp2 = true) ++
      MatrixDecoder.decodeArray.map { case (bits, dec) => bits -> dec.generate() } ++
      ZicondDecode.table ++ ZimopDecode.table ++ ZfaDecode.table ++ ZfbfDecode.table).map(_._1)
    for (other <- cpuStatic) assert(!overlaps(candidate, other), s"FP4 conflicts with CPU $other")
    for (owner <- Seq(freechips.rocketchip.rocket.Instructions,
        freechips.rocketchip.rocket.CustomInstructions);
        method <- owner.getClass.getMethods.toSeq
        if method.getParameterCount == 0 && method.getReturnType == classOf[BitPat]) {
      val pattern = method.invoke(owner).asInstanceOf[BitPat]
      assert(!overlaps(candidate, pattern), s"FP4 conflicts with Rocket ${method.getName}")
    }
    assert(!Mxfp4VectorDecode.enabled(Parameters.empty))
    assert(Mxfp4VectorDecode.table(false).isEmpty)
    assert(Mxfp4VectorDecode.table(true).length == 1)
    assert(Mxfp4VectorDecode.encodeOp.value > 255)
    assert(Mxfp4VectorDecode.encodeOp.value != yunsuan.VfcvtType.fcvtmod_w_d.litValue)
  }

  it should "R37 gate real vector decode classification permissions and legality" in {
    val instruction = Mxfp4VectorDecode.ENCODE.value | (BigInt(1) << 25) |
      (BigInt(8) << 20) | (BigInt(16) << 15) | (BigInt(24) << 7)
    for (enabled <- Seq(false, true)) {
      test(new VectorEncodeDecodeProbe()(vectorEncodeParams(enabled))).withAnnotations(testAnnos) { d =>
        d.io.inst.poke(instruction.U); d.io.sew.poke(2.U); d.io.lmul.poke(0.U)
        d.io.vstart.poke(0.U); d.io.vill.poke(false.B)
        d.io.fsOff.poke(false.B); d.io.vsOff.poke(false.B); d.io.badFrm.poke(false.B)
        d.io.illegal.expect((!enabled).B)
        if (enabled) {
          d.io.complex.expect(true.B); d.io.flagsWrite.expect(true.B)
          d.io.op.expect(Mxfp4VectorDecode.encodeOp.value.U)
          for (lmul <- Seq(6, 7, 0, 1, 2, 3)) {
            d.io.lmul.poke(lmul.U)
            d.io.illegal.expect(false.B)
            d.io.uops.expect((if (lmul <= 3) 1 << lmul else 1).U)
          }
          d.io.lmul.poke(0.U)
          for (sew <- Seq(0, 1, 3)) {
            d.io.sew.poke(sew.U); d.io.illegal.expect(true.B)
          }
          d.io.sew.poke(2.U)
          for (port <- Seq(d.io.vill, d.io.fsOff, d.io.vsOff, d.io.badFrm)) {
            port.poke(true.B); d.io.illegal.expect(true.B); port.poke(false.B)
          }
          d.io.vstart.poke(1.U); d.io.illegal.expect(true.B); d.io.vstart.poke(0.U)
          // A different custom3 pattern must not acquire vector classification.
          d.io.inst.poke((instruction ^ (BigInt(1) << 26)).U)
          d.io.complex.expect(false.B); d.io.flagsWrite.expect(false.B)
          d.io.illegal.expect(true.B)
        }
      }
    }
  }

  private class VectorEncodeProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val valid = Input(Bool())
      val flush = Input(Bool())
      val legacy = Input(Bool())
      val values = Input(Vec(4, UInt(32.W)))
      val scales = Input(Vec(4, UInt(32.W)))
      val old = Input(Vec(4, UInt(32.W)))
      val mask = Input(UInt(32.W))
      val vl = Input(UInt(8.W))
      val lmul = Input(UInt(3.W))
      val index = Input(UInt(3.W))
      val vm = Input(Bool())
      val ma = Input(Bool())
      val ta = Input(Bool())
      val ready = Output(Bool())
      val outValid = Output(Bool())
      val result = Output(Vec(4, UInt(32.W)))
      val flags = Output(UInt(5.W))
      val illegal = Output(Bool())
    })
    val convert = Module(new VCVT(FuConfig.VfcvtCfg)(p))
    tieInputs(convert.io)
    convert.io.in.valid := io.valid
    convert.io.out.ready := true.B
    convert.io.flush.valid := io.flush
    convert.io.flush.bits.level := RedirectLevel.flush
    convert.io.in.bits.ctrl.fuOpType := Mux(io.legacy,
      yunsuan.VfcvtType.vfcvt_xufv, Mxfp4VectorDecode.encodeOp.value.U(9.W))
    convert.io.in.bits.data.src(0) := io.scales.asUInt
    convert.io.in.bits.data.src(1) := io.values.asUInt
    convert.io.in.bits.data.src(2) := io.old.asUInt
    convert.io.in.bits.data.getSrcMask := io.mask
    val vconfig = WireInit(0.U.asTypeOf(new VConfig()(p)))
    vconfig.vl := io.vl
    vconfig.vtype.vsew := 2.U
    vconfig.vtype.vlmul := io.lmul
    convert.io.in.bits.data.getSrcVConfig := vconfig.asUInt
    val control = convert.io.in.bits.ctrl.vpu.get
    control.vsew := 2.U
    control.vlmul := io.lmul
    control.vuopIdx := io.index
    control.vm := io.vm
    control.vma := io.ma
    control.vta := io.ta
    // A piped FU receives EXU-owned control/data/valid stages in addition to
    // its instantaneous request. Reuse ExeUnit.pipelineReg's exact valid
    // and needFlush recurrence; zeroing these ports would disable writeback.
    val pipeControl = convert.io.in.bits.ctrl +:
      Seq.fill(2)(Reg(chiselTypeOf(convert.io.in.bits.ctrl)))
    val pipeData = convert.io.in.bits.data +:
      Seq.fill(2)(Reg(chiselTypeOf(convert.io.in.bits.data)))
    val pipeValid = io.valid +: Seq.fill(2)(RegInit(false.B))
    for (stage <- 1 to 2) {
      pipeValid(stage) := pipeValid(stage - 1) &&
        !pipeControl(stage - 1).robIdx.needFlush(convert.io.flush)
      pipeControl(stage) := pipeControl(stage - 1)
      pipeData(stage) := pipeData(stage - 1)
    }
    convert.io.in.bits.ctrlPipe.get := VecInit(pipeControl)
    convert.io.in.bits.dataPipe.get := VecInit(pipeData)
    convert.io.in.bits.validPipe.get := VecInit(pipeValid)
    io.ready := convert.io.in.ready
    io.outValid := convert.io.out.valid
    io.result := convert.io.out.bits.res.data.asTypeOf(io.result)
    io.flags := convert.io.out.bits.res.fflags.get
    io.illegal := convert.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
  }

  it should "R37 use real VCVT masks tails pipeline flush and CPU flag output" in {
    val params = vectorEncodeParams(true)
    bindMinitBackend(params)
    case class Request(values: Vector[Long], scales: Vector[Long], expected: Vector[Long],
        mask: Long = 0xffffffffL, vl: Int = 4, index: Int = 0, lmul: Int = 0,
        vm: Boolean = true, ma: Boolean = false, ta: Boolean = false,
        flags: Int = 0, legacy: Boolean = false)
    val old = Vector(0xa5a50000L, 0xa5a51111L, 0xa5a52222L, 0xa5a53333L)
    val values = Vector(0x3f800000L, 0x3fc00000L, 0x80000000L, 0xc0a00000L)
    val codes = Vector(1L, 1L, 0L, 12L) // e128: d=1, 1.5 tie lowers, -5 tie -> -4.
    val normal = Request(values, Vector.fill(4)(128L), codes)
    val bad = Request(Vector(0x7fc00001L, 0x7f800000L, 0x3f800000L, 0x3f800000L),
      Vector(128L, 128L, 256L, 252L), Vector.fill(4)(0L), flags = 16)
    val requests = Vector(normal, bad,
      normal.copy(values = Vector.fill(4)(0x3f800000L),
        scales = Vector(124L, 125L, 126L, 127L), expected = Vector(7L, 6L, 4L, 2L)),
      normal.copy(values = Vector(values(0), 0x7fc00001L, values(2), 0x7f800000L),
        scales = Vector(128L, 256L, 128L, 252L), mask = 5, vm = false,
        expected = Vector(codes(0), old(1), codes(2), old(3))),
      normal.copy(values = values.take(2) ++ Vector(0x7fc00001L, 0x7f800000L),
        scales = Vector(128L, 128L, 256L, 252L), vl = 2,
        expected = codes.take(2) ++ old.drop(2)),
      bad.copy(mask = 0, vm = false, expected = old, flags = 0),
      bad.copy(vl = 0, expected = old, flags = 0),
      bad.copy(vl = 0, ta = true, expected = old, flags = 0),
      normal.copy(mask = 5, vm = false, expected = Vector(codes(0), old(1), codes(2), old(3))),
      normal.copy(mask = 5, vm = false, ma = true, expected = Vector(codes(0), 0xffffffffL, codes(2), 0xffffffffL)),
      normal.copy(vl = 2, expected = codes.take(2) ++ old.drop(2)),
      normal.copy(vl = 2, ta = true, expected = codes.take(2) ++ Vector.fill(2)(0xffffffffL)),
      normal.copy(lmul = 7, vl = 2, expected = codes.take(2) ++ old.drop(2)),
      normal.copy(lmul = 6, vl = 1, expected = codes.take(1) ++ old.drop(1)),
      normal.copy(lmul = 3, vl = 30, index = 7, expected = codes.take(2) ++ old.drop(2)),
      normal.copy(lmul = 3, vl = 25, index = 7, expected = old),
      normal.copy(values = Vector(0L, 0x3f800000L, 0x40000000L, 0x40400000L),
        expected = Vector(0L, 1L, 2L, 3L), legacy = true),
      normal, bad, normal)
    test(new VectorEncodeProbe()(params)).withAnnotations(testAnnos) { d =>
      d.io.valid.poke(false.B); d.io.flush.poke(false.B); d.io.legacy.poke(false.B)
      d.io.values.foreach(_.poke(0.U)); d.io.scales.foreach(_.poke(0.U))
      d.io.old.zip(old).foreach { case (port, value) => port.poke(value.U) }
      d.io.mask.poke(0.U); d.io.vl.poke(0.U); d.io.lmul.poke(0.U); d.io.index.poke(0.U)
      d.io.vm.poke(true.B); d.io.ma.poke(false.B); d.io.ta.poke(false.B)
      d.clock.step(3)
      val pending = mutable.Queue.empty[Request]
      var returned = 0
      for (request <- requests.map(Some(_)) ++ Vector.fill(5)(None)) {
        if (d.io.outValid.peek().litToBoolean) {
          assert(pending.nonEmpty)
          val expected = pending.dequeue()
          d.io.result.zip(expected.expected).foreach { case (port, value) => port.expect(value.U) }
          d.io.flags.expect(expected.flags.U); d.io.illegal.expect(false.B)
          returned += 1
        }
        d.io.valid.poke(request.nonEmpty.B)
        request.foreach { r =>
          d.io.values.zip(r.values).foreach { case (port, value) => port.poke(value.U) }
          d.io.scales.zip(r.scales).foreach { case (port, value) => port.poke(value.U) }
          d.io.mask.poke(r.mask.U); d.io.vl.poke(r.vl.U); d.io.index.poke(r.index.U)
          d.io.lmul.poke(r.lmul.U); d.io.vm.poke(r.vm.B)
          d.io.ma.poke(r.ma.B); d.io.ta.poke(r.ta.B); d.io.legacy.poke(r.legacy.B)
          d.io.ready.expect(true.B)
          pending.enqueue(r)
        }
        d.clock.step()
      }
      assert(pending.isEmpty && returned == requests.length)
      // Kill a real in-flight conversion through the existing ROB/flush path.
      d.io.valid.poke(true.B); d.clock.step()
      d.io.valid.poke(false.B); d.io.flush.poke(true.B); d.clock.step()
      d.io.flush.poke(false.B)
      for (_ <- 0 until 4) { d.io.outValid.expect(false.B); d.clock.step() }
      println(s"R37 real VCVT PASS requests=$returned, masked/tail NV suppressed, flush drained")
    }
  }

  private lazy val minitParams: Parameters = {
    // CDE evaluates partial functions on every lookup. Capture the core once,
    // so the backend bindings below apply to the same parameter objects.
    val core = config(XSCoreParamsKey).copy(MatrixExtension = mx,
      RobSize = 16, RabSize = 64)
    val debug = config(DebugOptionsKey).copy(
      EnableDifftest = false, AlwaysBasicDiff = false, AlwaysBasicDB = false)
    config.alterPartial {
      case XSCoreParamsKey => core
      case DebugOptionsKey => debug
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false, enablePerf = false, fpgaPlatform = false, enableXMR = false)
      case utility.PerfCounterOptionsKey => utility.PerfCounterOptions(
        enablePerfPrint = false, enablePerfDB = false,
        perfLevel = utility.XSPerfLevel.NORMAL, perfDBHartID = 0)
    }
  }

  private class SelectedReadyDispatchProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val valid = Input(Bool())
      val select = Input(UInt(2.W))
      val payload = Input(UInt(32.W))
      val branchReady = Input(Vec(4, Bool()))
      val ready = Output(Bool())
      val branchValid = Output(Vec(4, Bool()))
      val branchPayload = Output(Vec(4, UInt(32.W)))
    })
    val dispatcher = Module(new Dispatcher(UInt(34.W), 4,
      (request: UInt) => (0 until 4).map(i => request(1, 0) === i.U))(p))
    dispatcher.io.in.valid := io.valid
    dispatcher.io.in.bits := Cat(io.payload, io.select)
    io.ready := dispatcher.io.in.ready
    for (i <- 0 until 4) {
      dispatcher.io.out(i).ready := io.branchReady(i)
      io.branchValid(i) := dispatcher.io.out(i).valid
      io.branchPayload(i) := dispatcher.io.out(i).bits(33, 2)
    }
  }

  it should "R38 selected-ready conserve Dispatcher handshakes under every sibling ready mask" in {
    test(new SelectedReadyDispatchProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      for (selected <- 0 until 4; mask <- 0 until 16) {
        val payload = BigInt("83a50000", 16) + selected * 16 + mask
        d.io.valid.poke(true.B)
        d.io.select.poke(selected.U)
        d.io.payload.poke(payload.U)
        for (i <- 0 until 4)
          d.io.branchReady(i).poke(((mask & (1 << i)) != 0).B)
        val selectedReady = (mask & (1 << selected)) != 0
        d.io.ready.expect(selectedReady.B)
        for (i <- 0 until 4) {
          d.io.branchValid(i).expect((i == selected).B)
          d.io.branchPayload(i).expect(payload.U)
        }
        val childFires = (0 until 4).count(i =>
          d.io.branchValid(i).peek().litToBoolean &&
            d.io.branchReady(i).peek().litToBoolean)
        assert(childFires == (if (selectedReady) 1 else 0),
          s"selection=$selected readyMask=$mask lost or duplicated a transaction")
        d.clock.step()
        d.io.valid.poke(false.B)
        for (i <- 0 until 4) d.io.branchValid(i).expect(false.B)
        d.clock.step()
      }
    }
  }

  private class DispatchedFenceProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val valid = Input(Bool())
      val op = Input(UInt(9.W))
      val value = Input(UInt(64.W))
      val siblingReady = Input(Bool())
      val returnValid = Input(Bool())
      val ready = Output(Bool())
      val parentAccepted = Output(Bool())
      val fenceAccepted = Output(Bool())
      val completed = Output(Bool())
      val returnReady = Output(Bool())
      val sync1 = Output(UInt(64.W))
    })
    val dispatcher = Module(new Dispatcher(UInt(73.W), 2,
      (_: UInt) => Seq(false.B, true.B))(p))
    val fence = Module(new Fence(FuConfig.FenceCfg)(p))
    tieInputs(fence.io)
    dispatcher.io.in.valid := io.valid
    dispatcher.io.in.bits := Cat(io.op, io.value)
    dispatcher.io.out(0).ready := io.siblingReady
    dispatcher.io.out(1).ready := fence.io.in.ready
    fence.io.in.valid := dispatcher.io.out(1).valid
    fence.io.in.bits.ctrl.fuOpType := dispatcher.io.out(1).bits(72, 64)
    fence.io.in.bits.data.src(0) := dispatcher.io.out(1).bits(63, 0)
    // Same Rename packed logical-source fields as the real sync1 request.
    fence.io.in.bits.data.imm := (1 << 6).U
    fence.io.out.ready := true.B
    fence.io.fenceio.get.sbuffer.sbIsEmpty := true.B
    val returned = fence.io.fenceio.get.amuRelease.get
    returned.valid := io.returnValid
    returned.bits := 0.U.asTypeOf(returned.bits)
    returned.bits.msyncRd(1) := true.B
    io.ready := dispatcher.io.in.ready
    io.parentAccepted := dispatcher.io.in.fire
    io.fenceAccepted := fence.io.in.fire
    io.completed := fence.io.out.valid
    io.returnReady := returned.ready
    io.sync1 := expose(fence.msyncRegs)(1)
  }

  it should "R38 selected-ready execute one real Fence reset and acquire despite a stalled sibling" in {
    test(new DispatchedFenceProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.valid.poke(false.B)
      d.io.op.poke(FenceOpType.msyncregreset)
      d.io.value.poke(0.U)
      d.io.siblingReady.poke(false.B)
      d.io.returnValid.poke(false.B)
      d.clock.step(2)
      d.io.valid.poke(true.B)
      d.io.ready.expect(true.B)
      d.io.parentAccepted.expect(true.B)
      d.io.fenceAccepted.expect(true.B)
      d.clock.step()
      d.io.valid.poke(false.B)
      d.io.completed.expect(true.B)
      d.clock.step()
      for (_ <- 0 until 3) {
        d.io.completed.expect(false.B)
        d.io.parentAccepted.expect(false.B)
        d.io.fenceAccepted.expect(false.B)
        d.io.sync1.expect(0.U)
        d.clock.step()
      }
      d.io.op.poke(FenceOpType.macquire)
      d.io.value.poke(1.U)
      d.io.valid.poke(true.B)
      d.io.ready.expect(true.B)
      d.io.parentAccepted.expect(true.B)
      d.io.fenceAccepted.expect(true.B)
      d.clock.step()
      d.io.valid.poke(false.B)
      for (_ <- 0 until 3) {
        d.io.completed.expect(false.B)
        d.io.sync1.expect(0.U)
        d.clock.step()
      }
      d.io.returnValid.poke(true.B)
      d.io.returnReady.expect(true.B)
      d.clock.step()
      d.io.returnValid.poke(false.B)
      d.io.sync1.expect(1.U)
      var acquired = false
      for (_ <- 0 until 4) {
        acquired ||= d.io.completed.peek().litToBoolean
        d.clock.step()
      }
      assert(acquired, "one real sync1 return must unblock the real Fence acquire")
      d.io.sync1.expect(1.U)
      d.io.completed.expect(false.B)
    }
  }

  // Same parameter binding as BackendInlined before constructing ExuOutput.
  private def bindMinitBackend(p: Parameters): xiangshan.backend.BackendParams = {
    val backend = p(XSCoreParamsKey).backendParams
    backend.allSchdParams.foreach(_.bindBackendParam(backend))
    backend.allIssueParams.foreach(_.bindBackendParam(backend))
    backend.allExuParams.zipWithIndex.foreach { case (exu, index) =>
      exu.bindBackendParam(backend)
      exu.updateIQWakeUpConfigs(backend.iqWakeUpParams)
      exu.updateExuIdx(index)
    }
    backend
  }

  private class MinitCsrProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val valid = Input(Bool())
      val admission = Input(Bool())
      val cancel = Input(Bool())
      val ready = Output(Bool())
      val outValid = Output(Bool())
      val writeMstatus = Output(Bool())
      val msOff = Output(Bool())
    })
    val csr = Module(new CSR(FuConfig.CsrCfg)(p))
    tieInputs(csr.io)
    csr.io.in.valid := io.valid
    csr.io.in.bits.ctrl.fuOpType := CSROpType.minit
    csr.io.out.ready := true.B
    csr.io.csrio.get.minitAdmissionReady := io.admission
    csr.io.flush.valid := io.cancel
    csr.io.flush.bits.level := RedirectLevel.flush
    io.ready := csr.io.in.ready
    io.outValid := csr.io.out.valid
    io.writeMstatus := expose(csr.csrMod.mstatus.w.wen)
    io.msOff := expose(csr.csrMod.io.status.matrixState.off)
  }

  private class MatrixFlagsCsrProbe(val narrow: Boolean = false)(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val csrValid = Input(Bool())
      val csrAddr = Input(UInt(12.W))
      val csrData = Input(UInt(64.W))
      val csrOp = Input(UInt(9.W))
      val bootMstatus = Input(Bool())
      val seedScalarFlags = Input(Bool())
      val csrReady = Output(Bool())
      val csrOutValid = Output(Bool())
      val csrIllegal = Output(Bool())
      val csrReadData = Output(UInt(64.W))
      val mcsrWen = Output(Bool())
      val aliasWen = Output(Bool())
      val mstatusWen = Output(Bool())
      val matrixOff = Output(Bool())
      val scalarOff = Output(Bool())
      val flags = Output(UInt(5.W))
      val mcsr = Output(UInt(64.W))
      val fflags = Output(UInt(5.W))
      val mxrm = Output(UInt(2.W))
      val returnEvent = Flipped(Decoupled(new AmuReleaseIO2XS()(p)))
      val merged = Output(Bool())
      val fenceValid = Input(Bool())
      val fenceOp = Input(UInt(9.W))
      val fenceValue = Input(UInt(64.W))
      val fenceReady = Output(Bool())
      val fenceDone = Output(Bool())
      val msync = Output(UInt(64.W))
    })
    val csr = Module(new CSR(FuConfig.CsrCfg)(p))
    val fence = Module(new Fence(FuConfig.FenceCfg)(p))
    tieInputs(csr.io)
    tieInputs(fence.io)
    csr.io.in.valid := io.csrValid
    csr.io.in.bits.ctrl.fuOpType := (if (narrow) CSROpType.wrt else io.csrOp)
    // Narrow fixture: one real status CSRRW, then matrix-only CSRRW rd=0.
    // No internal register forcing or replacement of legalwen/write mapping.
    val csrAddr = if (narrow) Mux(io.bootMstatus, CSRs.mstatus.U(12.W),
      Cat("h80".U(8.W), io.csrAddr(3, 0))) else io.csrAddr
    csr.io.in.bits.data.src(0) := (if (narrow)
      Mux(io.bootMstatus, ((BigInt(1) << 25) | (BigInt(1) << 13)).U, io.csrData)
      else io.csrData)
    // Same packed IMM_Z used by DecodeUnit (rd,rs1,CSR address).
    csr.io.in.bits.data.imm := Cat((if (narrow) 0 else 1).U(5.W), 1.U(5.W), csrAddr)
    if (narrow) {
      // Real commit endpoint seeds only the scalar sentinel, never MS/FS.
      csr.io.csrio.get.fpu.fflags.valid := io.seedScalarFlags
      csr.io.csrio.get.fpu.fflags.bits := 10.U
    }
    csr.io.out.ready := true.B
    csr.io.csrio.get.minitAdmissionReady := true.B
    fence.io.in.valid := io.fenceValid
    fence.io.in.bits.ctrl.fuOpType := (if (narrow) FenceOpType.macquire else io.fenceOp)
    fence.io.in.bits.data.src(0) := io.fenceValue
    fence.io.in.bits.data.imm := (3 << 6).U // Existing msync3 encoding.
    fence.io.out.ready := true.B
    fence.io.fenceio.get.sbuffer.sbIsEmpty := true.B
    AmuReleaseIO2XS.connectReturn(io.returnEvent, fence.io.fenceio.get.amuRelease.get,
      csr.io.csrio.get.mpu.xmfflags)
    io.csrReady := csr.io.in.ready
    io.csrOutValid := csr.io.out.valid
    io.csrIllegal := csr.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr)
    io.csrReadData := (if (narrow) 0.U else csr.io.out.bits.res.data)
    io.mcsrWen := expose(csr.csrMod.mcsr.w.wen)
    io.aliasWen := expose(csr.csrMod.mcsr.wAliasMflags.wen)
    io.mstatusWen := expose(csr.csrMod.mstatus.w.wen)
    io.matrixOff := expose(csr.csrMod.io.status.matrixState.off)
    io.scalarOff := expose(csr.csrMod.io.status.fpState.off)
    io.flags := expose(csr.csrMod.mcsr.mfflags)
    io.mcsr := expose(csr.csrMod.mcsr.rdata)
    io.fflags := expose(csr.csrMod.fcsr.fflags)
    io.mxrm := expose(csr.csrMod.mcsr.mxrm)
    io.merged := csr.io.csrio.get.mpu.xmfflags.fire
    io.fenceReady := fence.io.in.ready
    io.fenceDone := fence.io.out.valid
    io.msync := expose(fence.msyncRegs)(3)
  }

  it should "DFlags merge at real mcsr write edges with alias backpressure and ordered Fence visibility" in {
    println("[DFlags CPU] elaborating real CSR/NewCSR and Fence")
    test(new MatrixFlagsCsrProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      println("[DFlags CPU] simulator initialized; exercising actual write edges")
      d.io.csrValid.poke(false.B); d.io.csrAddr.poke(CSRs.mstatus.U)
      d.io.csrData.poke(0.U); d.io.csrOp.poke(CSROpType.wrt)
      d.io.bootMstatus.poke(false.B); d.io.seedScalarFlags.poke(false.B)
      d.io.fenceValid.poke(false.B); d.io.fenceOp.poke(FenceOpType.macquire)
      d.io.fenceValue.poke(1.U)
      d.io.returnEvent.valid.poke(false.B)
      d.io.returnEvent.bits.flagsValid.poke(false.B); d.io.returnEvent.bits.flags.poke(0.U)
      d.io.returnEvent.bits.msyncRd.foreach(_.poke(false.B))
      d.clock.step(3)

      def request(addr: Int, data: BigInt, op: UInt = CSROpType.wrt): Unit = {
        println(s"[DFlags CPU] request CSR 0x${addr.toHexString}")
        d.io.csrAddr.poke(addr.U); d.io.csrData.poke(data.U); d.io.csrOp.poke(op)
        d.io.csrValid.poke(true.B); d.io.csrReady.expect(true.B)
        d.clock.step() // R -> R+1: wenLegalReg is now the actual write enable.
        d.io.csrValid.poke(false.B)
      }
      def settle(): Unit = {
        d.clock.step(2)
        d.io.csrReady.expect(true.B)
      }
      def event(flags: Int): Unit = {
        d.io.returnEvent.valid.poke(true.B)
        d.io.returnEvent.bits.flagsValid.poke(true.B)
        d.io.returnEvent.bits.flags.poke(flags.U)
      }
      def expectState(flags: Int): Unit = {
        d.io.flags.expect(flags.U)
        assert(((d.io.mcsr.peek().litValue >> 3) & 31) == flags)
        d.io.fflags.expect(10.U) // Scalar fflags never receives matrix events.
      }
      // Enable existing matrix/scalar CSR permission, seed scalar fflags.
      request(CSRs.mstatus, (BigInt(1) << 25) | (BigInt(1) << 13)); settle()
      request(CSRs.fflags, 10); settle()
      request(CSRs.mfflags, 0); settle()
      expectState(0)
      d.io.fenceValid.poke(true.B); d.io.fenceReady.expect(true.B)
      d.clock.step(); d.io.fenceValid.poke(false.B)
      d.io.fenceDone.expect(false.B) // macquire waits for a future Release.

      for ((addr, software, raised) <- Seq(
          (CSRs.mcsr, BigInt(2) << 3, 0x11),
          (CSRs.mfflags, BigInt(4), 0x02))) {
        request(addr, software)
        d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
        (if (addr == CSRs.mcsr) d.io.mcsrWen else d.io.aliasWen).expect(true.B)
        event(raised)
        d.io.returnEvent.ready.expect(false.B)
        d.io.merged.expect(false.B)
        d.io.fenceDone.expect(false.B); d.io.msync.expect(0.U)
        d.clock.step() // Software write wins by handshake exclusion, not priority.
        val written = if (addr == CSRs.mcsr) (software >> 3).toInt else software.toInt
        expectState(written)
        d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
        d.clock.step() // Held event merges exactly once on this real edge.
        d.io.returnEvent.valid.poke(false.B)
        expectState(written | raised)
        d.clock.step(2); expectState(written | raised)
      }

      // Request-cycle overlap is distinct from the actual R+1 write edge:
      // an unsynchronized later software write may clear earlier flags.
      event(1)
      d.io.csrAddr.poke(CSRs.mfflags.U); d.io.csrData.poke(8.U)
      d.io.csrOp.poke(CSROpType.wrt); d.io.csrValid.poke(true.B)
      d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
      d.clock.step()
      d.io.csrValid.poke(false.B); d.io.returnEvent.valid.poke(false.B)
      d.io.aliasWen.expect(true.B)
      d.clock.step(2); expectState(8)

      // Other aliases update different fields and need not block MFFLAGS.
      request(CSRs.mxrm, 2)
      event(4)
      d.io.returnEvent.ready.expect(true.B)
      d.clock.step(); d.io.returnEvent.valid.poke(false.B)
      d.io.mxrm.expect(2.U); expectState(12)
      settle()
      // Read-only TLENB write is illegal: it must not manufacture a conflict.
      request(CSRs.tlenb, 0)
      d.io.csrIllegal.expect(true.B)
      event(2); d.io.returnEvent.ready.expect(true.B)
      d.clock.step(); d.io.returnEvent.valid.poke(false.B); settle()
      expectState(14)

      // Zero event followed by back-to-back flags; pure Release is then a
      // separate token, so msync cannot precede the older sticky merge.
      for (bits <- Seq(0, 1, 16)) {
        event(bits); d.io.returnEvent.ready.expect(true.B)
        d.clock.step()
      }
      d.io.returnEvent.valid.poke(false.B)
      expectState(31)
      d.io.msync.expect(0.U)
      d.io.fenceDone.expect(false.B)
      request(CSRs.mfflags, 31) // Actual alias write blocks flags, not pure Release.
      d.io.aliasWen.expect(true.B)
      d.io.returnEvent.bits.flagsValid.poke(false.B)
      d.io.returnEvent.bits.msyncRd(3).poke(true.B)
      d.io.returnEvent.valid.poke(true.B)
      d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(false.B)
      d.clock.step()
      d.io.returnEvent.valid.poke(false.B); d.io.returnEvent.bits.msyncRd(3).poke(false.B)
      d.io.msync.expect(1.U); expectState(31)
      // The previously blocked real macquire now observes the Release.
      var waited = 0
      while (!d.io.fenceDone.peek().litToBoolean && waited < 8) { d.clock.step(); waited += 1 }
      d.io.fenceDone.expect(true.B); expectState(31)
      d.clock.step(2)
      request(CSRs.mcsr, 0, CSROpType.set)
      d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
      assert(((d.io.csrReadData.peek().litValue >> 3) & 31) == 31)
      settle()
      request(CSRs.mfflags, 0, CSROpType.set)
      d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
      d.io.csrReadData.expect(31.U); settle()
      request(CSRs.mstatus, BigInt(1) << 13); settle() // MS Off, FS retained.
      d.io.matrixOff.expect(true.B)
      request(CSRs.mstatus, 0, CSROpType.minit)
      d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
      d.io.mstatusWen.expect(true.B)
      settle(); d.io.matrixOff.expect(false.B)
      expectState(31) // MINIT changes scale/status, never sticky matrix flags.
    }
  }

  it should "DFlags narrow CSRRW matrix actual-wen backpressure and ordered real Fence" in {
    println("[DFlags narrow] real CSR/NewCSR+Fence elaboration; no TLENB/CSRRS/RC/MINIT")
    test(new MatrixFlagsCsrProbe(narrow = true)(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.csrValid.poke(false.B); d.io.csrAddr.poke(CSRs.mcsr.U)
      d.io.csrData.poke(0.U); d.io.csrOp.poke(CSROpType.wrt)
      d.io.bootMstatus.poke(true.B); d.io.seedScalarFlags.poke(false.B)
      d.io.returnEvent.valid.poke(false.B)
      d.io.returnEvent.bits.flagsValid.poke(true.B); d.io.returnEvent.bits.flags.poke(0.U)
      d.io.returnEvent.bits.msyncRd.foreach(_.poke(false.B))
      d.io.fenceValid.poke(false.B); d.io.fenceOp.poke(FenceOpType.macquire)
      d.io.fenceValue.poke(1.U)
      println("[DFlags narrow] simulator initialized")
      d.clock.step()
      d.io.matrixOff.expect(true.B); d.io.scalarOff.expect(true.B)
      d.io.csrValid.poke(true.B); d.io.csrReady.expect(true.B)
      d.clock.step()
      d.io.csrValid.poke(false.B)
      d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
      d.io.mstatusWen.expect(true.B)
      d.io.seedScalarFlags.poke(true.B)
      d.clock.step() // The one real mstatus write enables MS/FS here.
      d.io.matrixOff.expect(false.B); d.io.scalarOff.expect(false.B)
      d.io.bootMstatus.poke(false.B); d.io.seedScalarFlags.poke(false.B)
      d.clock.step() // Real delayed fflags commit now reaches the scalar Reg.
      d.io.fflags.expect(10.U); d.io.flags.expect(0.U)
      println("[DFlags narrow] legal mstatus boot and real scalar sentinel established")

      def state(flags: Int): Unit = {
        d.io.flags.expect(flags.U)
        assert(((d.io.mcsr.peek().litValue >> 3) & 31) == flags)
        d.io.fflags.expect(10.U)
      }
      def request(addr: Int, data: BigInt): Unit = {
        require((addr & 0xff0) == 0x800)
        d.io.csrAddr.poke(addr.U); d.io.csrData.poke(data.U)
        d.io.csrValid.poke(true.B); d.io.csrReady.expect(true.B)
        d.clock.step()
        d.io.csrValid.poke(false.B)
        d.io.csrOutValid.expect(true.B); d.io.csrIllegal.expect(false.B)
      }
      // Start a real macquire before either flags or the Release is visible.
      d.io.fenceValid.poke(true.B); d.io.fenceReady.expect(true.B)
      d.clock.step(); d.io.fenceValid.poke(false.B)
      d.io.fenceDone.expect(false.B)
      for ((addr, software, raised) <- Seq(
          (CSRs.mcsr, BigInt(2) << 3, 0x11),
          (CSRs.mfflags, BigInt(4), 2))) {
        request(addr, software)
        (if (addr == CSRs.mcsr) d.io.mcsrWen else d.io.aliasWen).expect(true.B)
        d.io.returnEvent.valid.poke(true.B)
        d.io.returnEvent.bits.flags.poke(raised.U)
        d.io.returnEvent.ready.expect(false.B); d.io.merged.expect(false.B)
        d.io.msync.expect(0.U); d.io.fenceDone.expect(false.B)
        d.clock.step() // R+1 actual software write excludes flags fire.
        val written = if (addr == CSRs.mcsr) (software >> 3).toInt else software.toInt
        state(written)
        d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
        d.clock.step() // The held event merges on this edge, exactly once.
        d.io.returnEvent.valid.poke(false.B)
        state(written | raised)
        println(s"[DFlags narrow] actual-wen conflict+held merge PASS addr=0x${addr.toHexString}")
      }
      // A request at R need not block an older merge; the real R+1 software
      // write is allowed to clear it when software has not synchronized.
      d.io.returnEvent.valid.poke(true.B); d.io.returnEvent.bits.flags.poke(1.U)
      d.io.csrAddr.poke(CSRs.mfflags.U); d.io.csrData.poke(8.U)
      d.io.csrValid.poke(true.B)
      d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
      d.clock.step()
      d.io.csrValid.poke(false.B); d.io.returnEvent.valid.poke(false.B)
      d.io.aliasWen.expect(true.B)
      d.clock.step(); state(8)
      request(CSRs.mxrm, 2)
      d.io.returnEvent.valid.poke(true.B); d.io.returnEvent.bits.flags.poke(4.U)
      d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
      d.clock.step()
      d.io.returnEvent.valid.poke(false.B)
      d.io.mxrm.expect(2.U); state(12)
      // Zero event and consecutive events exercise the production atomic fork.
      for (bits <- Seq(0, 1, 16)) {
        d.io.returnEvent.valid.poke(true.B); d.io.returnEvent.bits.flags.poke(bits.U)
        d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(true.B)
        d.clock.step()
      }
      d.io.returnEvent.valid.poke(false.B)
      state(29); d.io.msync.expect(0.U); d.io.fenceDone.expect(false.B)
      println("[DFlags narrow] unrelated alias, zero/back-to-back flags and FCSR isolation PASS")
      request(CSRs.mfflags, 29)
      d.io.aliasWen.expect(true.B)
      d.io.returnEvent.bits.flagsValid.poke(false.B)
      d.io.returnEvent.bits.msyncRd(3).poke(true.B)
      d.io.returnEvent.valid.poke(true.B)
      // CSR busy only blocks flags-bearing events; pure Release still fires.
      d.io.returnEvent.ready.expect(true.B); d.io.merged.expect(false.B)
      d.clock.step()
      d.io.returnEvent.valid.poke(false.B); d.io.returnEvent.bits.msyncRd(3).poke(false.B)
      d.io.msync.expect(1.U); state(29)
      var waited = 0
      while (!d.io.fenceDone.peek().litToBoolean && waited < 4) { d.clock.step(); waited += 1 }
      d.io.fenceDone.expect(true.B); state(29)
      d.clock.step()
      d.io.msync.expect(1.U); d.io.merged.expect(false.B); state(29)
      println("[DFlags narrow] real Fence Release/msync/macquire ordering PASS")
    }
  }

  private class MinitAmuProbe(implicit p: Parameters) extends Module {
    private implicit val backend: xiangshan.backend.BackendParams = bindMinitBackend(p)
    val io = IO(new Bundle {
      val enq = Input(Bool())
      val needAMU = Input(Bool())
      val wb = Input(Bool())
      val commit = Input(Bool())
      val accept = Input(Bool())
      val olderPending = Output(Bool())
      val deliver = Output(Bool())
      val traceRetireId = Input(UInt(64.W))
      val traceEpoch = Input(UInt(64.W))
      val slot = Input(UInt(4.W)) // minitParams fixes the real ROB buffer to 16 entries.
      val deliveredRetireId = Output(UInt(64.W))
      val deliveredEpoch = Output(UInt(64.W))
      val deliveredPc = Output(UInt(64.W))
    })
    val amu = Module(new AmuCtrlBuffer()(p, backend))
    tieInputs(amu.io)
    amu.io.enq(0).valid := io.enq
    amu.io.enq(0).reqValid := io.enq
    amu.io.enq(0).needAMU := io.needAMU
    amu.io.enq(0).allocPtr.value := io.slot
    amu.io.enq(0).pc.foreach(_ := "h80001000".U)
    val wb = amu.io.wb.find(_.bits.amuCtrl.nonEmpty).get
    wb.valid := io.wb
    wb.bits.robIdx.value := io.slot
    wb.bits.amuCtrl.get.data := 1.U
    amu.io.deqCommit(0).valid := io.commit
    amu.io.deqCommit(0).ptr.value := io.slot
    amu.io.deqCommit(0).retireId.foreach(_ := io.traceRetireId)
    amu.io.deqCommit(0).epoch.foreach(_ := io.traceEpoch)
    amu.io.toAMU.foreach(_.ready := io.accept)
    io.olderPending := amu.io.olderPending
    io.deliver := amu.io.toAMU.map(_.fire).reduce(_ || _)
    io.deliveredRetireId := amu.io.toAMU.head.bits.retireId.getOrElse(0.U)
    io.deliveredEpoch := amu.io.toAMU.head.bits.epoch.getOrElse(0.U)
    io.deliveredPc := amu.io.toAMU.head.bits.pc.getOrElse(0.U)
  }

  private class MinitRobProbe(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val enq = Input(Bool())
      val young = Input(Bool())
      val blockCommit = Input(Bool())
      val cancel = Input(Bool())
      val illegal = Input(Bool())
      val ack = Input(Bool())
      val canEnq = Output(Bool())
      val clear = Output(Bool())
      val commit = Output(Bool())
      val olderPending = Output(Bool())
      val enqIndex = Output(UInt(5.W))
      val exception = Output(Bool())
      val flushOut = Output(Bool())
      val walking = Output(Bool())
      val awaitAck = Output(Bool())
    })
    private val backend = bindMinitBackend(p)
    val rob = Module(LazyModule(new Rob(backend)(p)).module)
    tieInputs(rob.io)
    rob.io.trace.blockCommit := io.blockCommit
    rob.io.minitClearAck.get := io.ack
    rob.io.enq.req(0).valid := io.enq || io.young
    rob.io.enq.needAlloc(0) := io.enq || io.young
    val uop = rob.io.enq.req(0).bits
    uop.robIdx := rob.io.enq.resp(0)
    uop.fuType := Mux(io.young, xiangshan.backend.fu.FuType.mma.U, xiangshan.backend.fu.FuType.csr.U)
    uop.fuOpType := CSROpType.minit
    uop.blockBackward := !io.young
    uop.waitForward := !io.young
    uop.firstUop := true.B
    uop.lastUop := true.B
    uop.numUops := 1.U
    uop.numWB := Mux(io.young, 1.U, 0.U)
    uop.instrSize := 1.U
    uop.needAmuCtrl.get := io.young
    uop.exceptionVec(ExceptionNO.illegalInstr) := io.illegal
    uop.hasException := io.illegal
    rob.io.redirect.valid := io.cancel
    rob.io.redirect.bits.level := RedirectLevel.flush
    rob.io.redirect.bits.robIdx := rob.io.robDeqPtr
    io.canEnq := rob.io.enq.canAccept
    io.clear := rob.io.minitClear.get
    io.commit := rob.io.commits.isCommit && rob.io.commits.commitValid.asUInt.orR
    io.olderPending := rob.io.olderAmuPending.get
    io.enqIndex := rob.io.enq.resp(0).value
    io.exception := rob.io.exception.valid
    io.flushOut := rob.io.flushOut.valid
    io.walking := rob.io.commits.isWalk
    io.awaitAck := expose(rob.minitAwaitAck)
  }

  private val mx = MatrixIsaParams(
    enableInt8Int32 = true,
    enableFp8Fp32 = true,
    enableMxfp8Fp32 = true,
    enableMxfp4Fp32 = true
  )

  private def instruction(func4: Int, store: Boolean = false, reserved: Int = 0): BigInt =
    (BigInt(func4) << 28) | (BigInt(reserved) << 26) |
      (if (store) BigInt(1) << 25 else BigInt(0)) | BigInt(0x102b)

  private def matches(pattern: chisel3.util.BitPat, inst: BigInt): Boolean =
    (inst & pattern.mask) == pattern.value

  private def conversionParams(enabled: Boolean, lanes: Int = 8, trlen: Int = 512,
                               dynamic: Boolean = false): Parameters = {
    val rows = if (dynamic) 128 else 64
    val core = config(XSCoreParamsKey).copy(MatrixExtension = mx, TLEN = rows * trlen, TRLEN = trlen)
    val cute = CuteParams.CUTE_2Tops.copy(MatrixExtension = mx,
      Tensor_MN = rows, ExperimentalMxfpConvertLoad = enabled, MxfpConvertLanes = lanes,
      ExperimentalMxfpDynamicQuant = dynamic)
    config.alterPartial {
      case XSCoreParamsKey => core
      case CuteParamsKey => cute
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false, enablePerf = false, fpgaPlatform = false, enableXMR = false)
    }
  }

  it should "R27 lint converting-load masks against the full CPU static superset and Rocket constants" in {
    def overlaps(a: BitPat, b: BitPat): Boolean =
      ((a.value ^ b.value) & a.mask & b.mask) == 0
    val supplied = Seq(MatrixDecoder.MLACVT_F32_MX, MatrixDecoder.MLBCVT_F32_MX)
    val dynamic = Seq(MatrixDecoder.MLADYN_F32_MX, MatrixDecoder.MLBDYN_F32_MX)
    val candidates = supplied ++ dynamic
    // Literal candidate contract, not expected bits generated by the decoder.
    for ((pattern, value) <- dynamic.zip(Seq(BigInt("3800102b", 16), BigInt("b800102b", 16)))) {
      assert(pattern.value == value && pattern.mask == BigInt("fe007e7f", 16))
      for (rd <- 0 until 32; rs1 <- Seq(0, 9, 31); rs2 <- Seq(0, 10, 31)) {
        val inst = value | (BigInt(rd) << 7) | (BigInt(rs1) << 15) | (BigInt(rs2) << 20)
        assert(matches(pattern, inst) == (rd < 4))
      }
    }
    // Mirror all DecodeUnit table owners, deliberately using the matrix static
    // superset here rather than the default-off effective matrix table.
    val cpuStatic = (XDecode.table ++ FpDecode.table ++ BitmanipDecode.table ++
      ScalarCryptoDecode.table ++ XSDebugDecode.table ++ CBODecode.table ++
      SvinvalDecode.table ++ HypervisorDecode.table ++ VecDecoder.table(hasVfexp2 = true) ++
      Mxfp4VectorDecode.table(true) ++
      MatrixDecoder.decodeArray.map { case (bits, dec) => bits -> dec.generate() } ++
      ZicondDecode.table ++ ZimopDecode.table ++ ZfaDecode.table ++ ZfbfDecode.table).map(_._1)
    for ((candidate, index) <- candidates.zipWithIndex) {
      assert(cpuStatic.count(p => p.mask == candidate.mask && p.value == candidate.value) == 1)
      for (other <- cpuStatic if other.mask != candidate.mask || other.value != candidate.value) {
        assert(!overlaps(candidate, other),
          s"conversion candidate $index overlaps CPU pattern $other")
      }
    }
    for ((a, index) <- candidates.zipWithIndex; b <- candidates.drop(index + 1)) {
      assert(!overlaps(a, b))
    }
    // All public zero-argument BitPat constants, not a hand-picked list of
    // exact strings; this also catches future broader Rocket encodings.
    val rocketPatterns = Seq(freechips.rocketchip.rocket.Instructions,
      freechips.rocketchip.rocket.CustomInstructions).flatMap { owner =>
      val patterns = owner.getClass.getMethods.toSeq.filter(m =>
        m.getParameterCount == 0 && m.getReturnType == classOf[BitPat]).map(m =>
        s"${owner.getClass.getSimpleName}.${m.getName}" -> m.invoke(owner).asInstanceOf[BitPat])
      assert(patterns.nonEmpty)
      patterns
    }
    for (candidate <- candidates; (name, pattern) <- rocketPatterns) {
      assert(!overlaps(candidate, pattern), s"conversion candidate overlaps Rocket $name")
    }
    assert(!MatrixDecoder.convertF32Enabled(Parameters.empty))
    assert(!MatrixDecoder.dynamicQuantEnabled(Parameters.empty))
    for (candidate <- supplied) {
      assert(!MatrixDecoder.table(mx).exists { case (p, _) => overlaps(candidate, p) })
      assert(MatrixDecoder.table(mx, experimentalMxfpConvertLoad = true)
        .count { case (p, _) => overlaps(candidate, p) } == 1)
    }
    for (candidate <- dynamic; convert <- Seq(false, true); enabled <- Seq(false, true)) {
      val table = MatrixDecoder.table(mx, experimentalMxfpConvertLoad = convert,
        experimentalMxfpDynamicQuant = enabled)
      assert(table.count { case (p, _) => overlaps(candidate, p) } == (if (convert && enabled) 1 else 0))
    }
  }

  it should "R27 keep old configs off and explicitly opt in the same ABI for 8 and 32 lanes" in {
    assert(!MatrixDecoder.convertF32Enabled(new top.DefaultMatrix2T64MxfpConfig))
    for (params <- Seq(new top.DefaultMatrix2TMxfpConfig,
        new top.DefaultMatrix2TMxfpVectorEncodeConfig,
        new top.DefaultMatrix2T64MxfpConfig,
        new top.DefaultMatrix2T64MxfpConvert8Config,
        new top.DefaultMatrix2T64MxfpConvert32Config)) {
      assert(!MatrixDecoder.dynamicQuantEnabled(params))
    }
    for ((params, lanes) <- Seq(
        (new top.DefaultMatrix2T64MxfpConvert8Config, 8),
        (new top.DefaultMatrix2T64MxfpConvert32Config, 32))) {
      assert(MatrixDecoder.convertF32Enabled(params))
      assert(params(CuteParamsKey).MxfpConvertLanes == lanes)
      assert(params(CuteParamsKey).Tensor_MN == 64)
      assert(params(xiangshan.XSTileKey).head.TLEN == 64 * 64 * 8)
    }
    val baseline = new top.DefaultMatrix2TMxfpConfig
    val dynamic = new top.DefaultMatrix2TMxfpDynamicQuantConfig
    assert(MatrixDecoder.convertF32Enabled(dynamic) && MatrixDecoder.dynamicQuantEnabled(dynamic))
    assert(dynamic(CuteParamsKey).ExperimentalMxfpDynamicQuant)
    assert(dynamic(CuteParamsKey).MxfpConvertLanes == 8)
    assert(dynamic(CuteParamsKey).MxfpConvertOwners == 4)
    assert(dynamic(CuteParamsKey).Tensor_MN == 128 && dynamic(CuteParamsKey).Tensor_K == 64)
    assert(dynamic(CuteParamsKey).Matrix_MN == baseline(CuteParamsKey).Matrix_MN)
    val core = dynamic(xiangshan.XSTileKey).head
    val oldCore = baseline(xiangshan.XSTileKey).head
    assert(core.TLEN == 128 * 64 * 8 && core.TRLEN == 64 * 8)
    assert(core.TLEN == oldCore.TLEN && core.TRLEN == oldCore.TRLEN)
    assert(core.MELEN == oldCore.MELEN && core.MatrixExtension == oldCore.MatrixExtension)
    assert(core.ExperimentalMxfp4VectorEncode == oldCore.ExperimentalMxfp4VectorEncode)
  }

  it should "R27 gate real decode and retain normal A MK and B KN register metadata" in {
    for ((enabled, dynamic) <- Seq((false, false), (true, false), (true, true))) {
      val params = conversionParams(enabled, dynamic = dynamic)
      val core = params(XSCoreParamsKey)
      test(new DecodeUnit()(params)).withAnnotations(testAnnos) { dut =>
        dut.io.enq.mcfg.entries(2).raw.poke(0x16.U)
        for ((pattern, op, src2, src3, allowed) <- Seq(
            (MatrixDecoder.MLACVT_F32_MX, MldstOpType.mlacvtF32Mx, core.Mtilem_IDX, core.Mtilek_IDX, enabled),
            (MatrixDecoder.MLBCVT_F32_MX, MldstOpType.mlbcvtF32Mx, core.Mtilek_IDX, core.Mtilen_IDX, enabled),
            (MatrixDecoder.MLADYN_F32_MX, MldstOpType.mladynF32Mx, core.Mtilem_IDX, core.Mtilek_IDX, dynamic),
            (MatrixDecoder.MLBDYN_F32_MX, MldstOpType.mlbdynF32Mx, core.Mtilek_IDX, core.Mtilen_IDX, dynamic),
            (MLA, MldstOpType.mla, core.Mtilem_IDX, core.Mtilek_IDX, true),
            (MLB, MldstOpType.mlb, core.Mtilek_IDX, core.Mtilen_IDX, true))) {
          val inst = pattern.value | (BigInt(9) << 15) | (BigInt(10) << 20) | (BigInt(2) << 7)
          dut.io.enq.ctrlFlow.instr.poke(inst.U)
          dut.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr).expect((!allowed).B)
          if (allowed) {
            dut.io.deq.decodedInst.fuOpType.expect(op)
            dut.io.deq.decodedInst.needAmuCtrl.expect(true.B)
            dut.io.deq.decodedInst.lsrc(0).expect(9.U)
            dut.io.deq.decodedInst.lsrc(1).expect(10.U)
            dut.io.deq.decodedInst.lsrc(2).expect(src2.U)
            dut.io.deq.decodedInst.lsrc(3).expect(src3.U)
            dut.io.deq.decodedInst.mcfgReadView.get.sel.expect(2.U)
            dut.io.deq.decodedInst.mcfgReadView.get.entry.raw.expect(0x16.U)
            dut.io.deq.decodedInst.wfflags.expect(false.B)
            dut.io.deq.decodedInst.needFrm.scalaNeedFrm.expect(false.B)
            dut.io.deq.decodedInst.needFrm.vectorNeedFrm.expect(false.B)
          }
        }
        for (pattern <- Seq(MatrixDecoder.MLADYN_F32_MX, MatrixDecoder.MLBDYN_F32_MX);
             rd <- 0 until 32) {
          val raw = BigInt("1234567800000016", 16)
          dut.io.enq.mcfg.entries(rd & 7).raw.poke(raw.U)
          dut.io.enq.ctrlFlow.instr.poke((pattern.value | (BigInt(rd) << 7)).U)
          dut.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr).expect((!dynamic || rd >= 4).B)
          if (dynamic && rd < 4) {
            dut.io.deq.decodedInst.mcfgReadView.get.sel.expect(rd.U)
            dut.io.deq.decodedInst.mcfgReadView.get.entry.raw.expect(raw.U)
          }
        }
      }
    }
  }

  it should "R27 leave a non-matrix CPU with no CUTE key on its normal illegal-decode path" in {
    assert(config.lift(CuteParamsKey).isEmpty)
    assert(!config(XSCoreParamsKey).HasMatrixExtension)
    assert(!MatrixDecoder.convertF32Enabled(config))
    assert(!MatrixDecoder.dynamicQuantEnabled(config))
    // Neither a missing CUTE key nor a CUTE-only opt-in on a non-matrix core
    // may install matrix instructions in the effective CPU decode table.
    val withCute = config.alterPartial {
      case CuteParamsKey => CuteParams.CUTE_2Tops.copy(MatrixExtension = mx,
        ExperimentalMxfpConvertLoad = true, ExperimentalMxfpDynamicQuant = true)
    }
    for (params <- Seq(config, withCute)) {
      test(new DecodeUnit()(params)).withAnnotations(testAnnos) { dut =>
        for (pattern <- Seq(MatrixDecoder.MLACVT_F32_MX, MatrixDecoder.MLBCVT_F32_MX,
            MatrixDecoder.MLADYN_F32_MX, MatrixDecoder.MLBDYN_F32_MX)) {
          dut.io.enq.ctrlFlow.instr.poke(pattern.value.U)
          dut.io.deq.decodedInst.exceptionVec(ExceptionNO.illegalInstr).expect(true.B)
        }
      }
    }
  }

  it should "R27 emit F32 source geometry and MX destination mode through real MLS with static rejection" in {
    test(new MlsUnit()(conversionParams(enabled = true, dynamic = true))).withAnnotations(testAnnos) { dut =>
      dut.io.redirect.valid.poke(false.B); dut.io.replay.valid.poke(false.B)
      dut.io.lsin.valid.poke(false.B); dut.io.lsout.ready.poke(true.B)
      dut.io.lsq.lsin.ready.poke(true.B); dut.io.mlsq_rep_full.poke(false.B)
      dut.io.tlb.req.ready.poke(true.B); dut.io.tlb.resp.valid.poke(true.B)
      dut.io.tlb.resp.bits.miss.poke(false.B)
      dut.io.tlb.resp.bits.paddr(0).poke(0x5000.U)
      dut.io.pmp.ld.poke(false.B); dut.io.pmp.st.poke(false.B)
      dut.io.pmp.mmio.poke(false.B); dut.io.pmp.atomic.poke(false.B)
      dut.io.lsin.bits.uop.imm.poke(2.U)
      dut.clock.step(3)
      def issue(op: UInt, raw: Int, dim0: Int, dim1: Int, stride: Int = 256,
                base: Int = 0x4000, illegal: Boolean = false, misaligned: Boolean = false,
                sourceSize: Int = 2, reg: Int = 2): BigInt = {
        dut.io.lsin.bits.src(0).poke(base.U); dut.io.lsin.bits.src(1).poke(stride.U)
        dut.io.lsin.bits.src(2).poke(dim0.U); dut.io.lsin.bits.src(3).poke(dim1.U)
        dut.io.lsin.bits.uop.fuOpType.poke(op)
        dut.io.lsin.bits.uop.imm.poke(reg.U)
        dut.io.lsin.bits.uop.mcfgReadView.get.entry.raw.poke(raw.U)
        dut.io.lsin.valid.poke(true.B)
        dut.io.lsin.ready.expect(true.B)
        dut.io.tlb.req.bits.size.expect(sourceSize.U)
        dut.clock.step(); dut.io.lsin.valid.poke(false.B)
        // A later decode/config value must not replace this uop's snapshot.
        dut.io.lsin.bits.uop.mcfgReadView.get.entry.raw.poke(12.U)
        var waited = 0
        while (!dut.io.lsout.valid.peek().litToBoolean && waited < 8) {
          dut.clock.step(); waited += 1
        }
        dut.io.lsout.valid.expect(true.B)
        dut.io.lsout.bits.uop.exceptionVec(ExceptionNO.illegalInstr).expect(illegal.B)
        dut.io.lsout.bits.uop.exceptionVec(ExceptionNO.loadAddrMisaligned).expect(misaligned.B)
        dut.io.lsout.bits.uop.mcfgReadView.get.entry.raw.expect(raw.U)
        val payload = dut.io.lsout.bits.amuCtrl.get.data.peek().litValue
        assert(payload.bitLength <= 150)
        dut.clock.step(2)
        payload
      }
      for ((raw, mode) <- Seq(0x16 -> 10, 0x17 -> 8, 0x18 -> 7);
           rows <- Seq(1, 64); sideA <- Seq(true, false)) {
        val dim0 = if (sideA) rows else 64
        val dim1 = if (sideA) 64 else rows
        val payload = issue(if (sideA) MldstOpType.mlacvtF32Mx else MldstOpType.mlbcvtF32Mx,
          raw, dim0, dim1, stride = 260)
        assert((payload >> 132) == 0)
        assert(((payload >> 131) & 1) == 1 && ((payload >> 126) & 1) == 0)
        assert(((payload >> 127) & 15) == mode)
        assert(((payload >> 122) & 15) == 2)
        assert(((payload >> 118) & 1) == (if (sideA) 1 else 0))
        assert(((payload >> 117) & 1) == (if (sideA) 0 else 1))
        assert(((payload >> 69) & ((BigInt(1) << 48) - 1)) == 0x5000)
        assert(((payload >> 21) & ((BigInt(1) << 48) - 1)) == 260)
        assert(((payload >> 12) & 511) == dim0 && ((payload >> 3) & 511) == dim1)
        assert((payload & 7) == 2) // source e32, never packed e4/e8.
      }
      for (sideA <- Seq(true, false); stride <- Seq(256, 260); base <- Seq(0x4000, 0x4004)) {
        val dim0 = if (sideA) 8 else 64
        val dim1 = if (sideA) 64 else 8
        val dynOp = if (sideA) MldstOpType.mladynF32Mx else MldstOpType.mlbdynF32Mx
        val oldOp = if (sideA) MldstOpType.mlacvtF32Mx else MldstOpType.mlbcvtF32Mx
        val generated = issue(dynOp, 0x16, dim0, dim1, stride = stride, base = base)
        val supplied = issue(oldOp, 0x16, dim0, dim1, stride = stride, base = base)
        assert((generated >> 133) == 0)
        assert(((generated >> 131) & 3) == 3 && ((generated >> 126) & 1) == 0)
        assert(((generated >> 127) & 15) == 10)
        assert(((generated >> 122) & 15) == 2)
        assert(((generated >> 118) & 1) == (if (sideA) 1 else 0))
        assert(((generated >> 117) & 1) == (if (sideA) 0 else 1))
        assert(((generated >> 12) & 511) == dim0 && ((generated >> 3) & 511) == dim1)
        assert((generated & 7) == 2)
        // Same full source geometry, mode, destination and translated address;
        // only bit132 may distinguish dynamic from supplied-scale conversion.
        assert((generated ^ supplied) == (BigInt(1) << 132))
      }
      for (sideA <- Seq(true, false)) {
        val op = if (sideA) MldstOpType.mladynF32Mx else MldstOpType.mlbdynF32Mx
        for ((rows, k) <- Seq((0, 64), (1, 64), (7, 64), (9, 64), (64, 64),
            (128, 64), (8, 0), (8, 32), (8, 63), (8, 65), (8, 128))) {
          issue(op, 0x16, if (sideA) rows else k, if (sideA) k else rows, illegal = true)
        }
        val dim0 = if (sideA) 8 else 64
        val dim1 = if (sideA) 64 else 8
        for (raw <- Seq(0x17, 0x18, 6, 7, 8, 12, 0x15, 0x26)) {
          issue(op, raw, dim0, dim1, illegal = true)
        }
        for (stride <- Seq(0, 32, 64, 128, 252, 257, 258, 259)) {
          issue(op, 0x16, dim0, dim1, stride = stride, illegal = true)
        }
        for (base <- Seq(0x4001, 0x4002, 0x4003)) {
          issue(op, 0x16, dim0, dim1, base = base, misaligned = true)
        }
        for (reg <- 4 until 8) issue(op, 0x16, dim0, dim1, reg = reg, illegal = true)
        for (bit <- Seq(8, 7, 6, 2)) {
          issue((op.litValue | (BigInt(1) << bit)).U, 0x16, dim0, dim1, illegal = true)
        }
      }
      for (bad <- Seq(BigInt(3) | (BigInt(1) << 5), // C
          BigInt(3) | (BigInt(3) << 3))) { // two sides
        issue(bad.U, 0x16, 8, 64, illegal = true)
      }
      for ((dim0, dim1) <- Seq((0, 64), (65, 64), (1, 0), (1, 32), (1, 65))) {
        issue(MldstOpType.mlacvtF32Mx, 0x16, dim0, dim1, illegal = true)
        issue(MldstOpType.mlbcvtF32Mx, 0x16, dim1, dim0, illegal = true)
      }
      for (raw <- Seq(6, 7, 8, 12, 0x15, 0x26)) {
        issue(MldstOpType.mlacvtF32Mx, raw, 1, 64, illegal = true)
      }
      for (stride <- Seq(32, 64, 252, 257)) {
        issue(MldstOpType.mlacvtF32Mx, 0x16, 1, 64, stride = stride, illegal = true)
      }
      issue(MldstOpType.mlacvtF32Mx, 0x16, 1, 64, base = 0x4002, misaligned = true)
      // Direct malformed internal tags must also fail closed, not become
      // an ordinary load after commit.
      for (bad <- Seq(
          MldstOpType.mlacvtF32Mx.litValue | (BigInt(1) << 8), // store
          MldstOpType.mlacvtF32Mx.litValue | (BigInt(1) << 7), // scale
          MldstOpType.mlacvtF32Mx.litValue | (BigInt(1) << 6), // whole
          MldstOpType.mlacvtF32Mx.litValue | (BigInt(1) << 2), // transpose
          BigInt(2) | (BigInt(1) << 5), // C
          BigInt(2) | (BigInt(3) << 3))) { // two sides
        issue(bad.U, 0x16, 1, 64, illegal = true)
      }
      val ordinary = issue(MldstOpType.mla, 0x16, 1, 128, stride = 64, sourceSize = 0)
      assert((ordinary >> 126) == 0 && (ordinary & 7) == 7)
    }
  }

  it should "R27 reject injected conversion tags when off and respect packed TR capacity and MX capability" in {
    val off = conversionParams(enabled = false)
    val short = conversionParams(enabled = true, trlen = 128)
    val half = conversionParams(enabled = true, trlen = 256)
    val disabledCore = conversionParams(enabled = true)
    val noMx = disabledCore.alterPartial {
      case XSCoreParamsKey => disabledCore(XSCoreParamsKey).copy(
        MatrixExtension = MatrixIsaParams(enableInt8Int32 = true))
    }
    val dynamic = conversionParams(enabled = true, dynamic = true)
    val dynamicShort = conversionParams(enabled = true, trlen = 128, dynamic = true)
    val dynamicHalf = conversionParams(enabled = true, trlen = 256, dynamic = true)
    val dynamicNoMx = dynamic.alterPartial {
      case XSCoreParamsKey => dynamic(XSCoreParamsKey).copy(
        MatrixExtension = MatrixIsaParams(enableInt8Int32 = true))
    }
    // Literal expected rejection for supplied and dynamic; enabling the old
    // converting load alone must never implicitly enable generated scales.
    for ((params, raw, suppliedIllegal, dynamicIllegal) <- Seq(
        (off, 0x16, true, true), (short, 0x16, true, true), (short, 0x18, true, true),
        (half, 0x16, false, true), (half, 0x18, true, true), (noMx, 0x16, true, true),
        (dynamic, 0x16, false, false), (dynamic, 0x18, false, true),
        (dynamicShort, 0x16, true, true), (dynamicHalf, 0x16, false, false),
        (dynamicNoMx, 0x16, true, true))) {
      test(new MlsUnit()(params)).withAnnotations(testAnnos) { dut =>
        dut.io.redirect.valid.poke(false.B); dut.io.replay.valid.poke(false.B)
        dut.io.lsout.ready.poke(true.B); dut.io.lsq.lsin.ready.poke(true.B)
        dut.io.tlb.req.ready.poke(true.B); dut.io.tlb.resp.valid.poke(true.B)
        dut.io.tlb.resp.bits.miss.poke(false.B)
        dut.io.tlb.resp.bits.paddr(0).poke(0x5000.U)
        dut.io.lsin.bits.src(0).poke(0x4000.U); dut.io.lsin.bits.src(1).poke(256.U)
        dut.io.lsin.bits.uop.imm.poke(2.U)
        dut.io.lsin.valid.poke(false.B)
        dut.clock.step(3)
        for ((op, rows, illegal) <- Seq(
            (MldstOpType.mlacvtF32Mx, 1, suppliedIllegal),
            (MldstOpType.mladynF32Mx, 8, dynamicIllegal))) {
          dut.io.lsin.bits.src(2).poke(rows.U); dut.io.lsin.bits.src(3).poke(64.U)
          dut.io.lsin.bits.uop.fuOpType.poke(op)
          dut.io.lsin.bits.uop.mcfgReadView.get.entry.raw.poke(raw.U)
          dut.io.lsin.valid.poke(true.B)
          dut.io.lsin.ready.expect(true.B); dut.io.tlb.req.bits.size.expect(2.U)
          dut.clock.step(); dut.io.lsin.valid.poke(false.B)
          var waited = 0
          while (!dut.io.lsout.valid.peek().litToBoolean && waited < 8) {
            dut.clock.step(); waited += 1
          }
          dut.io.lsout.valid.expect(true.B)
          dut.io.lsout.bits.uop.exceptionVec(ExceptionNO.illegalInstr).expect(illegal.B)
          dut.clock.step(2)
        }
      }
    }
  }

  it should "MINIT remain a noSpec blockBack CSR without an AMU control token" in {
    test(new DecodeUnit()(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.enq.ctrlFlow.instr.poke(freechips.rocketchip.rocket.Instructions.MINIT.value.U)
      d.io.deq.decodedInst.needAmuCtrl.expect(false.B)
      d.io.deq.decodedInst.waitForward.expect(true.B)
      d.io.deq.decodedInst.blockBackward.expect(true.B)
      d.io.deq.decodedInst.fuOpType.expect(CSROpType.minit)
    }
  }

  it should "MINIT gate NewCSR valid writes and ready on drain or same-cycle redirect" in {
    test(new MinitCsrProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.valid.poke(false.B); d.io.admission.poke(false.B); d.io.cancel.poke(false.B)
      d.clock.step(3)
      d.io.msOff.expect(true.B)
      d.io.valid.poke(true.B)
      for (_ <- 0 until 4) {
        d.io.ready.expect(false.B); d.io.writeMstatus.expect(false.B)
        d.io.outValid.expect(false.B); d.clock.step()
      }
      d.io.admission.poke(true.B); d.io.cancel.poke(true.B)
      for (_ <- 0 until 3) {
        d.io.ready.expect(false.B); d.io.writeMstatus.expect(false.B)
        d.clock.step(); d.io.msOff.expect(true.B)
      }
      d.io.cancel.poke(false.B); d.io.ready.expect(true.B)
      d.clock.step(); d.io.valid.poke(false.B)
      var writes = 0
      for (_ <- 0 until 8) {
        if (d.io.writeMstatus.peek().litToBoolean) writes += 1
        d.clock.step()
      }
      assert(writes == 1, s"MINIT wrote mstatus $writes times")
      d.io.msOff.expect(false.B)
    }
  }

  it should "MINIT olderPending include accepted AMU through postcommit delivery fire" in {
    test(new MinitAmuProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.enq.poke(false.B); d.io.needAMU.poke(true.B); d.io.wb.poke(false.B)
      d.io.slot.poke(0.U)
      d.io.commit.poke(false.B); d.io.accept.poke(false.B); d.clock.step(2)
      d.io.olderPending.expect(false.B)
      d.io.enq.poke(true.B); d.io.olderPending.expect(true.B)
      d.clock.step(); d.io.enq.poke(false.B)
      d.io.wb.poke(true.B); d.clock.step(); d.io.wb.poke(false.B)
      d.io.commit.poke(true.B); d.clock.step(); d.io.commit.poke(false.B)
      d.clock.step(3); d.io.olderPending.expect(true.B)
      d.io.accept.poke(true.B); d.io.deliver.expect(true.B)
      d.io.olderPending.expect(true.B)
      d.clock.step(3); d.io.olderPending.expect(false.B)
    }
  }

  it should "retain committed AMU trace identity through delivery backpressure without CCT" in {
    val traceParams = minitParams.alterPartial {
      case DebugOptionsKey => minitParams(DebugOptionsKey).copy(EnableDifftest = true)
    }
    test(new MinitAmuProbe()(traceParams)).withAnnotations(testAnnos) { d =>
      d.io.enq.poke(false.B); d.io.needAMU.poke(true.B)
      d.io.wb.poke(false.B); d.io.commit.poke(false.B); d.io.accept.poke(false.B)
      d.io.slot.poke(0.U)
      d.io.traceRetireId.poke(0.U); d.io.traceEpoch.poke(0.U)
      d.clock.step(2)
      d.io.enq.poke(true.B); d.clock.step(); d.io.enq.poke(false.B)
      d.io.wb.poke(true.B); d.clock.step(); d.io.wb.poke(false.B)
      d.io.traceRetireId.poke(123.U); d.io.traceEpoch.poke(7.U)
      d.io.commit.poke(true.B); d.clock.step(); d.io.commit.poke(false.B)
      // Subsequent ROB cycles must not change the entry's captured owner.
      d.io.traceRetireId.poke(999.U); d.io.traceEpoch.poke(8.U)
      for (_ <- 0 until 4) {
        d.io.deliveredRetireId.expect(123.U)
        d.io.deliveredEpoch.expect(7.U)
        d.io.deliveredPc.expect("h80001000".U)
        d.io.deliver.expect(false.B)
        d.clock.step()
      }
      d.io.accept.poke(true.B)
      d.io.deliver.expect(true.B)
      d.io.deliveredRetireId.expect(123.U); d.io.deliveredEpoch.expect(7.U)
      d.clock.step(3); d.io.olderPending.expect(false.B)
    }
  }

  it should "distinguish same-PC AMU trace identities across buffer slot reuse" in {
    val traceParams = minitParams.alterPartial {
      case DebugOptionsKey => minitParams(DebugOptionsKey).copy(EnableDifftest = true)
    }
    test(new MinitAmuProbe()(traceParams)).withAnnotations(testAnnos) { d =>
      d.io.enq.poke(false.B); d.io.needAMU.poke(true.B)
      d.io.wb.poke(false.B); d.io.commit.poke(false.B); d.io.accept.poke(false.B)
      d.io.slot.poke(0.U); d.io.traceRetireId.poke(0.U); d.io.traceEpoch.poke(0.U)
      d.clock.step(2)
      // More than one complete ring; epoch values are supplied by the commit
      // producer here. This tests entry ownership, not MINIT epoch generation.
      for (ordinal <- 0 until 20) {
        val id = 200 + ordinal
        val epoch = 7 + ordinal / 16
        d.io.slot.poke((ordinal % 16).U)
        d.io.accept.poke(false.B)
        d.io.enq.poke(true.B); d.clock.step(); d.io.enq.poke(false.B)
        d.io.wb.poke(true.B); d.clock.step(); d.io.wb.poke(false.B)
        d.io.traceRetireId.poke(id.U); d.io.traceEpoch.poke(epoch.U)
        d.io.commit.poke(true.B); d.clock.step(); d.io.commit.poke(false.B)
        d.io.traceRetireId.poke((id + 1000).U); d.io.traceEpoch.poke((epoch + 1).U)
        for (_ <- 0 until 2) {
          d.io.deliver.expect(false.B); d.io.olderPending.expect(true.B)
          d.io.deliveredPc.expect("h80001000".U)
          d.io.deliveredRetireId.expect(id.U); d.io.deliveredEpoch.expect(epoch.U)
          d.clock.step()
        }
        d.io.accept.poke(true.B); d.io.deliver.expect(true.B)
        d.io.deliveredRetireId.expect(id.U); d.io.deliveredEpoch.expect(epoch.U)
        d.clock.step(3); d.io.olderPending.expect(false.B)
      }
    }
  }

  it should "MINIT real ROB commit clear once then hold younger enqueue until registered ack" in {
    test(new MinitRobProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      d.io.enq.poke(false.B); d.io.young.poke(false.B)
      d.io.blockCommit.poke(true.B); d.io.cancel.poke(false.B)
      d.io.illegal.poke(false.B); d.io.ack.poke(false.B); d.clock.step(3)
      d.io.canEnq.expect(true.B)
      d.io.enq.poke(true.B); d.clock.step(); d.io.enq.poke(false.B)
      d.clock.step(4)
      d.io.clear.expect(false.B); d.io.canEnq.expect(false.B)
      d.io.olderPending.expect(false.B) // MINIT must not self-drain-deadlock.
      val nextIndex = d.io.enqIndex.peek().litValue
      d.io.young.poke(true.B)
      d.clock.step(2)
      d.io.enqIndex.expect(nextIndex.U)
      d.io.olderPending.expect(false.B) // No young AMU accepted while blocked.
      d.io.blockCommit.poke(false.B)
      var waited = 0
      while (!d.io.clear.peek().litToBoolean && waited < 12) {
        d.clock.step(); waited += 1
      }
      d.io.clear.expect(true.B); d.io.commit.expect(true.B)
      d.clock.step()
      d.io.clear.expect(false.B); d.io.canEnq.expect(false.B)
      d.clock.step(2); d.io.canEnq.expect(false.B)
      d.io.enqIndex.expect(nextIndex.U)
      d.io.ack.poke(true.B); d.io.canEnq.expect(true.B)
      d.clock.step(); d.io.ack.poke(false.B); d.io.young.poke(false.B)
      d.io.canEnq.expect(true.B); d.io.clear.expect(false.B)
      d.io.enqIndex.expect((nextIndex + 1).U)
      d.io.olderPending.expect(true.B) // Exactly the accepted young MMA.
    }
  }

  it should "MINIT suppress ROB clear and ack on redirect then illegal cancellation after reset" in {
    test(new MinitRobProbe()(minitParams)).withAnnotations(testAnnos) { d =>
      // Reuse one real ROB elaboration; only the unit-test reset separates
      // independent cancellation stimuli. MINIT itself never resets the ROB.
      for (illegal <- Seq(false, true)) {
        d.io.enq.poke(false.B); d.io.young.poke(false.B); d.io.blockCommit.poke(true.B)
        d.io.cancel.poke(false.B); d.io.illegal.poke(illegal.B); d.io.ack.poke(false.B)
        d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); d.clock.step(3)
        d.io.canEnq.expect(true.B); d.io.awaitAck.expect(false.B)
        d.io.olderPending.expect(false.B); d.io.enqIndex.expect(0.U)
        d.io.enq.poke(true.B); d.clock.step(); d.io.enq.poke(false.B)
        var sawException = d.io.exception.peek().litToBoolean
        var sawFlush = d.io.flushOut.peek().litToBoolean
        var sawWalk = d.io.walking.peek().litToBoolean
        for (_ <- 0 until 3) {
          d.io.clear.expect(false.B); d.io.awaitAck.expect(false.B)
          d.io.canEnq.expect(false.B)
          d.clock.step()
          sawException ||= d.io.exception.peek().litToBoolean
          sawFlush ||= d.io.flushOut.peek().litToBoolean
        }
        if (!illegal) { d.io.cancel.poke(true.B); d.clock.step(); d.io.cancel.poke(false.B) }
        d.io.blockCommit.poke(false.B)
        for (_ <- 0 until 12) {
          d.io.clear.expect(false.B); d.io.awaitAck.expect(false.B)
          d.io.olderPending.expect(false.B)
          if (illegal) d.io.canEnq.expect(false.B)
          sawException ||= d.io.exception.peek().litToBoolean
          sawFlush ||= d.io.flushOut.peek().litToBoolean
          sawWalk ||= d.io.walking.peek().litToBoolean
          d.clock.step()
        }
        if (illegal) assert(sawException && sawFlush, "illegal MINIT must take the real ROB exception/flush path")
        else assert(sawWalk, "redirected MINIT must take the real ROB walk path")
        // The external ack remains low; awaitAck must not be entered without
        // commit-clear. Redirect recovery is the existing flush/walk path,
        // not a fabricated completion or an ack-based young unblock.
        println(s"MINIT ${if (illegal) "illegal" else "redirect"} cancellation PASS")
      }
    }
  }

  it should "decode only the two legal kind11 scale loads in the canonical table" in {
    val table = MatrixDecoder.table(mx)
    for (func4 <- Seq(3, 7, 11, 15); store <- Seq(false, true); reserved <- 0 until 4) {
      val inst = instruction(func4, store, reserved)
      val decoded = table.filter { case (pattern, _) => matches(pattern, inst) }
      val legal = (func4 == 3 || func4 == 11) && !store && reserved == 0
      assert(decoded.nonEmpty == legal, s"func4=$func4 store=$store reserved=$reserved")
    }
    assert(matches(MLA, instruction(0)))
    assert(matches(MLB, instruction(1)))
    assert(matches(MatrixDecoder.MLASCALE, instruction(3)))
    assert(matches(MatrixDecoder.MLBSCALE, instruction(11)))
  }

  it should "keep table0 unscaled and require matching table1 MX operands with table0 FP32 C" in {
    for (t <- Seq(6, 7, 8)) {
      assert(McfgHelpers.decode(t, 1).legal)
      assert(McfgHelpers.isMmaTripleSupportedRaw(0x10 | t, 0x10 | t, 12, mx))
      assert(!McfgHelpers.isMmaTripleSupportedRaw(0x10 | t, t, 12, mx))
      assert(!McfgHelpers.isMmaTripleSupportedRaw(0x10 | t, 0x10 | t, 0x1c, mx))
      assert(!McfgHelpers.isMmaTripleSupportedRaw(0x10 | t, 0x10 | t, 12, MatrixIsaParams()))
    }
    assert(McfgHelpers.isMmaTripleSupportedRaw(7, 7, 12, mx))
    assert(McfgHelpers.isMmaTripleSupportedRaw(8, 8, 12, mx))
    assert(!McfgHelpers.isMmaTripleSupportedRaw(0x17, 0x18, 12, mx))
    assert(!McfgHelpers.decode(5, 1).legal)
    for (disabled <- Seq(
      MatrixIsaParams(enableFp8Fp32 = true),
      MatrixIsaParams(enableMxfp4Fp32 = true),
      MatrixIsaParams(enableMxfp4Fp32 = true, enableMxfp8Fp32 = true)
    )) {
      for (t <- Seq(6, 7, 8)) {
        val expected = (t == 6 && disabled.enableMxfp4Fp32) ||
          ((t == 7 || t == 8) && disabled.enableMxfp8Fp32)
        assert(McfgHelpers.isMmaTripleSupportedRaw(0x10 | t, 0x10 | t, 12, disabled) == expected,
          s"table1 type=$t disabled=$disabled")
      }
    }
    assert(!McfgHelpers.decode(7, 2).legal)
    for (a <- 0 until 16; b <- 0 until 16; c <- 0 until 16) {
      val expected = Seq(a, b, c).map(t => McfgHelpers.decode(t, 0).semanticType) match {
        case Seq(Some(at), Some(bt), Some(ct)) => McfgHelpers.isMmaTripleSupported(at, bt, ct, mx)
        case _ => false
      }
      assert(McfgHelpers.isMmaTripleSupportedRaw(a, b, c, mx) == expected)
    }
  }

  it should "read M or N as scale rows and K as scale columns from the canonical DecodeUnit" in {
    val matrixConfig = config.alterPartial {
      case XSCoreParamsKey => config(XSCoreParamsKey).copy(MatrixExtension = mx)
    }
    val core = matrixConfig(XSCoreParamsKey)
    test(new DecodeUnit()(matrixConfig)).withAnnotations(testAnnos) { dut =>
      dut.io.enq.mcfg.entries(2).raw.poke(0x18.U)
      for ((func4, rowSource) <- Seq(3 -> core.Mtilem_IDX, 11 -> core.Mtilen_IDX)) {
        dut.io.enq.ctrlFlow.instr.poke((instruction(func4) | (BigInt(2) << 7)).U)
        dut.io.deq.decodedInst.lsrc(2).expect(rowSource.U)
        dut.io.deq.decodedInst.lsrc(3).expect(core.Mtilek_IDX.U)
        dut.io.deq.decodedInst.mcfgReadView.get.sel.expect(2.U)
        dut.io.deq.decodedInst.mcfgReadView.get.entry.raw.expect(0x18.U)
      }
    }
  }

  it should "emit scale rows and ceil K over 32 columns through the existing MLS pipeline" in {
    val matrixConfig = config.alterPartial {
      case XSCoreParamsKey => config(XSCoreParamsKey).copy(MatrixExtension = mx)
      case LogUtilsOptionsKey => LogUtilsOptions(
        enableDebug = false, enablePerf = false, fpgaPlatform = false, enableXMR = false
      )
    }
    test(new MlsUnit()(matrixConfig)).withAnnotations(testAnnos) { dut =>
      dut.io.redirect.valid.poke(false.B)
      dut.io.replay.valid.poke(false.B)
      dut.io.lsout.ready.poke(true.B)
      dut.io.lsq.lsin.ready.poke(true.B)
      dut.io.tlb.req.ready.poke(true.B)
      dut.io.tlb.resp.valid.poke(true.B)
      dut.io.tlb.resp.bits.miss.poke(false.B)
      dut.io.tlb.resp.bits.paddr(0).poke(0x5000.U)
      dut.io.lsin.bits.src(0).poke(0x4000.U)
      dut.io.lsin.bits.src(1).poke(2.U)
      dut.io.lsin.bits.src(3).poke(33.U)
      dut.io.lsin.bits.uop.imm.poke(2.U)
      dut.io.lsin.bits.uop.mcfgReadView.get.entry.raw.poke(0x18.U)
      for ((op, rows) <- Seq(MldstOpType.mlascale -> 3, MldstOpType.mlbscale -> 65)) {
        dut.io.lsin.bits.src(2).poke(rows.U)
        dut.io.lsin.bits.uop.fuOpType.poke(op)
        dut.io.lsin.valid.poke(true.B)
        dut.io.tlb.req.bits.size.expect(0.U)
        dut.clock.step()
        dut.io.lsin.valid.poke(false.B)
        var waited = 0
        while (!dut.io.lsout.valid.peek().litToBoolean && waited < 8) {
          dut.clock.step()
          waited += 1
        }
        dut.io.lsout.valid.expect(true.B)
        dut.io.lsout.bits.uop.exceptionVec(ExceptionNO.illegalInstr).expect(false.B)
        val payload = dut.io.lsout.bits.amuCtrl.get.data.peek().litValue
        assert(((payload >> 127) & 15) == 7)
        assert(((payload >> 126) & 1) == 1)
        assert(((payload >> 122) & 15) == 2)
        assert(((payload >> 69) & ((BigInt(1) << 48) - 1)) == 0x5000)
        assert(((payload >> 21) & ((BigInt(1) << 48) - 1)) == 2)
        assert(((payload >> 12) & 511) == rows)
        assert(((payload >> 3) & 511) == 2)
        assert((payload & 7) == 0)
        dut.clock.step(2)
      }
    }
  }

  it should "emit scaled MMA metadata only for matching legal MX triples in the existing wrapper" in {
    val matrixConfig = config.alterPartial {
      case XSCoreParamsKey => config(XSCoreParamsKey).copy(MatrixExtension = mx)
    }
    test(new Mma(FuConfig.MmaCfg)(matrixConfig)).withAnnotations(testAnnos) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)
      dut.io.mfrm.get.poke(0.U)
      dut.io.mxrm.get.poke(0.U)
      dut.io.in.bits.data.imm.poke((4 | (1 << 3) | (2 << 6)).U)
      dut.io.in.bits.data.src(2).poke(3.U)
      dut.io.in.bits.data.src(3).poke(5.U)
      dut.io.in.bits.data.src(4).poke(33.U)
      dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(2).poke(12.U)
      for ((raw, computeType) <- Seq(0x18 -> 7, 0x17 -> 8, 0x16 -> 10, 8 -> 0, 7 -> 0)) {
        dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(0).poke(raw.U)
        dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(1).poke(raw.U)
        dut.io.out.valid.expect(true.B)
        dut.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr).expect(false.B)
        val payload = dut.io.out.bits.ctrl.amuCtrl.get.data.peek().litValue
        assert((payload >> 57) == 0)
        assert(((payload >> 53) & 15) == computeType)
        assert(((payload >> 46) & 15) == 4)
      }
      dut.io.mfrm.get.poke(1.U)
      for (raw <- Seq(0x18, 0x17, 0x16)) {
        dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(0).poke(raw.U)
        dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(1).poke(raw.U)
        dut.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr).expect(true.B)
      }
      dut.io.mfrm.get.poke(0.U)
      dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(0).poke(0x17.U)
      dut.io.in.bits.ctrl.mmaMcfgReadRaw.get(1).poke(0x18.U)
      dut.io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr).expect(true.B)
      assert(((dut.io.out.bits.ctrl.amuCtrl.get.data.peek().litValue >> 53) & 15) == 0)
    }
  }

  private class MetadataTop(implicit p: Parameters) extends Module {
    val io = IO(new Bundle {
      val a = Input(UInt(64.W))
      val b = Input(UInt(64.W))
      val c = Input(UInt(64.W))
      val op = Input(UInt(9.W))
      val legal = Output(Bool())
      val load = Output(Bool())
      val sideA = Output(Bool())
      val sideB = Output(Bool())
      val lsu = Output(UInt(150.W))
      val mma = Output(UInt(150.W))
    })
    val entries = Seq(io.a, io.b, io.c).map { raw =>
      val entry = Wire(new McfgEntry()(p))
      entry.raw := raw
      entry
    }
    io.legal := McfgHelpers.isMmaEntriesSupported(entries(0), entries(1), entries(2), mx)
    io.load := MldstOpType.isLoad(io.op)
    io.sideA := MldstOpType.isMatrixA(io.op)
    io.sideB := MldstOpType.isMatrixB(io.op)
    val lsu = WireInit(0.U.asTypeOf(new AmuLsuIO()(p)))
    require(lsu.getWidth == 150)
    lsu.isScale := MldstOpType.isScale(io.op)
    lsu.computeType := Mux(lsu.isScale,
      McfgHelpers.mxComputeType(entries.head.typeCode, entries.head.tableSel), 0.U)
    lsu.ms := 3.U
    lsu.isA := io.sideA
    lsu.isB := io.sideB
    lsu.baseAddr := "h123456789abc".U
    lsu.stride := 64.U
    lsu.row := 16.U
    lsu.column := 4.U
    val mma = WireInit(0.U.asTypeOf(new AmuMmaIO()(p)))
    require(mma.getWidth == 150)
    mma.computeType := McfgHelpers.mmaComputeType(entries(0), entries(1), entries(2), mx)
    mma.md := 4.U
    mma.mtilem := 16.U
    mma.mtilen := 16.U
    mma.mtilek := 128.U
    io.lsu := lsu.asUInt
    io.mma := mma.asUInt
  }

  it should "pack canonical four-bit compute metadata without shifting the existing AMU fields" in {
    test(new MetadataTop).withAnnotations(testAnnos) { dut =>
      dut.io.c.poke(12.U)
      val lowMask = (BigInt(1) << 126) - 1
      for ((raw, computeType) <- Seq(0x18 -> 7, 0x17 -> 8, 0x16 -> 10, 8 -> 0, 7 -> 0)) {
        dut.io.a.poke(raw.U)
        dut.io.b.poke(raw.U)
        dut.io.op.poke(MldstOpType.mlascale)
        val lsu = dut.io.lsu.peek().litValue
        assert((lsu >> 131) == 0)
        assert(((lsu >> 127) & 15) == computeType)
        assert(((lsu >> 126) & 1) == 1)
        assert(((lsu >> 122) & 15) == 3)
        assert(((lsu >> 69) & ((BigInt(1) << 48) - 1)) == BigInt("123456789abc", 16))
        assert(((lsu >> 21) & ((BigInt(1) << 48) - 1)) == 64)
        assert(((lsu >> 12) & 511) == 16)
        assert(((lsu >> 3) & 511) == 4)
        val mma = dut.io.mma.peek().litValue
        assert((mma >> 57) == 0)
        assert(((mma >> 53) & 15) == computeType)
        assert(((mma >> 46) & 15) == 4)
        dut.io.op.poke(MldstOpType.mla)
        assert((dut.io.lsu.peek().litValue >> 126) == 0)
        assert((dut.io.lsu.peek().litValue & lowMask) == (lsu & lowMask))
        dut.io.load.expect(true.B)
        dut.io.sideA.expect(true.B)
        dut.io.sideB.expect(false.B)
        dut.io.op.poke(MldstOpType.mlbscale)
        dut.io.load.expect(true.B)
        dut.io.sideA.expect(false.B)
        dut.io.sideB.expect(true.B)
      }
      dut.io.a.poke(0x17.U)
      dut.io.b.poke(0x18.U)
      dut.io.legal.expect(false.B)
      assert(((dut.io.mma.peek().litValue >> 53) & 15) == 0)
      dut.io.b.poke(0x17.U)
      dut.io.c.poke(0x1c.U)
      dut.io.legal.expect(false.B)
      assert(((dut.io.mma.peek().litValue >> 53) & 15) == 0)
    }
  }
}
