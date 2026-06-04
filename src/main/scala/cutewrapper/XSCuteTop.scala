package cute

import chisel3._
import chisel3.util._
import difftest._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import coupledL2.MatrixDataBundle
import utility.{ChiselDB, TLLoggerM}
import xiangshan.HasXSParameter
import coupledL2.{AmeIndexKey,AmeIndexField}

/**
 * A simple wrapper demonstration that integrates CUTEV2Top and Cute2TL,
 * facilitating their use for independent unit testing.
 */

// Import CUTEImplParameters to access ABMatrixRegNBanks
trait XSCuteParameters extends CUTEImplParameters

class XSCuteTestTopImpl(wrapper: XSCuteTestTop) extends LazyModuleImp(wrapper) {
  val cute = Module(new CUTEV2Top)
  val nBanks = wrapper.cuteParams.ABMatrixRegNBanks
  val io = IO(new CUTETopIO {
    val matrix_data_in = Vec(nBanks, Flipped(Decoupled(chiselTypeOf(wrapper.cute_tl.module.io.matrix_data_in(0).bits))))
  })
  io.ctrl2top <> cute.io.ctrl2top
  io.perf <> cute.io.perf
  wrapper.cute_tl.module.io.matrix_data_in <> io.matrix_data_in
  wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc
  io.mmu2llc := DontCare
  val tls = wrapper.node.zipWithIndex.map{ case(x, i) => x.makeIOs()(ValName(s"tl$i")) }
  val edgeIns = wrapper.node.map(_.edges.in(0))
}

// Add a LazyModule with TLAdapterNode between TLWidthWidget and cute_tl.node

class CuteDebugAdapter(label: String)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode() // identity adapter
  lazy val module = new LazyModuleImp(this) {
    // ChiselDB Bundle definitions for TileLink monitoring
    class TLAEventEntry extends Bundle {
      val opcode = UInt(3.W)
      val param = UInt(3.W)
      val size = UInt(8.W)
      val source = UInt(32.W)
      val address = UInt(64.W)
      val mask = UInt(64.W)
      val data = UInt(512.W)
    }

    class TLDEventEntry extends Bundle {
      val opcode = UInt(3.W)
      val param = UInt(3.W)
      val size = UInt(8.W)
      val source = UInt(32.W)
      val sink = UInt(32.W)
      val denied = Bool()
      val corrupt = Bool()
      val data = UInt(512.W)
    }

    // Create ChiselDB tables
    val tlAEventTable = ChiselDB.createTable("TLAEvent", new TLAEventEntry, basicDB = true)
    val tlDEventTable = ChiselDB.createTable("TLDEvent", new TLDEventEntry, basicDB = true) 

    (node.in zip node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in

      // Log TL-A channel events with ChiselDB
      val entry_a = Wire(new TLAEventEntry)
      entry_a.opcode := in.a.bits.opcode
      entry_a.param := in.a.bits.param
      entry_a.size := in.a.bits.size
      entry_a.source := in.a.bits.source
      entry_a.address := in.a.bits.address
      entry_a.mask := in.a.bits.mask
      entry_a.data := in.a.bits.data
      tlAEventTable.log(
        data = entry_a,
        en = in.a.fire,
        site = s"TLA_$label",
        clock = clock,
        reset = reset
      )
      
      // Log TL-D channel events with ChiselDB
      val entry_d = Wire(new TLDEventEntry)
      entry_d.opcode := in.d.bits.opcode
      entry_d.param := in.d.bits.param
      entry_d.size := in.d.bits.size
      entry_d.source := in.d.bits.source
      entry_d.sink := in.d.bits.sink
      entry_d.denied := in.d.bits.denied
      entry_d.corrupt := in.d.bits.corrupt
      entry_d.data := in.d.bits.data
      tlDEventTable.log(
        data = entry_d,
        en = in.d.fire,
        site = s"TLD_$label",
        clock = clock,
        reset = reset
      )
    }
  }
}

object CuteDebugAdapter {
  def apply(label: String)(implicit p: Parameters): TLAdapterNode = {
    val adapter = LazyModule(new CuteDebugAdapter(label))
    adapter.node
  }
}

class XSCuteTestTop(implicit p: Parameters) extends LazyModule with XSCuteParameters {
  val beatBytes = 32
  val transferBytes = 64
  val cute_tl = LazyModule(new Cute2TL)
  val node = Seq.fill(8)(TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, 0xffffffffL)),
      supportsGet = TransferSizes(1, transferBytes),
      supportsPutPartial = TransferSizes(1, transferBytes),
      supportsPutFull = TransferSizes(1, transferBytes),
      fifoId = Some(0)
    )),
    requestKeys = Seq(AmeIndexKey),
    responseFields = Seq(AmeIndexField()),
    beatBytes = beatBytes))))

  (node zip cute_tl.node).zipWithIndex foreach { case ((down, up), i) =>
    if (beatBytes == 64) {
      down := CuteDebugAdapter(s"near_cute_$i") := up
    }
    else {
      down := CuteDebugAdapter(s"near_manager_$i") := TLFragmenter(32, 64) := TLWidthWidget(64) := CuteDebugAdapter(s"near_cute_$i") := up
    }
  }

  lazy val module = new XSCuteTestTopImpl(this)
}

class XSCuteIO(implicit p: Parameters) extends CuteBundle {
  val cute = new CUTETopIO
  val matrix_data_in = Flipped(Vec(ABMatrixRegNBanks, Decoupled(new MatrixDataBundle())))
  val hartId = Input(UInt(8.W))
}

class XSCuteImp(wrapper: XSCute)(implicit p: Parameters) extends LazyModuleImp(wrapper) 
  with HasXSParameter 
  with CUTEImplParameters
{
    wrapper.node.zipWithIndex.foreach { case (node, i) =>
      (node.in zip node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
        out <> in
      }
    }

    val io = IO(new XSCuteIO())
    val cute = Module(new CUTEV2Top)
    io.cute <> cute.io
    io.cute.mmu2llc := DontCare
    wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc

    val enableTLLog = !env.FPGAPlatform && env.AlwaysBasicDB
    val matrix_data_ch0 = wrapper.cute_tl.module.io.matrix_data_in(0)
    val logm = Module(new TLLoggerM(s"L2_CUTE_${coreParams.HartId}", wrapper.node.head.out.head._2, enableTLLog))
    logm.io.a.valid := wrapper.node.head.out.head._1.a.valid
    logm.io.a.bits := wrapper.node.head.out.head._1.a.bits
    logm.io.m.valid := matrix_data_ch0.valid
    logm.io.m.bits.source := matrix_data_ch0.bits.source
    logm.io.m.bits.data := matrix_data_ch0.bits.data

    // Connect 8-channel matrix_data_in (currently only channel 0 is used in CUTE2TLImp)
    for (i <- 0 until ABMatrixRegNBanks) {
      val matrix_data_in = wrapper.cute_tl.module.io.matrix_data_in(i)
      matrix_data_in.valid := io.matrix_data_in(i).valid
      matrix_data_in.bits.source := io.matrix_data_in(i).bits.sourceId
      matrix_data_in.bits.data := io.matrix_data_in(i).bits.data.data
      io.matrix_data_in(i).ready := matrix_data_in.ready
    }

  // DiffTest: Monitor CUTE write requests to L2 Cache
  if (env.EnableDifftest) {

    val mmu = wrapper.cute_tl.module.io.mmu
    // Use wrapper.node.in(0) instead of wrapper.cute_tl.node.out(0)
    // because wrapper.node is connected to cute_tl.node and is visible in this module
    val (tl_in, _) = wrapper.node(0).in(0)
    
    // Record write request information when it's sent
    val writeReqTable = Reg(Vec(LLCSourceMaxNum, new Bundle {
      val addr = UInt(64.W)
      val data = Vec(64, UInt(8.W))
      val mask = UInt(64.W)
      val valid = Bool()
    }))
    dontTouch(writeReqTable)
    
    // Clear valid when response comes back
    when(tl_in.d.fire && tl_in.d.bits.opcode === TLMessages.AccessAck) {
      writeReqTable(tl_in.d.bits.source).valid := false.B
    }
    
    // Record write request when it fires
    when(mmu.Request(0).fire && mmu.Request(0).bits.RequestType_isWrite) {
      val sourceId = mmu.ConherentRequsetSourceID.bits
      writeReqTable(sourceId).addr := mmu.Request(0).bits.RequestAddr
      writeReqTable(sourceId).data := mmu.Request(0).bits.RequestData.asTypeOf(Vec(64, UInt(8.W)))
      writeReqTable(sourceId).mask := mmu.Request(0).bits.RequestMask
      writeReqTable(sourceId).valid := true.B
    }
    
    // Trigger DiffTest event when write response comes back
    val difftest = DifftestModule(new DiffMatrixStoreEvent, delay = 1)
    difftest.coreid := io.hartId
    difftest.index  := 0.U
    difftest.valid  := tl_in.d.fire && writeReqTable(tl_in.d.bits.source).valid &&
                       (tl_in.d.bits.opcode === TLMessages.AccessAck || tl_in.d.bits.opcode === TLMessages.ReleaseAck)
                        
    difftest.addr   := writeReqTable(tl_in.d.bits.source).addr
    difftest.data   := writeReqTable(tl_in.d.bits.source).data
    difftest.mask   := writeReqTable(tl_in.d.bits.source).mask
  }
}

class XSCute(implicit p: Parameters) extends LazyModule with XSCuteParameters {
  val cute_tl = LazyModule(new Cute2TL)
  val node = List.fill(ABMatrixRegNBanks)(TLAdapterNode())

  for (i <- 0 until ABMatrixRegNBanks) {
    node(i) :=* TLWidthWidget(64) := CuteDebugAdapter(s"near_cute_channel$i") := cute_tl.node(i)
  }

  lazy val module = new XSCuteImp(this)
}


trait CuteConsts {
  // Constant definitions (corresponding to definitions in cuteMarcoinstHelper.h)
  val TaskTypeTensorLoad = 3
  val TaskTypeTensorZeroLoad = 1
  val TaskTypeTensorRepeatRowLoad = 2
  
  val Tensor_M_Element_Length = 64
  val Tensor_N_Element_Length = 64
  val Tensor_K_Element_Length = 64
  
  // Function code definitions
  val CUTE_CONFIG_FUNCTOPS = 64
  val CUTE_ISSUE_MARCO_INST = CUTE_CONFIG_FUNCTOPS + 0
  val CUTE_ATENSOR_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 1
  val CUTE_BTENSOR_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 2
  val CUTE_CTENSOR_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 3
  val CUTE_DTENSOR_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 4
  val CUTE_MNK_KERNALSTRIDE_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 5
  val CUTE_CONV_CONFIG_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 6
  val CUTE_FIFO_DEQUEUE_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 16
  val CUTE_FIFO_GET_FINISH_TAIL_FIFOINDEX_FUNCTOPS = CUTE_CONFIG_FUNCTOPS + 17
  
  // Search function code definitions
  val CUTE_SEARCH_FUNCTOPS = 0
  val CUTE_IS_RUNNING_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 1
  val CUTE_RUNNING_CYCLYES_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 2
  val CUTE_MRMORY_LOAD_REQUEST_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 3
  val CUTE_MRMORY_STORE_REQUEST_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 4
  val CUTE_COMPUTE_CYCLYES_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 5
  val CUTE_FIFO_FINISH_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 6
  val CUTE_FIFO_FULL_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 7
  val CUTE_FIFO_VALID_SEARCH_FUNCTOPS = CUTE_SEARCH_FUNCTOPS + 8
}
