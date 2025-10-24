package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import coupledL2.MatrixDataBundle
import utility.ChiselDB


/**
 * A simple wrapper demonstration that integrates CUTEV2Top and Cute2TL,
 * facilitating their use for independent unit testing.
 */

class XSCuteTopImpl(wrapper: XSCuteTop) extends LazyModuleImp(wrapper) {
  val cute = Module(new CUTEV2Top)
  val io = IO(new CUTETopIO {
    val matrix_data_in = wrapper.cute_tl.module.io.matrix_data_in.cloneType
  })
  io.ctrl2top <> cute.io.ctrl2top
  io.mrelease <> cute.io.mrelease
  io.instfifo_head_id <> cute.io.instfifo_head_id
  io.instfifo_tail_id <> cute.io.instfifo_tail_id
  io.instfifo_release <> cute.io.instfifo_release
  wrapper.cute_tl.module.io.matrix_data_in <> io.matrix_data_in
  wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc
  io.mmu2llc := DontCare
  val tl = wrapper.node.makeIOs()(ValName("tl"))
  val edgeIn = wrapper.node.edges.in(0)
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

class XSCuteTop(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val transferBytes = 64
  val cute_tl = LazyModule(new Cute2TL)
  val node = TLManagerNode(Seq(TLSlavePortParameters.v1(
    managers = Seq(TLSlaveParameters.v1(
      address = Seq(AddressSet(0, 0xffffffffL)),
      supportsGet = TransferSizes(1, transferBytes),
      supportsPutPartial = TransferSizes(1, transferBytes),
      supportsPutFull = TransferSizes(1, transferBytes),
      fifoId = Some(0)
    )),
    beatBytes = beatBytes))
  )

  if (beatBytes == 64) {
    node := CuteDebugAdapter("near_cute") := cute_tl.node
  }
  else {
    node := CuteDebugAdapter("near_manager") := TLFragmenter(32, 64) := TLWidthWidget(64) := CuteDebugAdapter("near_cute") := cute_tl.node
  }
  lazy val module = new XSCuteTopImpl(this)
}

class XSCuteIO(implicit p: Parameters) extends Bundle {
  val cute = new CUTETopIO
  val matrix_data_in = Flipped(Decoupled(new MatrixDataBundle()))
}

class XSCuteImp(wrapper: XSCute) extends LazyModuleImp(wrapper) {
    (wrapper.node.in zip wrapper.node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }

    val io = IO(new XSCuteIO())
    val cute = Module(new CUTEV2Top)
    io.cute <> cute.io
    io.cute.mmu2llc := DontCare
    wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc

    val tl_data_in = wrapper.cute_tl.module.io.matrix_data_in
    tl_data_in.valid := io.matrix_data_in.valid
    tl_data_in.bits := 0.U.asTypeOf(tl_data_in.bits)
    tl_data_in.bits.opcode := TLMessages.AccessAckData
    tl_data_in.bits.source := io.matrix_data_in.bits.sourceId
    tl_data_in.bits.data := io.matrix_data_in.bits.data.data
    io.matrix_data_in.ready := tl_data_in.ready
}

class XSCute(implicit p: Parameters) extends LazyModule {
  val cute_tl = LazyModule(new Cute2TL)
  val node = TLAdapterNode()

  node := CuteDebugAdapter("near_cute") := cute_tl.node

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
