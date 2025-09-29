package cute

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._


/**
 * A simple wrapper demonstration that integrates CUTEV2Top and Cute2TL,
 * facilitating their use for independent unit testing.
 */

class XSCuteTopImpl(wrapper: XSCuteTop) extends LazyModuleImp(wrapper) {
  val io = IO(new CUTETopIO)
  val cute = Module(new CUTEV2Top)
  io <> cute.io
  wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc
  io.mmu2llc := DontCare
  val tl = wrapper.node.makeIOs()(ValName("tl"))
  val edgeIn = wrapper.node.edges.in(0)
}

// Add a LazyModule with TLAdapterNode between TLWidthWidget and cute_tl.node

class CuteDebugAdapter(label: String)(implicit p: Parameters) extends LazyModule {
  val node = TLAdapterNode() // identity adapter
  lazy val module = new LazyModuleImp(this) {
    (node.in zip node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
      // Print TL-A channel info when fired
      when (in.a.fire) {
        printf(cf"[CuteDebugAdapter:$label] TL-A fired: opcode=0x${in.a.bits.opcode}%x param=0x${in.a.bits.param}%x size=0x${in.a.bits.size}%x source=0x${in.a.bits.source}%x address=0x${in.a.bits.address}%x mask=0x${in.a.bits.mask}%x data=0x${in.a.bits.data}%x\n")
      }
      when (in.d.fire) {
          printf(cf"[CuteDebugAdapter:$label] TL-D fired: opcode=0x${in.d.bits.opcode}%x param=0x${in.d.bits.param}%x size=0x${in.d.bits.size}%x source=0x${in.d.bits.source}%x sink=0x${in.d.bits.sink}%x denied=${in.d.bits.denied} corrupt=${in.d.bits.corrupt} data=0x${in.d.bits.data}%x\n")
      }
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

class XSCuteImp(wrapper: XSCute) extends LazyModuleImp(wrapper) {
    (wrapper.node.in zip wrapper.node.out).foreach { case ((in, edgeIn), (out, edgeOut)) =>
      out <> in
    }

    val io = IO(new CUTETopIO)
    val cute = Module(new CUTEV2Top)
    io <> cute.io
    wrapper.cute_tl.module.io.mmu <> cute.io.mmu2llc
    io.mmu2llc := DontCare
}

class XSCute(implicit p: Parameters) extends LazyModule {
  val beatBytes = 32
  val transferBytes = 64
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
