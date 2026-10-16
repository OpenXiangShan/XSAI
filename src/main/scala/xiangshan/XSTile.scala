/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan

import org.chipsalliance.cde.config.{Config, Field, Parameters}
import chisel3._
import chisel3.util.{Valid, ValidIO, log2Up, Arbiter, Decoupled}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.tile.{BusErrorUnit, BusErrorUnitParams, BusErrors}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}
import freechips.rocketchip.amba.axi4._
import system.HasSoCParameter
import top.{ArgParser, BusPerfMonitor, Generator}
import utility._
import utility.sram.SramBroadcastBundle
import xscache.chi.{CHIAddrWidthKey, CHIDataCheckKey, CHIIssue, CHIPoisonKey, DecoupledPortIO, NonSecureKey, PortIO}
import xiangshan.backend.trace.TraceCoreInterface
import xiangshan.backend.fu.matrix._
import xiangshan.backend.fu.matrix.Bundles._
import cute.{CutePerfAsyncCrossing, CutePerfToCoreIO, XSCute}

object MatAcc extends Enumeration {
  type MatAcc = Value
  val AME, CUTE = Value
}

case object MatAccKey extends Field[MatAcc.Value](MatAcc.AME)

case class ExternalAcceleratorCDCParams(
  enable: Boolean = false,
  command: AsyncQueueParams = AsyncQueueParams(depth = 4, sync = 3, safe = true),
  completion: AsyncQueueParams = AsyncQueueParams(depth = 4, sync = 3, safe = true),
  matrixData: AsyncQueueParams = AsyncQueueParams(depth = 2, sync = 3, safe = true),
  tileLink: AsyncQueueParams = AsyncQueueParams(depth = 8, sync = 3, safe = true),
  perfSnapshot: AsyncQueueParams = AsyncQueueParams.singleton(sync = 3, safe = true)
)

case object ExternalAcceleratorCDCKey
  extends Field[ExternalAcceleratorCDCParams](ExternalAcceleratorCDCParams())

class XSTile()(implicit p: Parameters) extends LazyModule
  with HasXSParameter
  with HasSoCParameter
{
  override def shouldBeInlined: Boolean = false
  val core = LazyModule(new XSCore())
  val l2top = LazyModule(new L2Top())

  val cuteOpt = p(MatAccKey) match {
    case MatAcc.AME => None
    case MatAcc.CUTE => Some(LazyModule(new XSCute()))
  }
  private val acceleratorCDC = p(ExternalAcceleratorCDCKey)
  private val useAcceleratorCDC = cuteOpt.nonEmpty && acceleratorCDC.enable

  val enableL2 = coreParams.L2CacheParamsOpt.isDefined
  // =========== Public Ports ============
  val memBlock = core.memBlock.inner
  val memory_port = None
  val tl_uncache = l2top.inner.mmio_port
  val sep_tl_opt = l2top.inner.sep_tl_port_opt
  val beu_int_source = l2top.inner.beu.intNode
  val core_reset_sink = BundleBridgeSink(Some(() => Reset()))
  val clint_int_node = l2top.inner.clint_int_node
  val plic_int_node = l2top.inner.plic_int_node
  val debug_int_node = l2top.inner.debug_int_node
  val nmi_int_node = l2top.inner.nmi_int_node
  memBlock.clint_int_sink := IntBuffer() := clint_int_node
  memBlock.plic_int_sink :*= IntBuffer() :*= plic_int_node
  memBlock.debug_int_sink := IntBuffer() := debug_int_node
  memBlock.nmi_int_sink   := IntBuffer() := nmi_int_node
  memBlock.beu_local_int_sink := l2top.inner.beu_local_int_source_buffer

  // =========== Components' Connection ============
  // L1 to l1_xbar
  coreParams.dcacheParametersOpt.map { _ =>
    l2top.inner.misc_l2_pmu := l2top.inner.l1d_logger := memBlock.dcache_port :=
      memBlock.l1d_to_l2_buffer.node := memBlock.dcache.clientNode
  }

  val cuteTLAsyncCrossings = cuteOpt.toSeq.flatMap { cute =>
    cute.node.map { node =>
      if (useAcceleratorCDC) {
        val source = LazyModule(new TLAsyncCrossingSource())
        val sink = LazyModule(new TLAsyncCrossingSink(acceleratorCDC.tileLink))
        source.node := node
        sink.node := source.node
        l2top.inner.misc_l2_pmu := l2top.inner.cute_logger := sink.node
        Some((source, sink))
      } else {
        l2top.inner.misc_l2_pmu := l2top.inner.cute_logger := node
        None
      }
    }
  }.flatten

  l2top.inner.misc_l2_pmu := l2top.inner.l1i_logger := memBlock.frontendBridge.icache_node
  if (!coreParams.softPTW) {
    l2top.inner.misc_l2_pmu := l2top.inner.ptw_logger := l2top.inner.ptw_to_l2_buffer.node := memBlock.ptw_to_l2_buffer.node
  }

  // L2 Prefetch
  l2top.inner.l2cache match {
    case Some(l2) =>
      l2.pf_recv_node.foreach(recv => {
        println("Connecting L1 prefetcher to L2!")
        recv := memBlock.l2_pf_sender_opt.get
      })
    case None =>
  }

  val core_l3_tpmeta_source_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_source_node
    case None => None
  }
  val core_l3_tpmeta_sink_port = l2top.inner.l2cache match {
    case Some(l2) => l2.tpmeta_sink_node
    case None => None
  }

  // mmio
  l2top.inner.i_mmio_port := l2top.inner.i_mmio_buffer.node := memBlock.frontendBridge.instr_uncache_node
  if (soc.EnableICacheCtrl) {
    memBlock.frontendBridge.icachectrl_node := l2top.inner.icachectrl_port_opt.get
  }
  l2top.inner.d_mmio_port := memBlock.uncache_port

  // =========== IO Connection ============
  class XSTileImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val io = IO(new Bundle {
      val hartId = Input(UInt(hartIdLen.W))
      val msiInfo = Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W)))
      val msiAck = Output(Bool())
      val teemsiInfo = Option.when(soc.IMSICParams.HasTEEIMSIC)(Input(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W))))
      val teemsiAck = Option.when(soc.IMSICParams.HasTEEIMSIC)(Output(Bool()))
      val reset_vector = Input(UInt(PAddrBits.W))
      val cpu_halt = Output(Bool())
      val cpu_crtical_error = Output(Bool())
      val hartIsInReset = Output(Bool())
      val traceCoreInterface = new TraceCoreInterface
      val debugTopDown = new Bundle {
        val robHeadPaddr = Valid(UInt(PAddrBits.W))
        val l3MissMatch = Input(Bool())
      }
      val l3Miss = Input(Bool())
      val chi = Option.when(isOpenLLC)(new PortIO)
      val decoupledCHI = Option.when(isZhuJiang)(
        new DecoupledPortIO()(p.alter((_, _, _) => {
          case CHIIssue => p(CHIIssue)
          case CHIAddrWidthKey => p(CHIAddrWidthKey)
          case NonSecureKey => p(NonSecureKey)
          case CHIDataCheckKey => "none"
          case CHIPoisonKey => false
        }))
      )
      val nodeID = Some(Input(UInt(NodeIDWidth.W)))
      val clintTime = Input(ValidIO(UInt(64.W)))
      val dft = Option.when(hasDFT)(Input(new SramBroadcastBundle))
      val dft_reset = Option.when(hasMbist)(Input(new DFTResetSignals()))
      val l2_flush_en = Option.when(EnablePowerDown) (Output(Bool()))
      val l2_flush_done = Option.when(EnablePowerDown) (Output(Bool()))
      val acceleratorClock = Option.when(useAcceleratorCDC)(Input(Clock()))
    })

    dontTouch(io.hartId)
    dontTouch(io.msiInfo)
    io.teemsiInfo.foreach(dontTouch(_))
    io.chi.foreach(dontTouch(_))
    io.decoupledCHI.foreach(dontTouch(_))

    val core_soft_rst = core_reset_sink.in.head._1 // unused

    l2top.module.io.hartId.fromTile := io.hartId
    core.module.io.hartId := l2top.module.io.hartId.toCore
    core.module.io.reset_vector := l2top.module.io.reset_vector.toCore
    core.module.io.msiInfo := l2top.module.io.msiInfo.toCore
    l2top.module.io.msiInfo.fromTile := io.msiInfo
    core.module.io.teemsiInfo zip l2top.module.io.teemsiInfo foreach { case (core_teemsiInfo, l2top_teemsiInfo) =>
      core_teemsiInfo := l2top_teemsiInfo.toCore
    }
    l2top.module.io.teemsiInfo zip io.teemsiInfo foreach { case (l2top_teemsiInfo, io_teemsiInfo) =>
      l2top_teemsiInfo.fromTile := io_teemsiInfo
    }
    core.module.io.clintTime := l2top.module.io.clintTime.toCore
    l2top.module.io.clintTime.fromTile := io.clintTime
    l2top.module.io.reset_vector.fromTile := io.reset_vector
    l2top.module.io.cpu_halt.fromCore := core.module.io.cpu_halt
    io.cpu_halt := l2top.module.io.cpu_halt.toTile
    l2top.module.io.cpu_critical_error.fromCore := core.module.io.cpu_critical_error
    io.cpu_crtical_error := l2top.module.io.cpu_critical_error.toTile
    l2top.module.io.msiAck.fromCore := core.module.io.msiAck
    io.msiAck := l2top.module.io.msiAck.toTile
    l2top.module.io.teemsiAck zip core.module.io.teemsiAck foreach { case (l2top_teemsiAck, core_teemsiAck) =>
      l2top_teemsiAck.fromCore := core_teemsiAck
    }
    io.teemsiAck zip l2top.module.io.teemsiAck foreach { case (io_teemsiAck, l2top_teemsiAck) =>
      io_teemsiAck := l2top_teemsiAck.toTile
    }

    l2top.module.io.hartIsInReset.resetInFrontend := core.module.io.resetInFrontend
    io.hartIsInReset := l2top.module.io.hartIsInReset.toTile
    l2top.module.io.traceCoreInterface.fromCore <> core.module.io.traceCoreInterface
    io.traceCoreInterface <> l2top.module.io.traceCoreInterface.toTile

    l2top.module.io.beu_errors.icache <> core.module.io.beu_errors.icache
    l2top.module.io.beu_errors.dcache <> core.module.io.beu_errors.dcache
    l2top.module.io.beu_errors.uncache <> core.module.io.beu_errors.uncache

    l2top.module.io.l2_flush_en.foreach { _ := core.module.io.l2_flush_en }
    io.l2_flush_en.foreach { _ := core.module.io.l2_flush_en }
    core.module.io.l2_flush_done := l2top.module.io.l2_flush_done.getOrElse(false.B)
    io.l2_flush_done.foreach { _ := l2top.module.io.l2_flush_done.getOrElse(false.B) }

    l2top.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    l2top.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })
    core.module.io.dft.zip(io.dft).foreach({ case (a, b) => a := b })
    core.module.io.dft_reset.zip(io.dft_reset).foreach({ case (a, b) => a := b })

    if (enableL2) {
      // TODO: add ECC interface of L2
      l2top.module.io.pfCtrlFromCore := core.module.io.l2PfCtrl

      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := l2top.module.io.debugTopDown.l2MissMatch
      l2top.module.io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
      l2top.module.io.debugTopDown.robTrueCommit := core.module.io.debugTopDown.robTrueCommit
      l2top.module.io.l2_pmp_resp := core.module.io.l2_pmp_resp
      core.module.io.l2_tlb_req <> l2top.module.io.l2_tlb_req
      core.module.io.topDownInfo.l2Miss := l2top.module.io.l2Miss

      core.module.io.perfEvents <> l2top.module.io.perfEvents
    } else {

      l2top.module.io.beu_errors.l2 <> 0.U.asTypeOf(l2top.module.io.beu_errors.l2)
      core.module.io.l2_hint.bits.sourceId := l2top.module.io.l2_hint.bits.sourceId
      core.module.io.l2_hint.bits.isKeyword := l2top.module.io.l2_hint.bits.isKeyword
      core.module.io.l2_hint.valid := l2top.module.io.l2_hint.valid

      core.module.io.l2PfqBusy := false.B
      core.module.io.debugTopDown.l2MissMatch := false.B
      core.module.io.topDownInfo.l2Miss := false.B

      core.module.io.l2_tlb_req.req.valid := false.B
      core.module.io.l2_tlb_req.req.bits := DontCare
      core.module.io.l2_tlb_req.req_kill := DontCare
      core.module.io.l2_tlb_req.resp.ready := true.B

      core.module.io.perfEvents <> DontCare
    }

    io.debugTopDown.robHeadPaddr := core.module.io.debugTopDown.robHeadPaddr
    core.module.io.debugTopDown.l3MissMatch := io.debugTopDown.l3MissMatch
    l2top.module.io.l3Miss.fromTile := io.l3Miss
    core.module.io.topDownInfo.l3Miss := l2top.module.io.l3Miss.toCore

    io.chi.foreach(_ <> l2top.module.io.chi.get)
    io.decoupledCHI.foreach(_ <> l2top.module.io.decoupledCHI.get)
    l2top.module.io.nodeID.foreach(_ := io.nodeID.get)

    if (debugOpts.ResetGen && enableL2) {
      core.module.reset := l2top.module.reset_core
    }

    l2top.module.io.matrixPrefetch.foreach(_ := 0.U.asTypeOf(new xscache.coupledL2.prefetch.MatrixPrefetchControl))

    val acceleratorClock = io.acceleratorClock.getOrElse(clock)
    val acceleratorReset = if (useAcceleratorCDC) {
      withClockAndReset(acceleratorClock, reset) { ResetGen(io.dft_reset) }
    } else {
      reset.asAsyncReset
    }

    cuteOpt.foreach { case cute =>
      l2top.module.io.matrixDataOut512L2 := DontCare
      cute.module.io := DontCare
      cute.module.clock := acceleratorClock
      cute.module.reset := acceleratorReset

      cuteTLAsyncCrossings.foreach { case (source, sink) =>
        source.module.clock := acceleratorClock
        source.module.reset := acceleratorReset
        sink.module.clock := clock
        sink.module.reset := reset
      }

      core.module.io.amuCtrl.foreach { amuCtrl =>
        if (useAcceleratorCDC) {
          val commandCrossing = Module(new AsyncQueue(new AmuCtrlIO, acceleratorCDC.command))
          commandCrossing.io.enq_clock := clock
          commandCrossing.io.enq_reset := reset.asBool
          commandCrossing.io.deq_clock := acceleratorClock
          commandCrossing.io.deq_reset := acceleratorReset.asBool
          commandCrossing.io.enq <> amuCtrl
          val coreCommand = commandCrossing.io.deq.bits
          val cuteCommand = cute.module.io.cute.ctrl2top.amuCtrl.bits
          require(coreCommand.op.getWidth == cuteCommand.op.getWidth)
          require(coreCommand.data.getWidth == cuteCommand.data.getWidth)
          require(coreCommand.pc.isDefined == cuteCommand.pc.isDefined)
          require(coreCommand.coreid.isDefined == cuteCommand.coreid.isDefined)
          cute.module.io.cute.ctrl2top.amuCtrl.valid := commandCrossing.io.deq.valid
          cuteCommand.op := coreCommand.op
          cuteCommand.data := coreCommand.data
          cuteCommand.pc.zip(coreCommand.pc).foreach { case (sink, source) => sink := source }
          cuteCommand.coreid.zip(coreCommand.coreid).foreach { case (sink, source) => sink := source }
          commandCrossing.io.deq.ready := cute.module.io.cute.ctrl2top.amuCtrl.ready
        } else {
          cute.module.io.cute.ctrl2top.amuCtrl <> amuCtrl
        }
      }
      core.module.io.amuRelease.foreach { amuRelease =>
        if (useAcceleratorCDC) {
          val completionCrossing = Module(new AsyncQueue(new AmuReleaseIO2XS, acceleratorCDC.completion))
          completionCrossing.io.enq_clock := acceleratorClock
          completionCrossing.io.enq_reset := acceleratorReset.asBool
          completionCrossing.io.deq_clock := clock
          completionCrossing.io.deq_reset := reset.asBool
          completionCrossing.io.enq.valid := cute.module.io.cute.ctrl2top.mrelease.valid
          completionCrossing.io.enq.bits := cute.module.io.cute.ctrl2top.mrelease.bits.asTypeOf(
            completionCrossing.io.enq.bits
          )
          cute.module.io.cute.ctrl2top.mrelease.ready := completionCrossing.io.enq.ready
          amuRelease <> completionCrossing.io.deq
        } else {
          amuRelease.bits := cute.module.io.cute.ctrl2top.mrelease.bits.asTypeOf(amuRelease.bits)
          amuRelease.valid := cute.module.io.cute.ctrl2top.mrelease.valid
          cute.module.io.cute.ctrl2top.mrelease.ready := amuRelease.ready
        }
      }
      for {
        l2Prefetch <- l2top.module.io.matrixPrefetch
        cutePrefetch <- cute.module.io.cute.matrixPrefetch
      } l2Prefetch := cutePrefetch

      val matrix_data_out = l2top.module.io.matrixDataOut512L2
      val matrix_data_in = cute.module.io.matrix_data_in
      val outCount = matrix_data_out.length
      val inCount = matrix_data_in.length

      require(outCount > 0, "matrix_data_out must have at least one channel")
      require(inCount > 0, "matrix_data_in must have at least one channel")

      matrix_data_out.foreach(_.ready := false.B)
      val matrixDataToCute = Wire(Vec(inCount, Decoupled(chiselTypeOf(matrix_data_out(0).bits))))
      matrixDataToCute.foreach { in =>
        in.valid := false.B
        in.bits := DontCare
      }

      if (inCount >= outCount) {
        (matrixDataToCute zip matrix_data_out) foreach { case (in, out) =>
          in <> out
        }
      }
      else {
        val baseGroupSize = outCount / inCount
        val largerGroupCount = outCount % inCount

        for (inIdx <- 0 until inCount) {
          // Split consecutive output channels into nearly-even groups.
          // The first `largerGroupCount` groups take one extra channel so the remainder
          // is distributed to the front, e.g. 10 outputs over 3 inputs becomes 4/3/3.
          val groupStart = inIdx * baseGroupSize + (inIdx min largerGroupCount)
          val groupSize = baseGroupSize + (if (inIdx < largerGroupCount) 1 else 0)
          val groupEnd = groupStart + groupSize

          val arb = Module(new Arbiter(chiselTypeOf(matrix_data_out(0).bits), groupSize))
          for ((outIdx, arbIdx) <- (groupStart until groupEnd).zipWithIndex) {
            arb.io.in(arbIdx) <> matrix_data_out(outIdx)
          }
          matrixDataToCute(inIdx) <> arb.io.out
        }
      }

      for (i <- 0 until inCount) {
        if (useAcceleratorCDC) {
          val matrixCrossing = Module(new AsyncQueue(chiselTypeOf(matrixDataToCute(i).bits), acceleratorCDC.matrixData))
          matrixCrossing.io.enq_clock := clock
          matrixCrossing.io.enq_reset := reset.asBool
          matrixCrossing.io.deq_clock := acceleratorClock
          matrixCrossing.io.deq_reset := acceleratorReset.asBool
          matrixCrossing.io.enq <> matrixDataToCute(i)
          matrix_data_in(i) <> matrixCrossing.io.deq
        } else {
          matrix_data_in(i) <> matrixDataToCute(i)
        }
      }

      core.module.io.cutePerf.foreach { corePerf =>
        if (useAcceleratorCDC) {
          val perfCrossing = Module(new CutePerfAsyncCrossing(acceleratorCDC.perfSnapshot))
          perfCrossing.io.sourceClock := acceleratorClock
          perfCrossing.io.sourceReset := acceleratorReset.asBool
          perfCrossing.io.source := cute.module.io.cute.perf
          perfCrossing.io.sinkClock := clock
          perfCrossing.io.sinkReset := reset.asBool
          corePerf := perfCrossing.io.sink
        } else {
          corePerf <> cute.module.io.cute.perf
        }
      }

      cute.module.io.hartId := io.hartId
      cute.module.io.difftestClock.foreach(_ := clock)
      cute.module.io.difftestReset.foreach(_ := reset.asBool)
    }

    if (HasMatrixExtension && cuteOpt.isEmpty) {
      core.module.io.cutePerf.foreach(_ := 0.U.asTypeOf(new CutePerfToCoreIO))
    }

    if (!HasMatrixExtension) {
      l2top.module.io.matrixDataOut512L2 := DontCare
      l2top.module.io.matrixDataOut512L2.foreach(_.ready := false.B)
    }
  }

  lazy val module = new XSTileImp(this)
}
