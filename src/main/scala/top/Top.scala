/***************************************************************************************
* Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package top

import chisel3._
import chisel3.util._
import chisel3.experimental.dataview._
import difftest.{DifftestMemIO, DifftestModule, HasDiffTestInterfaces}
import xiangshan._
import utils._
import utility._
import utility.sram.SramBroadcastBundle
import xscache.chi.CHILogger
import xscache.openLLC.{OpenLLC, OpenLLCParamKey, OpenNCB}
import xscache.openLLC.TargetBinder._
import xs.utils.debug.{HardwareAssertionKey, HwaParams}
import xs.utils.perf.{DebugOptions => ZJDebugOptions, DebugOptionsKey => ZJDebugOptionsKey}
import xs.utils.perf.{LogUtilsOptions => ZJLogUtilsOptions, LogUtilsOptionsKey => ZJLogUtilsOptionsKey}
import xs.utils.perf.{PerfCounterOptions => ZJPerfCounterOptions, PerfCounterOptionsKey => ZJPerfCounterOptionsKey, XSPerfLevel => ZJXSPerfLevel}
import zhujiang.{HasCHIToZhuJiangBridge, HasZhuJiangAXI4Bridge, Zhujiang, ZJParametersKey}
import zhujiang.perf.{XiangShanUtilityPerfBackend, ZJPerfBackendKey}
import cc.xiangshan.openncb._
import system._
import device._
import chisel3.stage.ChiselGeneratorAnnotation
import org.chipsalliance.cde.config._
import freechips.rocketchip.devices.debug.DebugModuleKey
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._
import freechips.rocketchip.interrupts._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.jtag.JTAGIO
import freechips.rocketchip.util.{AsyncQueueParams, AsyncQueueSink, AsyncQueueSource, AsyncResetSynchronizerShiftReg}
import chisel3.experimental.annotate
import sifive.enterprise.firrtl.NestedPrefixModulesAnnotation

import scala.collection.mutable.Map
import difftest.gateway.Gateway

abstract class BaseXSSoc()(implicit p: Parameters) extends LazyModule
  with HasSoCParameter
  with BindingScope
{
  lazy val tlManagers: List[TLNexusNode] = List()
  lazy val dts = DTS(bindingTree)
  lazy val json = JSON(bindingTree)

  // collect info for DTS
  ResourceBinding {
    val width = ResourceInt(2)
    val model = "xiangshan," + os.read(os.resource / "publishVersion")
    val compatible = "freechips,rocketchip-unknown"
    Resource(ResourceAnchors.root, "model").bind(ResourceString(model))
    Resource(ResourceAnchors.root, "compat").bind(ResourceString(compatible + "-dev"))
    Resource(ResourceAnchors.soc, "compat").bind(ResourceString(compatible + "-soc"))
    Resource(ResourceAnchors.root, "width").bind(width)
    Resource(ResourceAnchors.soc, "width").bind(width)
    Resource(ResourceAnchors.cpus, "width").bind(ResourceInt(1))
    def bindManagers(xbar: TLNexusNode) = {
      ManagerUnification(xbar.edges.in.head.manager.managers).foreach{ manager =>
        manager.resources.foreach(r => r.bind(manager.toResource))
      }
    }
    tlManagers.foreach(xbar => bindManagers(xbar))
  }
}

trait HasDTSImp[+L <: BaseXSSoc] { this: LazyRawModuleImp =>
  val dtsLM = wrapper.asInstanceOf[L]
  FileRegisters.add("dts", dtsLM.dts)
  FileRegisters.add("graphml", dtsLM.graphML)
  FileRegisters.add("json", dtsLM.json)
  FileRegisters.add("plusArgs", freechips.rocketchip.util.PlusArgArtefacts.serialize_cHeader())
}

// A local integration island: only registered NoC-clock levels/snapshots leave
// it. Native synchronizers and both halves of its timestamp FIFO stay local.
class ZhuJiangLocalSideband(txWidth: Int, rxWidth: Int, timeFromFast: Boolean) extends RawModule {
  val io = IO(new Bundle {
    val fast_clock = Input(Clock())
    val noc_clock = Input(Clock())
    val cold_reset = Input(AsyncReset())
    val fast_reset_req = Input(Bool())
    val fast_reset = Output(AsyncReset())
    val core_reset = Output(AsyncReset())
    val fast_in = Input(UInt(txWidth.W))
    val noc_out = Output(UInt(txWidth.W))
    val noc_in = Input(UInt(rxWidth.W))
    val fast_out = Output(UInt(rxWidth.W))
    val time_in = Input(ValidIO(UInt(64.W)))
    val time_out = Output(ValidIO(UInt(64.W)))
  })
  val fastReset = withClockAndReset(io.fast_clock, io.cold_reset) { ResetGen() }
  val nocReset = withClockAndReset(io.noc_clock, io.cold_reset) { ResetGen() }
  io.fast_reset := fastReset.asAsyncReset
  io.core_reset := withClockAndReset(io.fast_clock,
    (io.cold_reset.asBool || io.fast_reset_req).asAsyncReset) { ResetGen() }.asAsyncReset
  io.noc_out := withClockAndReset(io.noc_clock, nocReset) {
    AsyncResetSynchronizerShiftReg(io.fast_in, 3, 0)
  }
  // This register is the receiving end of the synchronous inter-FPGA link.
  val nocReceived = withClockAndReset(io.noc_clock, nocReset) {
    val received = RegInit(0.U(rxWidth.W))
    received := io.noc_in
    received
  }
  io.fast_out := withClockAndReset(io.fast_clock, fastReset) {
    AsyncResetSynchronizerShiftReg(nocReceived, 3, 0)
  }
  val queueParams = AsyncQueueParams(depth = 8, sync = 3, safe = true)
  val sourceClock = if (timeFromFast) io.fast_clock else io.noc_clock
  val sourceReset = if (timeFromFast) fastReset else nocReset
  val sinkClock = if (timeFromFast) io.noc_clock else io.fast_clock
  val sinkReset = if (timeFromFast) nocReset else fastReset
  val timeSource = withClockAndReset(sourceClock, sourceReset) {
    Module(new AsyncQueueSource(UInt(64.W), queueParams))
  }
  val timeSink = withClockAndReset(sinkClock, sinkReset) {
    Module(new AsyncQueueSink(UInt(64.W), queueParams))
  }
  timeSink.io.async <> timeSource.io.async
  // ValidIO carries sampled time, not lossless events; accept snapshots when
  // ready, exactly as the canonical XSNoCTop timestamp source does.
  timeSource.io.enq.valid := io.time_in.valid
  timeSource.io.enq.bits := io.time_in.bits
  timeSink.io.deq.ready := true.B
  io.time_out.valid := withClockAndReset(sinkClock, sinkReset) {
    RegNext(timeSink.io.deq.valid, false.B)
  }
  io.time_out.bits := withClockAndReset(sinkClock, sinkReset) {
    RegEnable(timeSink.io.deq.bits, 0.U(64.W), timeSink.io.deq.valid)
  }
}

class XSTop()(implicit p: Parameters) extends BaseXSSoc()
  with HasCHIToZhuJiangBridge
  with HasZhuJiangAXI4Bridge
{
  val nocMisc = Some(LazyModule(new MemMisc()))
  val misc: MemMisc = nocMisc.get

  override lazy val tlManagers = List(
    misc.l3_xbar.map(_.asInstanceOf[TLNexusNode]),
    misc.peripheralXbar.map(_.asInstanceOf[TLNexusNode])
  ).flatten

  println(s"FPGASoC cores: $NumCores banks: $L3NBanks block size: $L3BlockSize bus size: $L3OuterBusWidth")

  val core_with_l2 = tiles.map(coreParams =>
    LazyModule(new XSTile()(XSCachedParametersOptional(p(CachedParameterKey), p.alter((site, here, up) => {
      case XSCoreParamsKey => coreParams
      case PerfCounterOptionsKey => up(PerfCounterOptionsKey).copy(perfDBHartID = coreParams.HartId)
    }))))
  )

  val chi_llcBridge_opt = Option.when(isOpenLLC)(
    LazyModule(new OpenNCB()(p.alter((site, here, up) => {
      case NCBParametersKey => new NCBParameters(
        outstandingDepth    = 64,
        axiMasterOrder      = EnumAXIMasterOrder.WriteAddress,
        readCompDMT         = false,
        writeCancelable     = false,
        writeNoError        = true,
        axiBurstAlwaysIncr  = true,
        chiDataCheck        = EnumCHIDataCheck.OddParity
      )
    })))
  )

  val noLLCBridge_opt = Option.when(isNoLLC)(
    LazyModule(new NoLLCCHI2AXI(entries = p(NoLLCEntries)))
  )

  val chi_mmioBridge_opt = Seq.fill(NumCores)(Option.when(usesLcreditCHI)(
    LazyModule(new OpenNCB()(p.alter((site, here, up) => {
      case NCBParametersKey => new NCBParameters(
        outstandingDepth            = 32,
        axiMasterOrder              = EnumAXIMasterOrder.None,
        readCompDMT                 = false,
        writeCancelable             = false,
        writeNoError                = true,
        asEndpoint                  = false,
        acceptOrderEndpoint         = true,
        acceptMemAttrDevice         = true,
        readReceiptAfterAcception   = true,
        axiBurstAlwaysIncr          = true,
        chiDataCheck                = EnumCHIDataCheck.OddParity
      )
    })))
  ))

  val nmiIntNode = IntSourceNode(IntSourcePortSimple(1, NumCores, (new NonmaskableInterruptIO).elements.size))
  val nmi = InModuleBody(nmiIntNode.makeIOs())

  val zhujiangSidebandIntNodes = Option.when(EnableZhuJiangAsyncBridge) {
    tiles.map(_ => (IntAdapterNode(), IntAdapterNode(), IntAdapterNode(), IntAdapterNode()))
  }
  for (i <- 0 until NumCores) {
    zhujiangSidebandIntNodes match {
      case Some(nodes) =>
        val (clint, plic, debug, beu) = nodes(i)
        core_with_l2(i).clint_int_node := clint := misc.timer.intnode
        core_with_l2(i).plic_int_node :*= plic :*= misc.plic.intnode
        core_with_l2(i).debug_int_node := debug := misc.debugModule.debug.dmOuter.dmOuter.intnode
        misc.plic.intnode := beu := core_with_l2(i).beu_int_source
      case None =>
        core_with_l2(i).clint_int_node := misc.timer.intnode
        core_with_l2(i).plic_int_node :*= misc.plic.intnode
        core_with_l2(i).debug_int_node := misc.debugModule.debug.dmOuter.dmOuter.intnode
        misc.plic.intnode := IntBuffer() := core_with_l2(i).beu_int_source
    }
    core_with_l2(i).nmi_int_node := nmiIntNode
    misc.SepTLXbarOpt.foreach { SepTLXbarOpt =>
      // SeperateBus can only be connected to DebugModule now in non-XSNoCTop environment
      println(s"SeparateDM: ${SeperateDM}")
      println(s"misc.SepTLXbarOpt: ${misc.SepTLXbarOpt}")
      require(core_with_l2(i).sep_tl_opt.isDefined)
      require(SeperateBusRanges.size >= 1)
      require(SeperateBusRanges.head.base <= p(DebugModuleKey).get.address.base)
      require(SeperateBusRanges.head.base <= p(SoCParamsKey).TIMERRange.base)
      SepTLXbarOpt := core_with_l2(i).sep_tl_opt.get
    }
  }
  val core_rst_nodes = core_with_l2.map(_ => BundleBridgeSource(() => Reset()))

  core_rst_nodes.zip(core_with_l2.map(_.core_reset_sink)).foreach({
    case (source, sink) =>  sink := source
  })

  chi_llcBridge_opt match {
    case Some(ncb) =>
      misc.soc_xbar.get := ncb.axi4node
    case None =>
  }

  noLLCBridge_opt.foreach { bridge =>
    misc.soc_xbar.get := bridge.axi4node
  }

  chi_mmioBridge_opt.foreach { e =>
    e match {
      case Some(ncb) =>
        misc.soc_xbar.get := ncb.axi4node
      case None =>
    }
  }

  private val zhujiangNocConfig = Option.when(isZhuJiang)(
    ZhuJiangNoCTopology(
      NumCores, soc.ZhuJiangParams, L3OuterBusWidth, if (EnableZhuJiangAsyncBridge) "async" else "sync"
    )
  )
  private val zhujiangCfgNodes = zhujiangNocConfig
    .map(_.island.filter(_.nodeType == xijiang.NodeType.HI))
    .getOrElse(Seq.empty)

  val zhujiangMemMaster = Option.when(isZhuJiang)(AXI4MasterNode(Seq(AXI4MasterPortParameters(
    masters = Seq(AXI4MasterParameters(
      name = "zhujiang-mem",
      id = IdRange(0, 8),
      aligned = true,
      maxFlight = Some(1)
    ))
  ))))

  // Only the NoC changes clock. Keep MemMisc and the cores together so their
  // timer, interrupt, debug and PMA connections retain their original domain.
  private def connectZhuJiangMaster(
    master: AXI4MasterNode, buffered: Boolean, name: String
  ): Option[(AXI4AsyncCrossingSource, AXI4AsyncCrossingSink)] = {
    val crossing = Option.when(EnableZhuJiangAsyncBridge) {
      val source = LazyModule(new AXI4AsyncCrossingSource()).suggestName(s"${name}_source")
      val sink = LazyModule(new AXI4AsyncCrossingSink(soc.ZhuJiangParams.asyncParams)).suggestName(s"${name}_sink")
      source.node := master
      sink.node := source.node
      (source, sink)
    }
    crossing match {
      case Some((_, sink)) =>
        if (buffered) misc.soc_xbar.get := AXI4Buffer() := sink.node
        else misc.soc_xbar.get := sink.node
      case None =>
        if (buffered) misc.soc_xbar.get := AXI4Buffer() := master
        else misc.soc_xbar.get := master
    }
    crossing
  }
  val zhujiangMemCrossing = zhujiangMemMaster.flatMap(
    connectZhuJiangMaster(_, buffered = false, name = "zhujiang_mem_cdc")
  )

  val zhujiangCfgMasters = zhujiangCfgNodes.zipWithIndex.map { case (node, i) =>
    val outstanding = node.axiDevParams.map(_.outstanding).getOrElse(8)
    AXI4MasterNode(Seq(AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = s"zhujiang-cfg-$i",
        id = IdRange(0, outstanding),
        aligned = true,
        maxFlight = Some(1)
      ))
    )))
  }

  val zhujiangCfgCrossings = zhujiangCfgMasters.zipWithIndex.flatMap { case (master, i) =>
    connectZhuJiangMaster(master, buffered = true, name = s"zhujiang_cfg_${i}_cdc")
  }

  class XSTopImp(wrapper: XSTop) extends LazyRawModuleImp(wrapper)
    with HasDTSImp[XSTop]
  {
    soc.XSTopPrefix.foreach { prefix =>
      val mod = this.toNamed
      annotate(this)(Seq(NestedPrefixModulesAnnotation(mod, prefix, true)))
    }

    val peripheral = IO(new VerilogAXI4Record(misc.peripheral.elts.head.params))
    val memory = IO(new VerilogAXI4Record(misc.memory.elts.head.params))

    memory.viewAs[AXI4Bundle] <> misc.memory.elements.head._2
    peripheral.viewAs[AXI4Bundle] <> misc.peripheral.elements.head._2

    val io = IO(new Bundle {
      val clock = Input(Clock())
      val acceleratorClock = Option.when(
        p(ExternalAcceleratorCDCKey).enable && p(MatAccKey) == MatAcc.CUTE
      )(Input(Clock()))
      val reset = Input(AsyncReset())
      val noc_clock = Option.when(EnableZhuJiangAsyncBridge)(Input(Clock()))
      val noc_reset = Option.when(EnableZhuJiangAsyncBridge)(Input(AsyncReset()))
      val sram_config = Input(UInt(16.W))
      val extIntrs = Input(UInt(NrExtIntr.W))
      val pll0_lock = Input(Bool())
      val pll0_ctrl = Output(Vec(6, UInt(32.W)))
      val systemjtag = new Bundle {
        val jtag = Flipped(new JTAGIO(hasTRSTn = false))
        val reset = Input(AsyncReset()) // No reset allowed on top
        val mfr_id = Input(UInt(11.W))
        val part_number = Input(UInt(16.W))
        val version = Input(UInt(4.W))
      }
      val debug_reset = Output(Bool())
      val rtc_clock = Input(Clock())
      val cacheable_check = new TLPMAIO()
      val riscv_halt = Output(Vec(NumCores, Bool()))
      val riscv_critical_error = Output(Vec(NumCores, Bool()))
      val riscv_rst_vec = Input(Vec(NumCores, UInt(soc.PAddrBits.W)))
      val traceCoreInterface = Vec(NumCores, new Bundle {
        val fromEncoder = Input(new Bundle {
          val enable = Bool()
          val stall  = Bool()
        })
        val toEncoder   = Output(new Bundle {
          val cause     = UInt(TraceCauseWidth.W)
          val tval      = UInt(TraceTvalWidth.W)
          val priv      = UInt(TracePrivWidth.W)
          val mstatus   = UInt(TraceStatusWidth.W)
          val valid     = UInt(TraceGrpNum.W)
          val iaddr     = UInt((TraceGrpNum * TraceIaddrWidth).W)
          val itype     = UInt((TraceGrpNum * TraceItypeWidth).W)
          val iretire   = UInt((TraceGrpNum * TraceIretireWidthCompressed).W)
          val ilastsize = UInt((TraceGrpNum * TraceIlastsizeWidth).W)
        })
      })
    })

    // A NoC reset invalidates live coherent traffic: reset the whole system,
    // not one FIFO endpoint. Runtime per-domain reset is not supported.
    val systemReset = io.noc_reset.map(r => (io.reset.asBool || r.asBool).asAsyncReset).getOrElse(io.reset)
    val nocClock = io.noc_clock.getOrElse(io.clock)
    val reset_sync = withClockAndReset(io.clock, systemReset) { ResetGen() }
    val nocReset = if (EnableZhuJiangAsyncBridge) {
      withClockAndReset(nocClock, systemReset) { ResetGen() }.asAsyncReset
    } else systemReset
    val jtag_reset_sync = withClockAndReset(io.systemjtag.jtag.TCK, io.systemjtag.reset) { ResetGen() }
    val chi_openllc_opt = Option.when(isOpenLLC)(
      withClockAndReset(io.clock, systemReset) {
        Module(new OpenLLC()(p.alter((site, here, up) => {
          case OpenLLCParamKey => soc.OpenLLCParamsOpt.get.copy(
            hartIds = tiles.map(_.HartId),
            FPGAPlatform = debugOpts.FPGAPlatform
          )
        })))
      }
    )
    val zhujiangParams = p.alterPartial {
      case ZJParametersKey => zhujiangNocConfig.getOrElse(soc.ZhuJiangParams)
      case ZJPerfBackendKey => XiangShanUtilityPerfBackend
      case HardwareAssertionKey => HwaParams(enable = false)
      case ZJLogUtilsOptionsKey => ZJLogUtilsOptions(
        enableDebug = false,
        enablePerf = debugOpts.EnablePerfDebug,
        fpgaPlatform = debugOpts.FPGAPlatform
      )
      case ZJPerfCounterOptionsKey => ZJPerfCounterOptions(
        enablePerfPrint = debugOpts.EnablePerfDebug && !debugOpts.FPGAPlatform,
        enablePerfDB = debugOpts.EnableRollingDB && !debugOpts.FPGAPlatform,
        perfLevel = ZJXSPerfLevel.withName(debugOpts.PerfLevel),
        perfDBHartID = 0
      )
      case ZJDebugOptionsKey => ZJDebugOptions(
        FPGAPlatform = debugOpts.FPGAPlatform,
        EnableDifftest = false,
        AlwaysBasicDiff = false,
        EnableDebug = false,
        EnablePerfDebug = debugOpts.EnablePerfDebug,
        UseDRAMSim = false,
        EnableTopDown = false,
        EnableChiselDB = false,
        AlwaysBasicDB = false,
        EnableRollingDB = debugOpts.EnableRollingDB,
        EnableHWMoniter = false
      )
    }
    val zhujiang_opt = Option.when(isZhuJiang)(
      withClockAndReset(nocClock, nocReset) {
        Module(new Zhujiang()(zhujiangParams))
      }
    )
    (zhujiangMemCrossing.toSeq ++ zhujiangCfgCrossings).foreach { case (source, sink) =>
      source.module.clock := nocClock
      source.module.reset := nocReset
      sink.module.clock := io.clock
      sink.module.reset := reset_sync
    }

    // override LazyRawModuleImp's clock and reset
    childClock := io.clock
    childReset := reset_sync

    // output
    io.debug_reset := misc.module.debug_module_io.debugIO.ndreset

    // input
    dontTouch(io)
    dontTouch(memory)
    misc.module.ext_intrs := io.extIntrs
    misc.module.pll0_lock := io.pll0_lock
    misc.module.cacheable_check <> io.cacheable_check

    io.pll0_ctrl <> misc.module.pll0_ctrl

    val msiInfo = WireInit(0.U.asTypeOf(ValidIO(UInt(soc.IMSICParams.MSI_INFO_WIDTH.W))))
    // syscnt io input descrip
    val ref_reset_sync = withClockAndReset(io.rtc_clock, systemReset) { ResetGen() }
    misc.module.scntIO.update_en := false.B
    misc.module.scntIO.update_value := 0.U
    misc.module.scntIO.stop_en := false.B
    misc.module.rtc_clock := io.rtc_clock // syscnt clock
    misc.module.rtc_reset := ref_reset_sync.asAsyncReset
    misc.module.bus_clock := io.clock
    misc.module.bus_reset := (if (EnableZhuJiangAsyncBridge) reset_sync else systemReset)

    val clintTime = WireInit(0.U.asTypeOf(ValidIO(UInt(64.W))))
    EnableClintAsyncBridge match {
      case Some(param) =>
        val timeClock = if (EnableZhuJiangAsyncBridge) io.clock else core_with_l2.head.module.clock
        val timeReset = if (EnableZhuJiangAsyncBridge) reset_sync else core_with_l2.head.module.reset
        val time_sink = withClockAndReset(timeClock, timeReset)(Module(new AsyncQueueSink(UInt(64.W), param)))
        time_sink.io.async <> misc.module.clintTime
        time_sink.io.deq.ready := true.B
        clintTime.valid := time_sink.io.deq.valid
        clintTime.bits  := time_sink.io.deq.bits
      case None =>
       clintTime := misc.module.clintTime
    }

    val asyncCoreResets = Seq.fill(NumCores)(WireDefault(reset_sync.asAsyncReset))
    val coreTimes = Seq.fill(NumCores)(Wire(ValidIO(UInt(64.W))))
    coreTimes.foreach(_ := clintTime)
    val observedHartReset = Wire(Vec(NumCores, Bool()))
    observedHartReset := VecInit(core_with_l2.map(_.module.io.hartIsInReset))
    val dmResetReqVec = misc.module.debug_module_io.resetCtrl.hartResetReq
      .getOrElse(0.U.asTypeOf(Vec(NumCores, Bool())))
    zhujiangSidebandIntNodes.foreach { nodes =>
      nodes.zipWithIndex.foreach { case ((clint, plic, debug, beu), i) =>
        val incoming = Seq(clint, plic, debug)
        val toCore = incoming.flatMap(_.in.flatMap(_._1.toSeq))
        val corePorts = incoming.flatMap(_.out.flatMap(_._1.toSeq))
        val fromCore = beu.in.flatMap(_._1.toSeq)
        val sharedPorts = beu.out.flatMap(_._1.toSeq)
        require(toCore.size == corePorts.size && fromCore.size == sharedPorts.size)
        val shared = Module(new ZhuJiangLocalSideband(toCore.size + 1, fromCore.size + 1, true))
          .suggestName(s"zhujiang_shared_sideband_$i")
        val local = Module(new ZhuJiangLocalSideband(fromCore.size + 1, toCore.size + 1, false))
          .suggestName(s"zhujiang_core_sideband_$i")
        for (side <- Seq(shared, local)) {
          side.io.fast_clock := io.clock
          side.io.noc_clock := nocClock
          side.io.cold_reset := systemReset
        }
        shared.io.fast_reset_req := false.B
        local.io.fast_reset_req := local.io.fast_out(toCore.size)
        shared.io.fast_in := VecInit(toCore :+ dmResetReqVec(i)).asUInt
        local.io.noc_in := shared.io.noc_out
        corePorts.zipWithIndex.foreach { case (port, bit) => port := local.io.fast_out(bit) }
        local.io.fast_in := VecInit(fromCore :+ core_with_l2(i).module.io.hartIsInReset).asUInt
        shared.io.noc_in := local.io.noc_out
        sharedPorts.zipWithIndex.foreach { case (port, bit) => port := shared.io.fast_out(bit) }
        observedHartReset(i) := shared.io.fast_out(fromCore.size)
        // Start from the existing RTC CDC's fast-domain snapshot.
        shared.io.time_in := clintTime
        local.io.time_in := shared.io.time_out
        coreTimes(i) := local.io.time_out
        asyncCoreResets(i) := local.io.core_reset
      }
    }

    for ((core, i) <- core_with_l2.zipWithIndex) {
      core.module.io.acceleratorClock.zip(io.acceleratorClock).foreach { case (tileClock, topClock) =>
        tileClock := topClock
      }
      core.module.io.hartId := i.U
      core.module.io.msiInfo := msiInfo
      core.module.io.teemsiInfo.foreach(_ := msiInfo)
      core.module.io.clintTime := coreTimes(i)
      io.riscv_halt(i) := core.module.io.cpu_halt
      io.riscv_critical_error(i) := core.module.io.cpu_crtical_error
      // trace Interface
      val traceInterface = core.module.io.traceCoreInterface
      traceInterface.fromEncoder := io.traceCoreInterface(i).fromEncoder
      io.traceCoreInterface(i).toEncoder.priv := traceInterface.toEncoder.priv
      io.traceCoreInterface(i).toEncoder.cause := traceInterface.toEncoder.trap.cause
      io.traceCoreInterface(i).toEncoder.tval := traceInterface.toEncoder.trap.tval
      io.traceCoreInterface(i).toEncoder.mstatus := traceInterface.toEncoder.mstatus
      io.traceCoreInterface(i).toEncoder.valid := VecInit(traceInterface.toEncoder.groups.map(_.valid)).asUInt
      io.traceCoreInterface(i).toEncoder.iaddr := VecInit(traceInterface.toEncoder.groups.map(_.bits.iaddr)).asUInt
      io.traceCoreInterface(i).toEncoder.itype := VecInit(traceInterface.toEncoder.groups.map(_.bits.itype)).asUInt
      io.traceCoreInterface(i).toEncoder.iretire := VecInit(traceInterface.toEncoder.groups.map(_.bits.iretire)).asUInt
      io.traceCoreInterface(i).toEncoder.ilastsize := VecInit(traceInterface.toEncoder.groups.map(_.bits.ilastsize)).asUInt

      core.module.io.dft.foreach(dontTouch(_) := DontCare)
      core.module.io.dft_reset.foreach(dontTouch(_) := DontCare)
      core.module.io.reset_vector := io.riscv_rst_vec(i)
    }

    withClockAndReset(io.clock, systemReset) {
      Option.when(usesLcreditCHI)(true.B).foreach { _ =>
        for ((core, i) <- core_with_l2.zipWithIndex) {
          val mmioLogger = CHILogger(s"L2[${i}]_MMIO", true)
          val llcLogger = CHILogger(s"L2[${i}]_LLC", true)
          bind(
            route(
              core.module.io.chi.get, Map((AddressSet(0x0L, 0x00007fffffffL), NumCores + i)) ++ AddressSet(0x0L,
              0xffffffffffffL).subtract(AddressSet(0x0L, 0x00007fffffffL)).map(addr => (addr, NumCores * 2)).toMap
            ),
            Map((NumCores + i) -> mmioLogger.io.up, (NumCores * 2) -> llcLogger.io.up)
          )
          chi_mmioBridge_opt(i).get.module.io.chi.connect(mmioLogger.io.down)
          require(core.module.io.chi.get.getWidth == llcLogger.io.up.getWidth)
          if (isOpenLLC) {
            chi_openllc_opt.get.io.rn(i) <> llcLogger.io.down
            require(llcLogger.io.down.getWidth == chi_openllc_opt.get.io.rn(i).getWidth)
          } else {
            noLLCBridge_opt.get.module.io.rn <> llcLogger.io.down
            require(llcLogger.io.down.getWidth == noLLCBridge_opt.get.module.io.rn.getWidth)
          }
        }
        if (isOpenLLC) {
          val memLogger = CHILogger(s"LLC_MEM", true)
          chi_openllc_opt.get.io.sn.connect(memLogger.io.up)
          chi_llcBridge_opt.get.module.io.chi.connect(memLogger.io.down)
          chi_openllc_opt.get.io.nodeID := (NumCores * 2).U
          chi_openllc_opt.foreach { l3 =>
            l3.io.debugTopDown.robHeadPaddr := core_with_l2.map(_.module.io.debugTopDown.robHeadPaddr)
          }
          core_with_l2.zip(chi_openllc_opt.get.io.debugTopDown.addrMatch).foreach { case (tile, l3Match) =>
            tile.module.io.debugTopDown.l3MissMatch := l3Match
          }
          core_with_l2.foreach(_.module.io.l3Miss := chi_openllc_opt.get.io.l3Miss)
        } else {
          noLLCBridge_opt.get.module.io.nodeID := (NumCores * 2).U
          dontTouch(noLLCBridge_opt.get.module.io.occupancy)
          dontTouch(noLLCBridge_opt.get.module.io.highWatermark)
          dontTouch(noLLCBridge_opt.get.module.io.accepted)
          core_with_l2.foreach(_.module.io.debugTopDown.l3MissMatch := false.B)
          core_with_l2.foreach(_.module.io.l3Miss := false.B)
        }
      }
    }

    zhujiang_opt.foreach { zj =>
      withClockAndReset(io.clock, if (EnableZhuJiangAsyncBridge) reset_sync else systemReset) {
        val zjp = zhujiangNocConfig.get
        val ccNodes = zjp.island.filter(_.nodeType == xijiang.NodeType.CC)
        require(ccNodes.size >= NumCores, s"ZhuJiang exposes ${ccNodes.size} CC nodes, but $NumCores cores are requested")
        require(zj.ccnIO.size >= NumCores, s"ZhuJiang exposes ${zj.ccnIO.size} CCN IOs, but $NumCores cores are requested")
        require(zj.ddrIO.nonEmpty, "XSAI + ZhuJiang expects at least one DDR AXI port")
        require(
          zj.cfgIO.size == zhujiangCfgMasters.size,
          s"ZhuJiang exposes ${zj.cfgIO.size} cfg AXI ports, but ${zhujiangCfgMasters.size} cfg master nodes were built"
        )

        for ((core, i) <- core_with_l2.zipWithIndex) {
          connectCHIToZhuJiang(core.module.io.decoupledCHI.get, zj.ccnIO(i), ccNodes(i), zhujiangParams)
        }

        zj.io.ci := 0.U
        zj.io.dft := DontCare
        zj.io.ramctl := DontCare
        val (zjMemAxi, _) = zhujiangMemMaster.get.out.head
        connectZJToAXI4(zj.ddrIO.head, zjMemAxi)
        zj.ddrIO.drop(1).foreach { axi => axi := DontCare }
        zj.cfgIO.zip(zhujiangCfgMasters).foreach { case (cfg, master) =>
          val (cfgAxi, _) = master.out.head
          connectZJToAXI4(cfg, cfgAxi)
        }
        zj.dmaIO.foreach { axi => axi := DontCare }
        zj.hwaIO.foreach { axi => axi := DontCare }

        core_with_l2.foreach(_.module.io.debugTopDown.l3MissMatch := false.B)
        core_with_l2.foreach(_.module.io.l3Miss := false.B)
      }
    }

    // tie off core soft reset
    for(node <- core_rst_nodes){
      node.out.head._1 := false.B.asAsyncReset
    }

    if (isOpenLLC) chi_openllc_opt match {
      case Some(l3) =>
        l3.io.debugTopDown.robHeadPaddr := core_with_l2.map(_.module.io.debugTopDown.robHeadPaddr)
        core_with_l2.zip(l3.io.debugTopDown.addrMatch).foreach { case (tile, l3Match) =>
          tile.module.io.debugTopDown.l3MissMatch := l3Match
        }
        core_with_l2.foreach(_.module.io.l3Miss := l3.io.l3Miss)
      case None =>
        core_with_l2.foreach(_.module.io.debugTopDown.l3MissMatch := false.B)
        core_with_l2.foreach(_.module.io.l3Miss := false.B)
    }

    core_with_l2.zipWithIndex.foreach { case (tile, i) =>
      tile.module.io.nodeID.foreach { case nodeID =>
        val chiNodeID = if (isZhuJiang) {
          zhujiangNocConfig.get.island.filter(_.nodeType == xijiang.NodeType.CC)(i).nodeId
        } else {
          i
        }
        nodeID := chiNodeID.U
        dontTouch(nodeID)
      }
    }

    misc.module.debug_module_io.resetCtrl.hartIsInReset := observedHartReset
    misc.module.debug_module_io.clock := io.clock
    misc.module.debug_module_io.reset := reset_sync

    misc.module.debug_module_io.debugIO.reset := misc.module.reset
    misc.module.debug_module_io.debugIO.clock := io.clock
    // TODO: delay 3 cycles?
    misc.module.debug_module_io.debugIO.dmactiveAck := misc.module.debug_module_io.debugIO.dmactive
    // jtag connector
    misc.module.debug_module_io.debugIO.systemjtag.foreach { x =>
      x.jtag        <> io.systemjtag.jtag
      x.reset       := jtag_reset_sync
      x.mfr_id      := io.systemjtag.mfr_id
      x.part_number := io.systemjtag.part_number
      x.version     := io.systemjtag.version
    }

    withClockAndReset(io.clock, reset_sync) {
      // Modules are reset one by one
      // reset ----> SYNC --> {SoCMisc, Cores}
      val resetChain = Seq(Seq(misc.module))
      ResetGen(resetChain, reset_sync, !debugOpts.ResetGen)
      // Ensure that cores could be reset when DM disable `hartReset`.
      val syncResetCores = misc.module.reset.asBool
      core_with_l2.zip(dmResetReqVec).zipWithIndex.foreach { case ((tile, dmResetReq), i) =>
        // Hart warm reset is a staged request, not a FIFO/socket flush.
        val selectedReq = if (EnableZhuJiangAsyncBridge) asyncCoreResets(i).asBool else dmResetReq
        ResetGen(Seq(Seq(tile.module)), (syncResetCores || selectedReq).asAsyncReset, !debugOpts.ResetGen)
      }
    }

  }

  lazy val module = new XSTopImp(this)
}

class XSTileDiffTop(implicit p: Parameters) extends XSTop {
  //TODO: need to keep the same module name as XSNoCDiffTop
  override lazy val desiredName: String = "XSTop"

  class XSTileDiffTopImp(wrapper: XSTop) extends XSTopImp(wrapper) with HasDiffTestInterfaces {
    override def cpuName: Option[String] = Some("XiangShan-KMHV2")
    override protected def implicitClock: Clock = io.clock
    // Difftest's wrapper drives this input; the derived systemReset must not
    // be returned here as a writable reset port. childReset uses systemReset.
    override protected def implicitReset: Reset = io.reset
    override def difftestMemIO: Option[DifftestMemIO] = Some(DifftestMemIO(memory))
  }
  override lazy val module = new XSTileDiffTopImp(this)
}

object TopMain extends App {
  val (config, firrtlOpts, firtoolOpts) = ArgParser.parse(args)

  // tools: init to close dpi-c when in fpga
  val envInFPGA = config(DebugOptionsKey).FPGAPlatform
  val enableDifftest = config(DebugOptionsKey).EnableDifftest || config(DebugOptionsKey).AlwaysBasicDiff
  val enableChiselDB = config(DebugOptionsKey).EnableChiselDB
  val enableConstantin = config(DebugOptionsKey).EnableConstantin
  Constantin.init(enableConstantin && !envInFPGA)
  ChiselDB.init(enableChiselDB && !envInFPGA)

  val topPrefix = config(SoCParamsKey).XSTopPrefix
  if (config(SoCParamsKey).UseXSNoCDiffTop) {
    if (enableDifftest) Gateway.setConfig("H") // use XMR to avoid extra topIO
    val soc = DisableMonitors(p => LazyModule(new XSNoCDiffTop()(p)))(config)
    Generator.execute(firrtlOpts, DifftestModule.top(soc.module, topPrefix), firtoolOpts)
  } else if (config(SoCParamsKey).UseXSTileDiffTop) {
    val soc = DisableMonitors(p => LazyModule(new XSTileDiffTop()(p)))(config)
    Generator.execute(firrtlOpts, DifftestModule.top(soc.module, topPrefix), firtoolOpts)
  } else {
    if (enableDifftest) {
      // TODO: Temporarily force XSTop to use internal DPI-C; will later split Top and Difftest like DiffTop
      Gateway.setConfig("U")
    }

    val soc = if (config(SoCParamsKey).UseXSNoCTop)
      DisableMonitors(p => LazyModule(new XSNoCTop()(p)))(config)
    else
      DisableMonitors(p => LazyModule(new XSTop()(p)))(config)

    Generator.execute(firrtlOpts, soc.module, firtoolOpts)

    // generate difftest bundles (w/o DifftestTopIO)
    if (enableDifftest) {
      DifftestModule.collect("XiangShan")
    }
  }

  FileRegisters.write(fileDir = "./build", filePrefix = "XSTop.")
}
