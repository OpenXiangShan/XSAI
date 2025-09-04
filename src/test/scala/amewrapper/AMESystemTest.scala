package amewrapper

import chisel3._
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.util._
import org.chipsalliance.cde.config._
import chisel3.stage.ChiselGeneratorAnnotation
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import huancun._
import coupledL2._
import coupledL2.prefetch._
import coupledL2.tl2tl._
import utility._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import freechips.rocketchip.tile.MaxHartIdBits
import AME._
import Expander.Uop_IO

class TestTop_AME_System(implicit p: Parameters) extends LazyModule {
override lazy val desiredName: String = "SimTop"
  val delayFactor = 0.2
  val cacheParams = p(L2ParamKey)

  // val io = IO(new TestTopIO)

  def createClientNode(name: String, sources: Int) = {
    val masterNode = TLClientNode(
      Seq(
        TLMasterPortParameters.v2(
          masters = Seq(
            TLMasterParameters.v1(
              name = name,
              sourceId = IdRange(0, sources),
              supportsProbe = TransferSizes(cacheParams.blockBytes)
            )
          ),
          channelBytes = TLChannelBeatBytes(cacheParams.blockBytes),
          minLatency = 1,
          echoFields = Nil,
          requestFields = Seq(AliasField(2), PrefetchField()),
          responseKeys = cacheParams.respKey
        )
      )
    )
    masterNode
  }

  val l1d = createClientNode(s"l1d", 32)
  val l1i = TLClientNode(
    Seq(
      TLMasterPortParameters.v1(
        clients = Seq(
          TLMasterParameters.v1(
            name = s"l1i",
            sourceId = IdRange(0, 32)
          )
        )
      )
    )
  )

  // System parameters
  val l2_banks = 8
  val l3_banks = 4
//   val cacheParams = p(L2ParamKey)
  val c_nodes = Seq(l1d)
  val l1i_nodes = Seq(l1i)
  // Create AME module
  val ame = LazyModule(new AMEModule()(p.alter((site, here, up) => {
    case AMEConfigKey => AMEParams(
      numTrRegs = 2,
      numAccRegs = 1,
      dataWidth = 32,
      matrixSize = 16
    )
    case L2ParamKey => L2Param(
      ways = 8,
      sets = 512,
      channelBytes = TLChannelBeatBytes(32),
      blockBytes = 64,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
      echoField = Seq(DirtyField())
    )
  })))


  // Create L2 Cache
  val l2 = LazyModule(new TL2TLCoupledL2()(p.alter((site, here, up) => {
    case MaxHartIdBits => 1
    case L2ParamKey => L2Param(
      name = s"l2",
      ways = 8,
      sets = 512,
      channelBytes = TLChannelBeatBytes(32),
      blockBytes = 64,
      clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
      echoField = Seq(DirtyField())
    )
    case huancun.BankBitsKey => log2Ceil(l2_banks)
        case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
  })))

  // Create L3 Cache
  val l3 = LazyModule(new HuanCun()(p.alter((site, here, up) => {
    case MaxHartIdBits => 1
        case LogUtilsOptionsKey => LogUtilsOptions(
      false,
      here(L2ParamKey).enablePerf,
      here(L2ParamKey).FPGAPlatform
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      here(L2ParamKey).enablePerf && !here(L2ParamKey).FPGAPlatform,
      here(L2ParamKey).enableRollingDB && !here(L2ParamKey).FPGAPlatform,
      XSPerfLevel.withName("VERBOSE"),
      0
    )
    case HCCacheParamsKey => HCCacheParameters(
      name = "l3",
      level = 3,
      ways = 16,
      sets = 4096,
      inclusive = false,
      clientCaches = Seq(
        CacheParameters(
          name = s"l2",
          sets = 4096,
          ways = 16,
          blockGranularity = log2Ceil(128)
        )
      ),
      echoField = Seq(DirtyField()),
      simulation = true
    )
  })))


  // Create memory
  val ram = LazyModule(new TLRAM(AddressSet(0, 0xff_ffffL), beatBytes = 32))

  // Create crossbars and bank binders
  val l1xbar = TLXbar()
  val l2xbar = TLXbar()
  val l3xbar = TLXbar()
  val l2bankBinders = BankBinder(l2_banks, 64)
  val l3bankBinders = BankBinder(l3_banks, 64)

    c_nodes.zipWithIndex.map {
    case (c, i) =>
      l1xbar := TLBuffer() := TLLogger(s"L2_L1D[${i}]", true) := c
  }

  l1i_nodes.zipWithIndex.map {
    case (ul, i) =>
      l1xbar := TLBuffer() := TLLogger(s"L2_L1I[${i}]", true) := ul
  }

  // Connect AME to L2
  ame.matrix_nodes.zipWithIndex.foreach { case (m, i) =>
    l1xbar := TLBuffer() := TLLogger(s"L2_Matrix[${i}]", true) := m
  }

  // Connect L2 to L3
  l2bankBinders :*= l2.node :*= TLBuffer() :*= l1xbar
  l3xbar :*= TLBuffer() :*= l2xbar :=* l2bankBinders

  // Connect L3 to RAM
  ram.node :=
    TLXbar() :=
    TLFragmenter(32, 64) :=
    TLCacheCork() :=
    TLClientsMerger() :=
    TLDelayer(delayFactor) :=
    TLLogger(s"MEM_L3", true) :=
    TLXbar() :=*
    l3bankBinders :*=
    l3.node :*=
    l3xbar

  class TestTop_AME_SystemImp(wrapper: LazyModule) extends LazyModuleImp(wrapper) {
    val uop_io = IO(new Uop_IO)
    val ameModule = ame.module
    val l2Module = l2.module

    l2Module.io.matrixDataOut512L2 <> ameModule.io.matrix_data_in

    // Default values for L2 control signals
    l2Module.io.hartId := 0.U
    l2Module.io.pfCtrlFromCore := 0.U.asTypeOf(new PrefetchCtrlFromCore)
    l2Module.io.debugTopDown := DontCare
    l2Module.io.l2_tlb_req := DontCare

    // Debug signals
    val timer = WireDefault(0.U(64.W))
    val logEnable = WireDefault(false.B)
    val clean = WireDefault(false.B)
    val dump = WireDefault(false.B)

    dontTouch(timer)
    dontTouch(logEnable)
    dontTouch(clean)
    dontTouch(dump)

    ameModule.io.Uop_io <> uop_io
    ameModule.io.writeAll := DontCare
    ameModule.io.readAll := DontCare
    ameModule.io.sigDone := DontCare

    c_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_port_$i"))
    }
    l1i_nodes.zipWithIndex.foreach {
      case (node, i) =>
        node.makeIOs()(ValName(s"master_ul_port_0_${i}"))
    }
    // ameModule.io.matrix_data_in := DontCare
    // success := Seq(l1d, l1i, ptw).map(_.module.finish).reduce(_&&_)
  }
  lazy val module = new TestTop_AME_SystemImp(this)
}

class AMESystemTest extends AnyFreeSpec with Matchers {
  "AME System should execute matrix operations correctly" in {
    val config = new Config((site, here, up) => {
      case AMEConfigKey => AMEParams(
        numTrRegs = 2,
        numAccRegs = 1,
        dataWidth = 32,
        matrixSize = 16
      )
      case L2ParamKey => L2Param(
        ways = 8,
        sets = 512,
        channelBytes = TLChannelBeatBytes(32),
        blockBytes = 64,
        clientCaches = Seq(L1Param(aliasBitsOpt = Some(2), vaddrBitsOpt = Some(16))),
        echoField = Seq(DirtyField())
      )
    })

    val top = DisableMonitors(p => LazyModule(new TestTop_AME_System()(p)))(config)
    simulate(top.module) { dut =>
      val ameModule = dut.ameModule

      // Initialize test data
      AMEModuleTestHelper.writeTestDataToAll(AMETestData.A, 0, ameModule)
      AMEModuleTestHelper.writeTestDataToAll(AMETestData.B, 1, ameModule)
      AMEModuleTestHelper.writeTestDataToAll(AMETestData.Ctmp, 4, ameModule)

      dut.clock.step(100) // Initial cycles

      // Test matrix load operation
      AMEModuleTestHelper.AMEStart(ameModule, 32, 32, 64, 0, 0, 0, 0x1000, 64, true.B, false.B, true.B)

      // Wait for ready
      while(!ameModule.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Wait for completion
      while(!ameModule.io.sigDone.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Test matrix computation
      AMEModuleTestHelper.AMEStart(ameModule, 32, 32, 64, 0, 1, 4, 0, 0, true.B, true.B, false.B)

      // Wait for completion
      while(!ameModule.io.sigDone.peek().litToBoolean) {
        dut.clock.step(1)
      }

      // Verify results
      AMEModuleTestHelper.readTestDataFromAll(AMETestData.C, 4, ameModule)

      dut.clock.step(100) // Final cycles
    }
  }
}

// Test Object for standalone execution
object TestTop_AME_System extends App {
  val config = new Config((site, here, up) => {
    case AMEConfigKey => AMEParams(
      numTrRegs = 2,
      numAccRegs = 1,
      dataWidth = 32,
      matrixSize = 16
    )
    case L2ParamKey => L2Param(
      ways = 8,
      sets = 512,
      channelBytes = TLChannelBeatBytes(32),
      blockBytes = 64
    )
  })

  val top = DisableMonitors(p => LazyModule(new TestTop_AME_System()(p)))(config)
  (new ChiselStage).execute(
    args,
    ChiselGeneratorAnnotation(() => top.module) +:
    TestTopAMEFirtoolOptions()
  )
}