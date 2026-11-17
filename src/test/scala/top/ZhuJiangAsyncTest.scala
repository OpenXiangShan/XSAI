package top

import chisel3._
import chisel3.util._
import freechips.rocketchip.amba.axi4.{AXI4AsyncCrossingSink, AXI4AsyncCrossingSource, AXI4Deinterleaver, AXI4Delayer, AXI4Fragmenter, AXI4IdentityNode, AXI4RAM}
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyRawModuleImp}
import freechips.rocketchip.tilelink.{TLFuzzer, TLRAMModel, TLToAXI4}
import freechips.rocketchip.util.AsyncQueueParams
import org.chipsalliance.cde.config.Parameters
import system.{HasSoCParameter, SoCParamsKey}
import xiangshan.DebugOptionsKey
import xijiang.NodeType
import xscache.chi.{CHIDataCheckKey, CHIPoisonKey, DecoupledPortIO}
import xs.utils.debug.{HardwareAssertionKey, HwaParams}
import zhujiang.{HasCHIToZhuJiangBridge, ZJParametersKey}
import zhujiang.chi.{DataFlit, ReqFlit, RespFlit, SnoopFlit}
import zhujiang.device.socket.SocketIcnSide

// Exercise the actual integration bridge and both native socket halves, not
// a FIFO model. This checks transport/field mapping, not CHI coherence.
class ZhuJiangSocketTest(socketType: String)(implicit p: Parameters)
  extends RawModule with HasCHIToZhuJiangBridge {
  val core_clock = IO(Input(Clock()))
  val noc_clock = IO(Input(Clock()))
  val core_reset = IO(Input(Bool()))
  val noc_reset = IO(Input(Bool()))
  val done = IO(Output(Bool()))
  val progressed = IO(Output(Bool()))
  val jointReset = (core_reset || noc_reset).asAsyncReset
  val node = p(ZJParametersKey).island.find(_.nodeType == NodeType.CC).get
  require(node.socket == socketType)
  val icn = withClockAndReset(noc_clock, jointReset) { Module(new SocketIcnSide(node)) }
  val chi = Wire(new DecoupledPortIO)
  withClockAndReset(core_clock, jointReset) {
    connectCHIToZhuJiang(chi, icn.io.socket, node, p)
  }

  val limit = 128
  def source[T <: Data](channel: DecoupledIO[T], clk: Clock)(fill: (T, UInt) => Unit): (UInt, Bool) = {
    withClockAndReset(clk, jointReset) {
      val count = RegInit(0.U(9.W))
      val blocked = RegInit(false.B)
      channel.valid := count < limit.U
      channel.bits := 0.U.asTypeOf(channel.bits)
      fill(channel.bits, count)
      when(channel.valid && !channel.ready) { blocked := true.B }
      when(channel.fire) { count := count + 1.U }
      (count, blocked)
    }
  }

  def sink[T <: Data](channel: DecoupledIO[T], clk: Clock)(check: (T, UInt) => Unit): UInt = {
    withClockAndReset(clk, jointReset) {
      val cycle = RegInit(0.U(16.W))
      val count = RegInit(0.U(9.W))
      cycle := cycle + 1.U
      // Fill every queue before draining, then repeatedly apply backpressure.
      channel.ready := cycle > 80.U && cycle % 11.U < 5.U
      when(channel.fire) {
        assert(count < limit.U, "duplicate/extra flit")
        check(channel.bits, count)
        count := count + 1.U
      }
      count
    }
  }

  def payload(count: UInt): UInt = Fill(32, count(7, 0))
  val txReq = source(chi.tx.req, core_clock) { (b, n) =>
    b.txnID := n
    b.addr := 0x80000000L.U + (n << 6)
    b.qos := n(3, 0)
  }
  val txRsp = source(chi.tx.rsp, core_clock) { (b, n) =>
    b.txnID := n
    b.dbID := n + 17.U
    b.qos := n(3, 0)
  }
  val txDat = source(chi.tx.dat, core_clock) { (b, n) =>
    b.txnID := n
    b.data := payload(n)
    b.be := Fill(32, true.B)
    b.dataID := n(1, 0)
  }
  val reqCount = sink(icn.io.dev.tx.req.get, noc_clock) { (raw, n) =>
    val b = raw.asTypeOf(new ReqFlit)
    assert(b.TxnID === n && b.QoS === n(3, 0))
    assert(b.Addr === 0x80000000L.U + (n << 6))
  }
  val rspCount = sink(icn.io.dev.tx.resp.get, noc_clock) { (raw, n) =>
    val b = raw.asTypeOf(new RespFlit)
    assert(b.TxnID === n && b.DBID === n + 17.U && b.QoS === n(3, 0))
  }
  val datCount = sink(icn.io.dev.tx.data.get, noc_clock) { (raw, n) =>
    val b = raw.asTypeOf(new DataFlit)
    assert(b.TxnID === n && b.Data === payload(n))
    assert(b.BE.andR && b.DataID === n(1, 0))
  }

  val rxRsp = source(icn.io.dev.rx.resp.get, noc_clock) { (raw, n) =>
    val b = Wire(new RespFlit)
    b := 0.U.asTypeOf(b)
    b.TxnID := n
    b.DBID := n + 23.U
    b.QoS := n(3, 0)
    raw := b.asTypeOf(raw)
  }
  val rxDat = source(icn.io.dev.rx.data.get, noc_clock) { (raw, n) =>
    val b = Wire(new DataFlit)
    b := 0.U.asTypeOf(b)
    b.TxnID := n
    b.Data := payload(n)
    b.BE := Fill(32, true.B)
    b.DataID := n(1, 0)
    raw := b.asTypeOf(raw)
  }
  val rxSnp = source(icn.io.dev.rx.snoop.get, noc_clock) { (raw, n) =>
    val b = Wire(new SnoopFlit)
    b := 0.U.asTypeOf(b)
    b.TxnID := n
    b.Addr := 0x10000000L.U + (n << 3)
    b.FwdNID := n
    b.RetToSrc := n(0)
    raw := b.asTypeOf(raw)
  }
  val rxRspCount = sink(chi.rx.rsp, core_clock) { (b, n) =>
    assert(b.txnID === n && b.dbID === n + 23.U && b.qos === n(3, 0))
  }
  val rxDatCount = sink(chi.rx.dat, core_clock) { (b, n) =>
    assert(b.txnID === n && b.data === payload(n))
    assert(b.be.andR && b.dataID === n(1, 0))
  }
  val rxSnpCount = sink(chi.rx.snp, core_clock) { (b, n) =>
    assert(b.txnID === n && b.addr === 0x10000000L.U + (n << 3))
    assert(b.fwdNID === n && b.retToSrc === n(0))
  }

  // The CC native socket also exposes REQ on ejection; the CPU bridge
  // intentionally backpressures it. Do not silently drop an unsupported flit.
  icn.io.dev.rx.req.foreach { channel =>
    channel.valid := false.B
    channel.bits := 0.U.asTypeOf(channel.bits)
  }
  icn.io.dev.rx.hpr.foreach { channel =>
    channel.valid := false.B
    channel.bits := 0.U.asTypeOf(channel.bits)
  }
  icn.io.dev.rx.debug.foreach { channel =>
    channel.valid := false.B
    channel.bits := 0.U.asTypeOf(channel.bits)
  }
  icn.io.dev.tx.snoop.foreach(_.ready := true.B)
  icn.io.dev.tx.hpr.foreach(_.ready := true.B)
  icn.io.dev.tx.debug.foreach(_.ready := true.B)
  val sent = Seq(txReq, txRsp, txDat, rxRsp, rxDat, rxSnp)
  val received = Seq(reqCount, rspCount, datCount, rxRspCount, rxDatCount, rxSnpCount)
  done := (sent.map(_._1) ++ received).map(_ === limit.U).reduce(_ && _) &&
    sent.map(_._2).reduce(_ && _)
  progressed := reqCount > 16.U && rxRspCount > 16.U
}

object ZhuJiangAsyncTestMain extends App {
  val topologyOnly = args.sameElements(Array("--topology-only"))
  require(topologyOnly || args.length == 2,
    "usage: ZhuJiangAsyncTestMain --topology-only | <sync|async> <output-dir>")
  val socketType = if (topologyOnly) "sync" else args(0)
  require(Set("sync", "async").contains(socketType))
  val out = if (topologyOnly) os.pwd else os.Path(args(1), os.pwd)
  val (base, _, _) = ArgParser.parse(Array(
    "--config", "DefaultMatrixConfig", "--issue", "E.b", "--num-cores", "2", "--llc", "ZhuJiang"
  ))
  for (n <- Seq(1, 2, 4)) {
    val sync = ZhuJiangNoCTopology(n, base(SoCParamsKey).ZhuJiangParams)
    assert(sync.island.count(_.nodeType == NodeType.CC) == n)
    assert(sync.island.filter(_.nodeType == NodeType.CC).forall(_.socket == "sync"))
    assert(sync.island.map(_.nodeId).distinct.size == sync.island.size)
    assert(sync.island.zipWithIndex.forall { case (node, index) =>
      node.nodeId == (index << sync.nodeAidBits)
    })
    if (n >= 4) {
      val dual = ZhuJiangNoCTopology(2, base(SoCParamsKey).ZhuJiangParams)
      assert(sync.nodeParams.take(dual.nodeParams.size) == dual.nodeParams)
      assert(sync.nodeParams.drop(dual.nodeParams.size).forall(_.nodeType == NodeType.CC))
    }
    val async = ZhuJiangNoCTopology(n, base(SoCParamsKey).ZhuJiangParams, 256, "async")
    assert(async.island.filter(_.nodeType == NodeType.CC).forall(_.socket == "async"))
    assert(async.nodeParams.filterNot(_.nodeType == NodeType.CC) ==
      sync.nodeParams.filterNot(_.nodeType == NodeType.CC))
    assert(async.island.map(_.nodeId) == sync.island.map(_.nodeId))
    assert(async.ciSuffix == sync.ciSuffix + "_cc_async")
    println(s"ZHUJIANG_TOPOLOGY_CHECK cores=$n CC=" +
      sync.island.filter(_.nodeType == NodeType.CC).map(node => s"0x${node.nodeId.toHexString}").mkString(","))
  }
  for (bad <- Seq(() => ZhuJiangNoCTopology(3), () => ZhuJiangNoCTopology(5),
    () => ZhuJiangNoCTopology(6), () =>
    ZhuJiangNoCTopology(2, base(SoCParamsKey).ZhuJiangParams, 256, "c2c"))) {
    try { bad(); sys.error("invalid topology accepted") }
    catch { case _: IllegalArgumentException => () }
  }
  class CheckSoC(implicit val p: Parameters) extends HasSoCParameter
  val (enabled, _, _) = ArgParser.parse(Array(
    "--config", "DefaultMatrixConfig", "--issue", "E.b", "--llc", "ZhuJiang", "--zhujiang-async"
  ))
  assert(enabled(SoCParamsKey).EnableZhuJiangAsyncBridge)
  assert(enabled(SoCParamsKey).EnableCHIAsyncBridge.isEmpty)
  assert(!base(SoCParamsKey).EnableZhuJiangAsyncBridge)
  new CheckSoC()(enabled)
  for (llc <- Seq("OpenLLC", "NoLLC")) {
    try {
      new CheckSoC()(enabled.alter(LLCConfig(llc)))
      sys.error(s"$llc accepted Zhujiang async flag")
    } catch { case _: IllegalArgumentException => () }
  }
  for (noCTop <- Seq(false, true)) {
    val unsupported = enabled.alterPartial {
      case SoCParamsKey => enabled(SoCParamsKey).copy(UseXSNoCTop = noCTop, UseXSNoCDiffTop = !noCTop)
    }
    try {
      new CheckSoC()(unsupported)
      sys.error("XSNoCTop/XSNoCDiffTop accepted Zhujiang async flag")
    } catch { case _: IllegalArgumentException => () }
  }

  if (topologyOnly) {
    println("ZHUJIANG_TOPOLOGY_ONLY_PASS")
    sys.exit(0)
  }

  implicit val p: Parameters = base.alterPartial {
    case ZJParametersKey => ZhuJiangNoCTopology(2, base(SoCParamsKey).ZhuJiangParams, 256, socketType)
    case HardwareAssertionKey => HwaParams(enable = false)
    case CHIDataCheckKey => "none"
    case CHIPoisonKey => false
  }
  Generator.execute(
    Array("--target", "systemverilog", "--target-dir", out.toString),
    new ZhuJiangSocketTest(socketType),
    Array("--disable-annotation-unknown")
  )
  os.write.over(out / "tb.sv", s"""
`timescale 1ns/1ps
module ZhuJiangAsyncTestBench;
  reg core_clock = 0, noc_clock = 0;
  reg core_reset = 1, noc_reset = 1;
  wire done, progressed;
  integer core_half = 10, noc_half = ${if (socketType == "sync") 10 else 37};
  integer phase;
  ZhuJiangSocketTest dut(.*);
  initial begin
    void'($$value$$plusargs("CORE_HALF=%d", core_half));
    forever #(core_half) core_clock = ~core_clock;
  end
  initial begin
    void'($$value$$plusargs("NOC_HALF=%d", noc_half));
    #3;
    forever #(noc_half) noc_clock = ~noc_clock;
  end
  initial begin
    // sync reference must use exactly the same clock, not a second phase.
    ${if (socketType == "sync") "force noc_clock = core_clock;" else ""}
    #1000 core_reset = 0;
    #501 noc_reset = 0;
    wait(progressed);
    // NoC-only reset request must cold-reset BOTH endpoints with live traffic.
    #7 noc_reset = 1;
    #1000 noc_reset = 0;
    wait(done);
    // Coordinated restart from empty with asymmetric release.
    #100 core_reset = 1; noc_reset = 1;
    #1000 noc_reset = 0;
    #257 core_reset = 0;
    wait(done);
    #1000;
    $$display("ZHUJIANG_SOCKET_PASS mode=$socketType channels=6 transactions_per_channel=128 resets=2 core_half=%0d noc_half=%0d", core_half, noc_half);
    $$finish;
  end
  initial begin
    #10000000;
    $$fatal(1, "socket scoreboard/reset/backpressure timeout");
  end
endmodule
""")
  println(s"ZHUJIANG_SOCKET_GENERATED mode=$socketType out=$out; configuration positives/negatives PASS")
}

// Reuse RocketChip's fuzzer, RAM scoreboard, burst fragmenter and random
// backpressure. The CDC is the exact source/sink pair used by XSTop.
class ZhuJiangAxiCrossingTest(beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val fuzz = LazyModule(new TLFuzzer(512))
  val model = LazyModule(new TLRAMModel("ZhujiangNoCAxi"))
  val toAxi = LazyModule(new TLToAXI4)
  val monitor = AXI4IdentityNode()
  val source = LazyModule(new AXI4AsyncCrossingSource())
  val sink = LazyModule(new AXI4AsyncCrossingSink(AsyncQueueParams(depth = 8, sync = 3, safe = true)))
  val deinterleave = LazyModule(new AXI4Deinterleaver(64))
  val delay = LazyModule(new AXI4Delayer(0.4))
  val fragment = LazyModule(new AXI4Fragmenter())
  val ram = LazyModule(new AXI4RAM(AddressSet(0, 0xfff), beatBytes = beatBytes))
  model.node := fuzz.node
  toAxi.node := model.node
  monitor := toAxi.node
  source.node := monitor
  sink.node := source.node
  // Fragmentation removes the downstream no-interleaving guarantee required
  // by TLToAXI4; restore it with the native adapter, not a test-only waiver.
  ram.node := fragment.node := delay.node := deinterleave.node := sink.node

  lazy val module = new LazyRawModuleImp(this) {
    val noc_clock = IO(Input(Clock()))
    val core_clock = IO(Input(Clock()))
    val reset = IO(Input(AsyncReset()))
    val done = IO(Output(Bool()))
    val burstSeen = IO(Output(Bool()))
    childClock := noc_clock
    childReset := reset
    for (mod <- Seq(sink.module, deinterleave.module, delay.module, fragment.module, ram.module)) {
      mod.clock := core_clock
      mod.reset := reset
    }
    val axi = monitor.in.head._1
    val burst = withClockAndReset(noc_clock, reset) { RegInit(false.B) }
    withClockAndReset(noc_clock, reset) {
      when((axi.aw.fire && axi.aw.bits.len =/= 0.U) ||
           (axi.ar.fire && axi.ar.bits.len =/= 0.U)) { burst := true.B }
    }
    burstSeen := burst
    done := fuzz.module.io.finished && burst
  }
}

object ZhuJiangAxiTestMain extends App {
  require(args.length == 2, "usage: ZhuJiangAxiTestMain <beat-bytes> <output-dir>")
  val bytes = args(0).toInt
  require(Set(8, 32).contains(bytes))
  val out = os.Path(args(1), os.pwd)
  implicit val p: Parameters = Parameters.empty
  val top = LazyModule(new ZhuJiangAxiCrossingTest(bytes))
  Generator.execute(Array("--target", "systemverilog", "--split-verilog", "--target-dir", out.toString),
    top.module, Array("--disable-annotation-unknown"))
  os.write.over(out / "tb.sv", s"""
`timescale 1ns/1ps
module ZhuJiangAxiTestBench;
  reg core_clock = 0, noc_clock = 0, reset = 1;
  wire done, burstSeen;
  integer core_half = 10, noc_half = 37;
  ZhuJiangAxiCrossingTest dut(.*);
  initial begin
    void'($$value$$plusargs("CORE_HALF=%d", core_half));
    forever #(core_half) core_clock = ~core_clock;
  end
  initial begin
    void'($$value$$plusargs("NOC_HALF=%d", noc_half));
    #3; forever #(noc_half) noc_clock = ~noc_clock;
  end
  initial begin
    #1501 reset = 0;
    wait(done);
    #1000;
    $$display("ZHUJIANG_AXI_PASS beat_bytes=$bytes operations=512 burst=1 core_half=%0d noc_half=%0d", core_half, noc_half);
    $$finish;
  end
  initial begin #100000000; $$fatal(1, "AXI data/burst scoreboard timeout"); end
endmodule
""")
  println(s"ZHUJIANG_AXI_GENERATED beat_bytes=$bytes out=$out")
}
