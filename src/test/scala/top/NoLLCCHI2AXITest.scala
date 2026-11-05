package top

import chisel3._
import chisel3.util._
import chiseltest._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xscache.chi._
import xscache.openLLC.TransmitterLinkMonitor
import xiangshan.XSTileKey

import scala.collection.mutable

class NoLLCCHI2AXITestHarness(implicit p: Parameters) extends LazyModule {
  val entries: Int = p(NoLLCEntries)
  val bridge = LazyModule(new NoLLCCHI2AXI(entries = entries))
  val memory = AXI4SlaveNode(Seq(AXI4SlavePortParameters(
    slaves = Seq(AXI4SlaveParameters(
      address = Seq(AddressSet(0x80000000L, 0x7fffffffL)),
      supportsRead = TransferSizes(1, 64),
      supportsWrite = TransferSizes(1, 64),
      interleavedId = Some(0)
    )),
    beatBytes = 32,
    minLatency = 1
  )))

  memory := bridge.axi4node

  lazy val module = new NoLLCCHI2AXITestHarnessImp(this)
}

class NoLLCCHI2AXITestHarnessImp(wrapper: NoLLCCHI2AXITestHarness)(implicit p: Parameters)
  extends LazyModuleImp(wrapper) {
    val (mem, edge) = wrapper.memory.in.head
    val io = IO(new Bundle {
      val chi = Flipped(new DecoupledPortIO)
      val axi = new Bundle {
        val aw = Decoupled(new AXI4BundleAW(edge.bundle))
        val w = Decoupled(new AXI4BundleW(edge.bundle))
        val b = Flipped(Decoupled(new AXI4BundleB(edge.bundle)))
        val ar = Decoupled(new AXI4BundleAR(edge.bundle))
        val r = Flipped(Decoupled(new AXI4BundleR(edge.bundle)))
      }
      val occupancy = Output(UInt(log2Ceil(wrapper.entries + 1).W))
      val accepted = Output(UInt(32.W))
    })

    val transmitter = Module(new TransmitterLinkMonitor)
    transmitter.io.in.tx.req.valid := io.chi.tx.req.valid
    transmitter.io.in.tx.req.bits := io.chi.tx.req.bits
    io.chi.tx.req.ready := transmitter.io.in.tx.req.ready
    transmitter.io.in.tx.rsp.valid := io.chi.tx.rsp.valid
    transmitter.io.in.tx.rsp.bits := io.chi.tx.rsp.bits
    io.chi.tx.rsp.ready := transmitter.io.in.tx.rsp.ready
    transmitter.io.in.tx.dat.valid := io.chi.tx.dat.valid
    transmitter.io.in.tx.dat.bits := io.chi.tx.dat.bits
    io.chi.tx.dat.ready := transmitter.io.in.tx.dat.ready
    io.chi.rx.rsp.valid := transmitter.io.in.rx.rsp.valid
    io.chi.rx.rsp.bits := transmitter.io.in.rx.rsp.bits
    transmitter.io.in.rx.rsp.ready := io.chi.rx.rsp.ready
    io.chi.rx.dat.valid := transmitter.io.in.rx.dat.valid
    io.chi.rx.dat.bits := transmitter.io.in.rx.dat.bits
    transmitter.io.in.rx.dat.ready := io.chi.rx.dat.ready
    io.chi.rx.snp.valid := transmitter.io.in.rx.snp.valid
    io.chi.rx.snp.bits := transmitter.io.in.rx.snp.bits
    transmitter.io.in.rx.snp.ready := io.chi.rx.snp.ready
    transmitter.io.out <> wrapper.bridge.module.io.rn
    wrapper.bridge.module.io.nodeID := 2.U
    io.occupancy := wrapper.bridge.module.io.occupancy
    io.accepted := wrapper.bridge.module.io.accepted

    io.axi.aw.valid := mem.aw.valid
    io.axi.aw.bits := mem.aw.bits
    mem.aw.ready := io.axi.aw.ready
    io.axi.w.valid := mem.w.valid
    io.axi.w.bits := mem.w.bits
    mem.w.ready := io.axi.w.ready
    mem.b.valid := io.axi.b.valid
    mem.b.bits := io.axi.b.bits
    io.axi.b.ready := mem.b.ready
    io.axi.ar.valid := mem.ar.valid
    io.axi.ar.bits := mem.ar.bits
    mem.ar.ready := io.axi.ar.ready
    mem.r.valid := io.axi.r.valid
    mem.r.bits := io.axi.r.bits
    io.axi.r.ready := mem.r.ready
}

class NoLLCCHI2AXITest extends AnyFlatSpec with ChiselScalatestTester with Matchers with HasCHIOpcodes {
  behavior of "NoLLCCHI2AXI"

  override implicit lazy val p: Parameters = (new DefaultConfig(1)).alterPartial {
    case NoLLCEntries => 64
    case CHIIssue => Issue.Eb
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }

  private def init(dut: NoLLCCHI2AXITestHarnessImp): Unit = {
    dut.io.chi.tx.req.valid.poke(false.B)
    dut.io.chi.tx.rsp.valid.poke(false.B)
    dut.io.chi.tx.dat.valid.poke(false.B)
    dut.io.chi.rx.rsp.ready.poke(true.B)
    dut.io.chi.rx.dat.ready.poke(true.B)
    dut.io.chi.rx.snp.ready.poke(true.B)
    dut.io.axi.aw.ready.poke(true.B)
    dut.io.axi.w.ready.poke(true.B)
    dut.io.axi.ar.ready.poke(true.B)
    dut.io.axi.b.valid.poke(false.B)
    dut.io.axi.r.valid.poke(false.B)
    dut.clock.step(20)
  }

  private def pokeReq(
    dut: NoLLCCHI2AXITestHarnessImp,
    opcode: UInt,
    txnID: Int,
    addr: BigInt,
    size: Int,
    order: UInt = OrderEncodings.None,
    ewa: Boolean = true,
    expCompAck: Boolean = false
  ): Unit = {
    val req = dut.io.chi.tx.req.bits
    req.qos.poke(0.U)
    req.tgtID.poke(2.U)
    req.srcID.poke(1.U)
    req.txnID.poke(txnID.U)
    req.returnNID.poke(0.U)
    req.stashNIDValid.poke(false.B)
    req.returnTxnID.poke(0.U)
    req.opcode.poke(opcode)
    req.size.poke(size.U)
    req.addr.poke(addr.U)
    req.ns.poke(false.B)
    req.likelyshared.poke(false.B)
    req.allowRetry.poke(true.B)
    req.order.poke(order)
    req.pCrdType.poke(0.U)
    req.memAttr.allocate.poke(false.B)
    req.memAttr.cacheable.poke(false.B)
    req.memAttr.device.poke(false.B)
    req.memAttr.ewa.poke(ewa.B)
    req.snpAttr.poke(false.B)
    req.lpIDWithPadding.poke(0.U)
    req.snoopMe.poke(false.B)
    req.expCompAck.poke(expCompAck.B)
    req.tagOp.foreach(_.poke(0.U))
    req.traceTag.poke(false.B)
    req.mpam.foreach { mpam =>
      mpam.perfMonGroup.poke(0.U)
      mpam.partID.poke(0.U)
      mpam.mpamNS.poke(false.B)
    }
    req.rsvdc.poke(0.U)
  }

  private def sendReq(
    dut: NoLLCCHI2AXITestHarnessImp,
    opcode: UInt,
    txnID: Int,
    addr: BigInt,
    size: Int,
    order: UInt = OrderEncodings.None,
    ewa: Boolean = true,
    expCompAck: Boolean = false
  ): Unit = {
    pokeReq(dut, opcode, txnID, addr, size, order, ewa, expCompAck)
    dut.io.chi.tx.req.valid.poke(true.B)
    var cycles = 0
    while (!dut.io.chi.tx.req.ready.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    dut.clock.step()
    dut.io.chi.tx.req.valid.poke(false.B)
  }

  private def pokeData(
    dut: NoLLCCHI2AXITestHarnessImp,
    opcode: UInt,
    dbID: BigInt,
    dataID: Int,
    data: BigInt,
    be: BigInt
  ): Unit = {
    val dat = dut.io.chi.tx.dat.bits
    dat.qos.poke(0.U)
    dat.tgtID.poke(2.U)
    dat.srcID.poke(1.U)
    dat.txnID.poke(dbID.U)
    dat.homeNID.poke(0.U)
    dat.opcode.poke(opcode)
    dat.respErr.poke(RespErrEncodings.OK)
    dat.resp.poke(0.U)
    dat.dataSource.poke(0.U)
    dat.cBusy.foreach(_.poke(0.U))
    dat.dbID.poke(0.U)
    dat.ccID.poke(0.U)
    dat.dataID.poke(dataID.U)
    dat.tagOp.foreach(_.poke(0.U))
    dat.tag.foreach(_.poke(0.U))
    dat.tu.foreach(_.poke(0.U))
    dat.traceTag.poke(false.B)
    dat.rsvdc.poke(0.U)
    dat.be.poke(be.U)
    dat.data.poke(data.U)
    dat.dataCheck.foreach { check =>
      val parity = (0 until 32).foldLeft(BigInt(0)) { case (result, i) =>
        val byte = (data >> (i * 8)) & 0xff
        val bit = if ((byte.bitCount & 1) == 0) BigInt(1) else BigInt(0)
        result | (bit << i)
      }
      check.poke(parity.U)
    }
    dat.poison.foreach(_.poke(0.U))
  }

  private def sendData(
    dut: NoLLCCHI2AXITestHarnessImp,
    opcode: UInt,
    dbID: BigInt,
    dataID: Int,
    data: BigInt,
    be: BigInt
  ): Unit = {
    pokeData(dut, opcode, dbID, dataID, data, be)
    dut.io.chi.tx.dat.valid.poke(true.B)
    var cycles = 0
    while (!dut.io.chi.tx.dat.ready.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    dut.clock.step()
    dut.io.chi.tx.dat.valid.poke(false.B)
  }

  private def takeRsp(dut: NoLLCCHI2AXITestHarnessImp): (BigInt, BigInt, BigInt) = {
    dut.io.chi.rx.rsp.ready.poke(false.B)
    var cycles = 0
    while (!dut.io.chi.rx.rsp.valid.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    val result = (
      dut.io.chi.rx.rsp.bits.opcode.peek().litValue,
      dut.io.chi.rx.rsp.bits.txnID.peek().litValue,
      dut.io.chi.rx.rsp.bits.dbID.peek().litValue
    )
    dut.io.chi.rx.rsp.ready.poke(true.B)
    dut.clock.step()
    result
  }

  private def takeDat(dut: NoLLCCHI2AXITestHarnessImp): (BigInt, BigInt, BigInt) = {
    dut.io.chi.rx.dat.ready.poke(true.B)
    var cycles = 0
    while (!dut.io.chi.rx.dat.valid.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    val result = (
      dut.io.chi.rx.dat.bits.txnID.peek().litValue,
      dut.io.chi.rx.dat.bits.dbID.peek().litValue,
      dut.io.chi.rx.dat.bits.dataID.peek().litValue
    )
    dut.clock.step()
    result
  }

  private def sendReadBeat(
    dut: NoLLCCHI2AXITestHarnessImp,
    id: BigInt,
    data: BigInt,
    last: Boolean
  ): Unit = {
    dut.io.axi.r.bits.id.poke(id.U)
    dut.io.axi.r.bits.data.poke(data.U)
    dut.io.axi.r.bits.resp.poke(AXI4Parameters.RESP_OKAY)
    dut.io.axi.r.bits.last.poke(last.B)
    dut.io.axi.r.valid.poke(true.B)
    var cycles = 0
    while (!dut.io.axi.r.ready.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    dut.clock.step()
    dut.io.axi.r.valid.poke(false.B)
  }

  private def sendB(dut: NoLLCCHI2AXITestHarnessImp, id: BigInt): Unit = {
    dut.io.axi.b.bits.id.poke(id.U)
    dut.io.axi.b.bits.resp.poke(AXI4Parameters.RESP_OKAY)
    dut.io.axi.b.valid.poke(true.B)
    var cycles = 0
    while (!dut.io.axi.b.ready.peek().litToBoolean && cycles < 100) {
      dut.clock.step()
      cycles += 1
    }
    assert(cycles < 100)
    dut.clock.step()
    dut.io.axi.b.valid.poke(false.B)
  }

  private def waitUntil(dut: NoLLCCHI2AXITestHarnessImp)(condition: => Boolean): Unit = {
    var cycles = 0
    while (!condition && cycles < 200) {
      dut.clock.step()
      cycles += 1
    }
    assert(condition, "timed out waiting for bridge progress")
  }

  private def externalAXIID(slot: BigInt, entries: Int): BigInt = slot % math.max(1, entries / 4)

  private def sendCompAck(dut: NoLLCCHI2AXITestHarnessImp, dbID: BigInt): Unit = {
    val ack = dut.io.chi.tx.rsp.bits
    ack.qos.poke(0.U)
    ack.tgtID.poke(2.U)
    ack.srcID.poke(1.U)
    ack.txnID.poke(dbID.U)
    ack.opcode.poke(CompAck)
    ack.respErr.poke(0.U)
    ack.resp.poke(0.U)
    ack.fwdState.poke(0.U)
    ack.cBusy.foreach(_.poke(0.U))
    ack.dbID.poke(0.U)
    ack.pCrdType.poke(0.U)
    ack.tagOp.foreach(_.poke(0.U))
    ack.traceTag.poke(false.B)
    dut.io.chi.tx.rsp.valid.poke(true.B)
    waitUntil(dut)(dut.io.chi.tx.rsp.ready.peek().litToBoolean)
    dut.clock.step()
    dut.io.chi.tx.rsp.valid.poke(false.B)
  }

  // Keep the eight-bank, 128-outstanding setup local to the bridge throughput tests.
  private lazy val writePerfConfig: Parameters = (new MinimalMatrixConfig(1)).alter((site, here, up) => {
    case NoLLCEntries => 128
    case XSTileKey => up(XSTileKey).map(p => p.copy(
      L2CacheParamsOpt = p.L2CacheParamsOpt.map(_.copy(sets = 128)),
      L2NBanks = 8
    ))
    case CHIIssue => Issue.Eb
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  })

  it should "buffer multiple granted writes with reversed data and independently stalled AXI channels" in {
    test(LazyModule(new NoLLCCHI2AXITestHarness()(writePerfConfig)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.chi.rx.rsp.ready.poke(false.B)
      dut.io.axi.aw.ready.poke(false.B)
      dut.io.axi.w.ready.poke(false.B)
      val address = BigInt("80020000", 16)
      val byteEnables = BigInt("ffffffff", 16)
      for (i <- 0 until 8) sendReq(dut, WriteBackFull, i, address + i * 64, 6)
      val grants = Seq.fill(8)(takeRsp(dut))
      grants.map(_._2).distinct.size shouldBe 8
      grants.foreach(_._1 shouldBe CompDBIDResp.litValue)
      for ((_, txnID, dbID) <- grants.reverse) {
        sendData(dut, CopyBackWrData, dbID, 2, txnID * 2 + 2, byteEnables)
        sendData(dut, CopyBackWrData, dbID, 0, txnID * 2 + 1, byteEnables)
      }
      for ((_, txnID, dbID) <- grants) {
        val axiID = externalAXIID(dbID, writePerfConfig(NoLLCEntries))
        waitUntil(dut)(dut.io.axi.aw.valid.peek().litToBoolean && dut.io.axi.w.valid.peek().litToBoolean)
        // Other buffers filling and stalled channels must not change the head packet.
        for (_ <- 0 until 5) {
          dut.io.axi.aw.bits.id.expect(axiID.U)
          dut.io.axi.aw.bits.addr.expect((address + txnID * 64).U)
          dut.io.axi.aw.bits.len.expect(1.U)
          dut.io.axi.w.bits.data.expect((txnID * 2 + 1).U)
          dut.io.axi.w.bits.strb.expect(byteEnables.U)
          dut.io.axi.w.bits.last.expect(false.B)
          dut.clock.step()
        }
        dut.io.axi.w.ready.poke(true.B)
        dut.clock.step()
        dut.io.axi.w.valid.expect(true.B)
        dut.io.axi.w.bits.data.expect((txnID * 2 + 2).U)
        dut.io.axi.w.bits.last.expect(true.B)
        dut.clock.step()
        dut.io.axi.w.ready.poke(false.B)
        // W may finish before AW; keep ownership until the delayed address handshake.
        for (_ <- 0 until 3) {
          dut.io.axi.aw.valid.expect(true.B)
          dut.io.axi.aw.bits.id.expect(axiID.U)
          dut.io.axi.aw.bits.addr.expect((address + txnID * 64).U)
          dut.io.axi.w.valid.expect(false.B)
          dut.clock.step()
        }
        dut.io.axi.aw.ready.poke(true.B)
        dut.clock.step()
        dut.io.axi.aw.ready.poke(false.B)
      }
      dut.io.occupancy.expect(8.U)
      grants.reverse.foreach { case (_, _, dbID) => sendB(dut, externalAXIID(dbID, writePerfConfig(NoLLCEntries))) }
      waitUntil(dut)(dut.io.occupancy.peek().litValue == 0)
    }
  }

  it should "retire mixed writes with simultaneous last-beat B and delayed CompAck in a two-slot bridge" in {
    val config = writePerfConfig.alterPartial { case NoLLCEntries => 2 }
    test(LazyModule(new NoLLCCHI2AXITestHarness()(config)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val byteEnables = BigInt("ffffffff", 16)
      for (full <- Seq(false, true)) {
        val address = BigInt("80030000", 16)
        val noSnpAddress = address + 64 + (if (full) 0 else 40)
        dut.io.chi.rx.rsp.ready.poke(false.B)
        dut.io.axi.aw.ready.poke(false.B)
        dut.io.axi.w.ready.poke(false.B)
        sendReq(dut, WriteBackFull, 10, address, 6, expCompAck = true)
        sendReq(dut, if (full) WriteNoSnpFull else WriteNoSnpPtl, 11, noSnpAddress,
          if (full) 6 else 3, ewa = false)
        val grants = Seq.fill(2)(takeRsp(dut))
        dut.io.chi.rx.rsp.ready.poke(false.B)
        val coherentID = grants.find(_._2 == 10).get._3
        val noSnpID = grants.find(_._2 == 11).get._3
        grants.find(_._2 == 10).get._1 shouldBe CompDBIDResp.litValue
        grants.find(_._2 == 11).get._1 shouldBe DBIDResp.litValue
        if (full) {
          sendData(dut, NonCopyBackWrData, noSnpID, 2, 23, byteEnables)
          sendData(dut, NonCopyBackWrData, noSnpID, 0, 22, byteEnables)
        } else {
          sendData(dut, NonCopyBackWrData, noSnpID, 2, BigInt(22) << 64, 0xff00)
        }
        sendData(dut, CopyBackWrData, coherentID, 2, 21, byteEnables)
        sendData(dut, CopyBackWrData, coherentID, 0, 20, byteEnables)
        for ((_, txnID, dbID) <- grants) {
          val twoBeats = txnID == 10 || full
          val axiID = externalAXIID(dbID, config(NoLLCEntries))
          waitUntil(dut)(dut.io.axi.aw.valid.peek().litToBoolean && dut.io.axi.w.valid.peek().litToBoolean)
          dut.io.axi.aw.bits.id.expect(axiID.U)
          dut.io.axi.aw.bits.addr.expect((if (txnID == 10) address else noSnpAddress).U)
          dut.io.axi.aw.bits.size.expect((if (twoBeats) 5 else 3).U)
          dut.io.axi.aw.bits.len.expect((if (twoBeats) 1 else 0).U)
          dut.io.axi.aw.ready.poke(true.B)
          // The response-ID queue needs the address at least one cycle before B.
          dut.clock.step()
          dut.io.axi.aw.ready.poke(false.B)
          dut.io.axi.w.ready.poke(true.B)
          if (twoBeats) {
            dut.io.axi.w.bits.data.expect((txnID * 2).U)
            dut.io.axi.w.bits.strb.expect(byteEnables.U)
            dut.io.axi.w.bits.last.expect(false.B)
            dut.clock.step()
          }
          dut.io.axi.w.valid.expect(true.B)
          dut.io.axi.w.bits.data.expect((if (twoBeats) txnID * 2 + 1 else BigInt(22) << 64).U)
          dut.io.axi.w.bits.strb.expect((if (twoBeats) byteEnables else BigInt(0xff00)).U)
          dut.io.axi.w.bits.last.expect(true.B)
          dut.io.axi.b.bits.id.poke(axiID.U)
          dut.io.axi.b.bits.resp.poke(AXI4Parameters.RESP_OKAY)
          dut.io.axi.b.valid.poke(true.B)
          dut.io.axi.b.ready.expect(true.B)
          dut.clock.step()
          dut.io.axi.b.valid.poke(false.B)
          dut.io.axi.aw.ready.poke(false.B)
          dut.io.axi.w.ready.poke(false.B)
        }
        val (opcode, txnID, _) = takeRsp(dut)
        opcode shouldBe Comp.litValue
        txnID shouldBe 11
        dut.clock.step(5)
        dut.io.occupancy.expect(1.U)
        sendCompAck(dut, coherentID)
        waitUntil(dut)(dut.io.occupancy.peek().litValue == 0)
      }
    }
  }

  private def runWriteThroughput(backpressure: Boolean): Unit = {
    writePerfConfig(XSTileKey).head.L2NBanks shouldBe 8
    writePerfConfig(XSTileKey).head.L2CacheParamsOpt.get.mshrs * 8 shouldBe 128
    writePerfConfig(NoLLCEntries) shouldBe 128
    val harness = LazyModule(new NoLLCCHI2AXITestHarness()(writePerfConfig))
    test(harness.module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val transactions = 128
      val address = BigInt("80010000", 16)
      val byteEnables = BigInt("ffffffff", 16)
      val dataPending = mutable.Queue.empty[(BigInt, BigInt, Int)]
      val writesPending = mutable.Queue.empty[BigInt]
      val random = new scala.util.Random(0x4c4c43L)
      var dataActive = false
      var stalledAW = Option.empty[(BigInt, BigInt)]
      var stalledW = Option.empty[(BigInt, BigInt, Boolean)]
      val issuedIDs = mutable.ArrayBuffer.empty[BigInt]
      val beatCycles = mutable.ArrayBuffer.empty[Int]
      val dataCycles = mutable.ArrayBuffer.empty[Int]
      var dataStalls = 0
      var dataStarvation = 0
      var requests = 0
      var responses = 0
      var cycle = 0
      var outputBeat = 0
      while (beatCycles.size < transactions * 2 && cycle < 10000) {
        if (requests < transactions) {
          pokeReq(dut, WriteBackFull, requests, address + requests * 64, 6)
        }
        dut.io.chi.tx.req.valid.poke((requests < transactions).B)
        val awReady = !backpressure || random.nextInt(4) != 0
        val rspReady = !backpressure || random.nextInt(4) != 0
        val wReady = (!backpressure || random.nextInt(4) != 0) &&
          (writesPending.nonEmpty || dut.io.axi.aw.valid.peek().litToBoolean && awReady)
        dut.io.axi.aw.ready.poke(awReady.B)
        dut.io.axi.w.ready.poke(wReady.B)
        dut.io.chi.rx.rsp.ready.poke(rspReady.B)
        if (!dataActive && dataPending.nonEmpty && (!backpressure || random.nextInt(4) != 0)) {
          dataActive = true
        }
        val sending = if (dataActive) dataPending.headOption else None
        if (sending.isEmpty && responses < transactions) {
          dataStarvation += 1
        }
        sending.foreach { case (dbID, txnID, beat) =>
          pokeData(dut, CopyBackWrData, dbID, beat * 2, txnID * 2 + beat + 1, byteEnables)
        }
        dut.io.chi.tx.dat.valid.poke(sending.nonEmpty.B)
        if (dut.io.chi.tx.req.valid.peek().litToBoolean && dut.io.chi.tx.req.ready.peek().litToBoolean) {
          requests += 1
        }
        if (dut.io.chi.rx.rsp.valid.peek().litToBoolean && rspReady) {
          dut.io.chi.rx.rsp.bits.opcode.expect(CompDBIDResp)
          val txnID = dut.io.chi.rx.rsp.bits.txnID.peek().litValue
          val dbID = dut.io.chi.rx.rsp.bits.dbID.peek().litValue
          dataPending.enqueue((dbID, txnID, 0), (dbID, txnID, 1))
          responses += 1
        }
        if (sending.nonEmpty && dut.io.chi.tx.dat.ready.peek().litToBoolean) {
          dataPending.dequeue()
          dataCycles += cycle
          dataActive = false
        } else if (sending.nonEmpty) {
          dataStalls += 1
        }
        val awValid = dut.io.axi.aw.valid.peek().litToBoolean
        val wValid = dut.io.axi.w.valid.peek().litToBoolean
        val awPacket = (dut.io.axi.aw.bits.id.peek().litValue, dut.io.axi.aw.bits.addr.peek().litValue)
        val wPacket = (dut.io.axi.w.bits.data.peek().litValue, dut.io.axi.w.bits.strb.peek().litValue,
          dut.io.axi.w.bits.last.peek().litToBoolean)
        stalledAW.foreach { packet =>
          awValid shouldBe true
          awPacket shouldBe packet
        }
        stalledW.foreach { packet =>
          wValid shouldBe true
          wPacket shouldBe packet
        }
        stalledAW = Option.when(awValid && !awReady)(awPacket)
        stalledW = Option.when(wValid && !wReady)(wPacket)
        if (awValid && awReady) {
          val txnID = (dut.io.axi.aw.bits.addr.peek().litValue - address) / 64
          dut.io.axi.aw.bits.len.expect(1.U)
          awPacket._1 shouldBe externalAXIID(txnID, writePerfConfig(NoLLCEntries))
          writesPending.enqueue(txnID)
          issuedIDs += dut.io.axi.aw.bits.id.peek().litValue
        }
        if (wValid && wReady) {
          assert(writesPending.nonEmpty)
          val txnID = writesPending.front
          dut.io.axi.w.bits.data.expect((txnID * 2 + outputBeat + 1).U)
          dut.io.axi.w.bits.strb.expect(byteEnables.U)
          dut.io.axi.w.bits.last.expect((outputBeat == 1).B)
          beatCycles += cycle
          if (outputBeat == 1) {
            writesPending.dequeue()
          }
          outputBeat = 1 - outputBeat
        }
        dut.clock.step()
        cycle += 1
      }
      dut.io.chi.tx.req.valid.poke(false.B)
      dut.io.chi.tx.dat.valid.poke(false.B)
      dut.io.chi.rx.rsp.ready.poke(true.B)
      beatCycles.size shouldBe transactions * 2
      requests shouldBe transactions
      responses shouldBe transactions
      issuedIDs.size shouldBe transactions
      issuedIDs.distinct.size shouldBe transactions / 4
      all(issuedIDs.groupBy(identity).values.map(_.size).toSeq) shouldBe 4
      dut.io.occupancy.expect(transactions.U)
      val utilization = beatCycles.size.toDouble / (beatCycles.last - beatCycles.head + 1)
      val steadyCycles = beatCycles.slice(32, beatCycles.size - 32)
      val steadyUtilization = steadyCycles.size.toDouble / (steadyCycles.last - steadyCycles.head + 1)
      println(f"NoLLC write throughput: $utilization%.4f beats/cycle, steady=$steadyUtilization%.4f, cycles=$cycle")
      val dataGaps = dataCycles.sliding(2).map(p => p(1) - p(0)).toSeq.groupBy(identity).map {
        case (gap, occurrences) => gap -> occurrences.size
      }
      println(s"CHI data: stalls=$dataStalls, starvation=$dataStarvation, handshake gaps=$dataGaps")
      // Complete compressed IDs out of order, preserving FIFO responses within each ID.
      issuedIDs.groupBy(identity).toSeq.sortBy(_._1).reverse.foreach { case (_, ids) =>
        ids.foreach(id => sendB(dut, id))
      }
      dut.clock.step(20)
      dut.io.occupancy.expect(0.U)
      if (!backpressure) {
        steadyUtilization should be >= 0.99
      }
    }
  }

  it should "sustain full-rate AXI writes with 128 outstanding transactions and eight L2 banks" in {
    runWriteThroughput(backpressure = false)
  }

  it should "preserve 128 writes under randomized CHI and AXI backpressure" in {
    runWriteThroughput(backpressure = true)
  }

  for (entries <- Seq(2, 64, 128, 256)) {
    it should s"backpressure after all $entries configured transaction slots are occupied" in {
      val config = p.alterPartial { case NoLLCEntries => entries }
      test(LazyModule(new NoLLCCHI2AXITestHarness()(config)).module)
        .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        init(dut)
        dut.io.axi.ar.ready.poke(false.B)
        for (i <- 0 until entries) {
          sendReq(dut, ReadUnique, i, 0x80000000L + i * 64L, 6)
        }
        dut.clock.step(5)
        dut.io.occupancy.expect(entries.U)
        dut.io.accepted.expect(entries.U)
        pokeReq(dut, MakeUnique, entries + 4, 0x80000000L + entries * 64L, 6)
        dut.io.chi.tx.req.valid.poke(true.B)
        dut.clock.step(10)
        dut.io.chi.tx.req.ready.expect(false.B)
      }
    }
  }

  it should "restore colliding read slot IDs while different AXI IDs complete out of order" in {
    val entries = 8
    val config = p.alterPartial { case NoLLCEntries => entries }
    test(LazyModule(new NoLLCCHI2AXITestHarness()(config)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.axi.ar.bits.id.getWidth shouldBe 1
      dut.io.axi.ar.ready.poke(false.B)
      for (i <- 0 until entries) {
        sendReq(dut, ReadUnique, 32 + i, 0x80000000L + i * 64L, 6)
      }

      var issued = Seq.empty[(BigInt, Int)]
      for (_ <- 0 until entries) {
        var cycles = 0
        while (!dut.io.axi.ar.valid.peek().litToBoolean && cycles < 100) {
          dut.clock.step()
          cycles += 1
        }
        assert(cycles < 100)
        val id = dut.io.axi.ar.bits.id.peek().litValue
        val slot = ((dut.io.axi.ar.bits.addr.peek().litValue - 0x80000000L) / 64).toInt
        id shouldBe BigInt(slot % 2)
        issued :+= ((id, slot))
        dut.io.axi.ar.ready.poke(true.B)
        dut.clock.step()
        dut.io.axi.ar.ready.poke(false.B)
      }
      dut.io.occupancy.expect(entries.U)

      // Preserve FIFO order within each compressed ID, but complete ID 1 first.
      for (id <- Seq(BigInt(1), BigInt(0)); (_, slot) <- issued.filter(_._1 == id)) {
        for (beat <- 0 until 2) {
          dut.io.chi.rx.dat.ready.poke(false.B)
          sendReadBeat(dut, id, slot * 2 + beat, last = beat == 1)
          val (txnID, dbID, dataID) = takeDat(dut)
          txnID shouldBe BigInt(32 + slot)
          dbID shouldBe BigInt(slot)
          dataID shouldBe BigInt(beat * 2)
        }
      }
      dut.clock.step(5)
      dut.io.occupancy.expect(0.U)
    }
  }

  it should "restore colliding write slot IDs until every AXI B response completes" in {
    val entries = 4
    val config = p.alterPartial { case NoLLCEntries => entries }
    test(LazyModule(new NoLLCCHI2AXITestHarness()(config)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      for (i <- 0 until entries) {
        dut.io.axi.aw.ready.poke(false.B)
        dut.io.axi.w.ready.poke(false.B)
        sendReq(dut, WriteNoSnpPtl, 32 + i, 0x80000000L + i * 64L, 3)
        val (opcode, txnID, dbID) = takeRsp(dut)
        opcode shouldBe CompDBIDResp.litValue
        txnID shouldBe BigInt(32 + i)
        dbID shouldBe BigInt(i)
        sendData(dut, NonCopyBackWrData, dbID, 0, i + 1, 0xff)
        var cycles = 0
        while ((!dut.io.axi.aw.valid.peek().litToBoolean || !dut.io.axi.w.valid.peek().litToBoolean) &&
          cycles < 100) {
          dut.clock.step()
          cycles += 1
        }
        assert(cycles < 100)
        dut.io.axi.aw.bits.id.expect(0.U)
        dut.io.axi.aw.ready.poke(true.B)
        dut.io.axi.w.ready.poke(true.B)
        dut.clock.step(2)
      }
      dut.io.occupancy.expect(entries.U)
      for (i <- 0 until entries) {
        sendB(dut, 0)
        dut.clock.step(5)
        dut.io.occupancy.expect((entries - i - 1).U)
      }
    }
  }

  it should "restore read IDs, allow early CompAck, and block a same-line request" in {
    test(LazyModule(new NoLLCCHI2AXITestHarness()(p)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      sendReq(dut, ReadUnique, 10, 0x80000000L, 6, expCompAck = true)
      sendReq(dut, ReadNotSharedDirty, 11, 0x80000040L, 6, expCompAck = true)

      var arIDs = Seq.empty[BigInt]
      var arCycles = 0
      while (arIDs.size < 2 && arCycles < 200) {
        if (dut.io.axi.ar.valid.peek().litToBoolean) {
          val id = dut.io.axi.ar.bits.id.peek().litValue
          if (!arIDs.contains(id)) {
            arIDs :+= id
          }
        }
        dut.clock.step()
        arCycles += 1
      }
      assert(arIDs.size == 2)
      arIDs.distinct.size shouldBe 2
      val firstID = arIDs.head
      val secondID = arIDs.last
      sendReadBeat(dut, secondID, 0x22, last = false)
      val secondFirst = takeDat(dut)
      secondFirst._1 shouldBe 11
      sendReadBeat(dut, secondID, 0x23, last = true)
      val secondLast = takeDat(dut)
      secondLast._1 shouldBe 11
      sendReadBeat(dut, firstID, 0x10, last = false)
      val firstData = takeDat(dut)
      firstData._1 shouldBe 10
      val firstReturnedDBID = firstData._2

      val ack = dut.io.chi.tx.rsp.bits
      ack.qos.poke(0.U)
      ack.tgtID.poke(2.U)
      ack.srcID.poke(1.U)
      ack.txnID.poke(firstReturnedDBID.U)
      ack.opcode.poke(CompAck)
      ack.respErr.poke(0.U)
      ack.resp.poke(0.U)
      ack.fwdState.poke(0.U)
      ack.cBusy.foreach(_.poke(0.U))
      ack.dbID.poke(0.U)
      ack.pCrdType.poke(0.U)
      ack.tagOp.foreach(_.poke(0.U))
      ack.traceTag.poke(false.B)
      dut.io.chi.tx.rsp.valid.poke(true.B)
      var ackCycles = 0
      while (!dut.io.chi.tx.rsp.ready.peek().litToBoolean && ackCycles < 200) {
        dut.clock.step()
        ackCycles += 1
      }
      assert(ackCycles < 200)
      dut.clock.step()
      dut.io.chi.tx.rsp.valid.poke(false.B)

      pokeReq(dut, ReadNoSnp, 12, 0x80000008L, 3, OrderEncodings.RequestOrder)
      dut.io.chi.tx.req.valid.poke(true.B)
      dut.clock.step(10)
      dut.io.chi.tx.req.ready.expect(false.B)
      dut.io.chi.tx.req.valid.poke(false.B)

      sendReadBeat(dut, firstID, 0x11, last = true)
    }
  }

  it should "preserve NoSnp partial-write address, size, byte enables, and B lifetime" in {
    test(LazyModule(new NoLLCCHI2AXITestHarness()(p)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      val address = 0x80000008L
      sendReq(dut, WriteNoSnpPtl, 7, address, 3, OrderEncodings.RequestOrder, ewa = true)
      val (opcode, txnID, dbID) = takeRsp(dut)
      opcode shouldBe CompDBIDResp.litValue
      txnID shouldBe 7

      dut.io.axi.aw.ready.poke(false.B)
      dut.io.axi.w.ready.poke(false.B)
      sendData(dut, NonCopyBackWrData, dbID, 0, BigInt("1122334455667788", 16) << 64, 0xff00)
      var writeCycles = 0
      while ((!dut.io.axi.aw.valid.peek().litToBoolean || !dut.io.axi.w.valid.peek().litToBoolean) &&
        writeCycles < 200) {
        dut.clock.step()
        writeCycles += 1
      }
      assert(writeCycles < 200)
      dut.io.axi.aw.bits.addr.expect(address.U)
      dut.io.axi.aw.bits.size.expect(3.U)
      dut.io.axi.aw.bits.len.expect(0.U)
      dut.io.axi.w.bits.strb.expect(0xff00.U)
      dut.io.axi.w.bits.last.expect(true.B)
      val axiID = dut.io.axi.aw.bits.id.peek().litValue
      dut.io.axi.aw.ready.poke(true.B)
      dut.io.axi.w.ready.poke(true.B)
      dut.clock.step()
      sendB(dut, axiID)
      dut.clock.step(5)
      dut.io.occupancy.expect(0.U)

      sendReq(dut, WriteNoSnpPtl, 8, address + 64, 3, ewa = false)
      val (separateOpcode, separateTxnID, separateDBID) = takeRsp(dut)
      separateOpcode shouldBe DBIDResp.litValue
      separateTxnID shouldBe 8
      sendData(dut, NonCopyBackWrData, separateDBID, 0, BigInt("8877665544332211", 16) << 64, 0xff00)
      var separateWriteCycles = 0
      while (!dut.io.axi.aw.valid.peek().litToBoolean && separateWriteCycles < 100) {
        dut.clock.step()
        separateWriteCycles += 1
      }
      assert(separateWriteCycles < 100)
      val separateAxiID = dut.io.axi.aw.bits.id.peek().litValue
      dut.clock.step()
      sendB(dut, separateAxiID)
      val (finalOpcode, finalTxnID, _) = takeRsp(dut)
      finalOpcode shouldBe Comp.litValue
      finalTxnID shouldBe 8
      dut.clock.step(5)
      dut.io.occupancy.expect(0.U)
    }
  }
}
