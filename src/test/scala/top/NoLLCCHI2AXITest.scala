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

class NoLLCCHI2AXITestHarness(implicit p: Parameters) extends LazyModule {
  val bridge = LazyModule(new NoLLCCHI2AXI())
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
      val occupancy = Output(UInt(log2Ceil(129).W))
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

  private def sendData(
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

  it should "backpressure after all 128 transaction slots are occupied" in {
    test(LazyModule(new NoLLCCHI2AXITestHarness()(p)).module)
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
      init(dut)
      dut.io.axi.ar.ready.poke(false.B)
      for (i <- 0 until 128) {
        sendReq(dut, ReadUnique, i, 0x80000000L + i * 64L, 6)
      }
      dut.clock.step(5)
      dut.io.occupancy.expect(128.U)
      dut.io.accepted.expect(128.U)
      pokeReq(dut, MakeUnique, 132, 0x80002100L, 6)
      dut.io.chi.tx.req.valid.poke(true.B)
      dut.clock.step(10)
      dut.io.chi.tx.req.ready.expect(false.B)
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
