package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.vector.Bundles.{VConfig, VLmul, VSew}
import xiangshan.backend.fu.wrapper.VCVT
import xiangshan.backend.regfile.IntPregParams
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey}
import yunsuan.VfcvtType

class MXFPDecodeSplitHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val vsew = Input(UInt(2.W))
    val vlmul = Input(UInt(3.W))
    val issue = Input(Bool())
    val fsOff = Input(Bool())
    val vsOff = Input(Bool())

    val inputReady = Output(Bool())
    val isComplex = Output(Bool())
    val isVcvt = Output(Bool())
    val isMxfp4 = Output(Bool())
    val isMxfp8 = Output(Bool())
    val splitIsMxfp4 = Output(Bool())
    val splitIsMxfp8 = Output(Bool())
    val vecWen = Output(Bool())
    val writesFflags = Output(Bool())
    val decodeIllegal = Output(Bool())
    val numUops = Output(UInt(log2Up(MaxUopSize + 1).W))
    val decodedLsrc = Output(Vec(3, UInt(LogicRegsWidth.W)))
    val decodedLdest = Output(UInt(LogicRegsWidth.W))

    val complexNum = Output(UInt(log2Up(RenameWidth + 1).W))
    val uopValid = Output(Vec(RenameWidth, Bool()))
    val uopIdx = Output(Vec(RenameWidth, UInt(log2Up(MaxUopSize).W)))
    val lsrc0 = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val lsrc1 = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val lsrc2 = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val ldest = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val uopIllegal = Output(Vec(RenameWidth, Bool()))
  })

  private val decode = Module(new DecodeUnit)
  decode.io.enq := 0.U.asTypeOf(decode.io.enq)
  decode.io.csrCtrl := 0.U.asTypeOf(decode.io.csrCtrl)
  decode.io.fromCSR := 0.U.asTypeOf(decode.io.fromCSR)
  decode.io.enq.ctrlFlow.instr := io.inst
  decode.io.enq.vtype.vsew := io.vsew
  decode.io.enq.vtype.vlmul := io.vlmul
  decode.io.fromCSR.illegalInst.fsIsOff := io.fsOff
  decode.io.fromCSR.illegalInst.vsIsOff := io.vsOff

  private val decoded = decode.io.deq.decodedInst
  io.isComplex := decode.io.deq.isComplex
  io.isVcvt := decoded.fuType === FuType.vfcvt.U
  io.isMxfp4 := decoded.fuOpType === VfcvtType.vfncvtmxfp4_ffw
  io.isMxfp8 := decoded.fuOpType === VfcvtType.vfncvtmxfp8_ffw
  io.splitIsMxfp4 := decoded.uopSplitType === UopSplitType.VEC_MXFP4
  io.splitIsMxfp8 := decoded.uopSplitType === UopSplitType.VEC_MXFP8
  io.vecWen := decoded.vecWen
  io.writesFflags := decoded.wfflags
  io.decodeIllegal := decoded.exceptionVec(ExceptionNO.EX_II)
  io.numUops := decode.io.deq.uopInfo.numOfUop
  io.decodedLsrc.zip(decoded.lsrc.take(3)).foreach { case (out, in) => out := in }
  io.decodedLdest := decoded.ldest

  private val split = Module(new DecodeUnitComp)
  split.io.redirect := false.B
  split.io.csrCtrl := 0.U.asTypeOf(split.io.csrCtrl)
  split.io.vtypeBypass := 0.U.asTypeOf(split.io.vtypeBypass)
  split.io.in.valid := io.issue
  split.io.in.bits.simpleDecodedInst := decoded
  split.io.in.bits.uopInfo := decode.io.deq.uopInfo
  io.inputReady := split.io.in.ready
  io.complexNum := split.io.complexNum

  for (i <- 0 until RenameWidth) {
    val out = split.io.out.complexDecodedInsts(i)
    out.ready := true.B
    io.uopValid(i) := out.valid
    io.uopIdx(i) := out.bits.uopIdx
    io.lsrc0(i) := out.bits.lsrc(0)
    io.lsrc1(i) := out.bits.lsrc(1)
    io.lsrc2(i) := out.bits.lsrc(2)
    io.ldest(i) := out.bits.ldest
    io.uopIllegal(i) := out.bits.exceptionVec(ExceptionNO.EX_II)
  }
}

class MXFPVCVTHarness(implicit p: Parameters) extends XSModule {
  private val cfg = FuConfig.VfcvtCfg
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val isFp4 = Input(Bool())
    val source = Input(UInt(128.W))
    val scale = Input(UInt(128.W))
    val oldVd = Input(UInt(128.W))
    val mask = Input(UInt(128.W))
    val vm = Input(Bool())
    val vma = Input(Bool())
    val vta = Input(Bool())
    val vl = Input(UInt(8.W))
    val vstart = Input(UInt(7.W))
    val vlmul = Input(UInt(3.W))
    val uopIdx = Input(UInt(6.W))
    val frm = Input(UInt(3.W))

    val ready = Output(Bool())
    val outValid = Output(Bool())
    val result = Output(UInt(128.W))
    val fflags = Output(UInt(5.W))
  })

  private val cvt = Module(new VCVT(cfg))
  cvt.io.flush := 0.U.asTypeOf(cvt.io.flush)
  cvt.io.out.ready := true.B
  cvt.io.frm.get := io.frm
  cvt.io.in.valid := io.valid
  cvt.io.in.bits := 0.U.asTypeOf(cvt.io.in.bits)

  private val ctrl = cvt.io.in.bits.ctrl
  ctrl.fuOpType := Mux(io.isFp4, VfcvtType.vfncvtmxfp4_ffw, VfcvtType.vfncvtmxfp8_ffw)
  ctrl.vecWen.get := true.B
  ctrl.fpu.get.rm := 7.U
  ctrl.vpu.get.vsew := VSew.e32
  ctrl.vpu.get.vlmul := io.vlmul
  ctrl.vpu.get.vm := io.vm
  ctrl.vpu.get.vma := io.vma
  ctrl.vpu.get.vta := io.vta
  ctrl.vpu.get.vstart := io.vstart
  ctrl.vpu.get.vuopIdx := io.uopIdx
  ctrl.vpu.get.fpu.isFpToVecInst := false.B

  private val vconfig = WireInit(0.U.asTypeOf(new VConfig))
  vconfig.vtype.vsew := VSew.e32
  vconfig.vtype.vlmul := io.vlmul
  vconfig.vtype.vma := io.vma
  vconfig.vtype.vta := io.vta
  vconfig.vl := io.vl

  cvt.io.in.bits.data.src(0) := io.source
  cvt.io.in.bits.data.src(1) := io.scale
  cvt.io.in.bits.data.src(2) := io.oldVd
  cvt.io.in.bits.data.src(cfg.maskSrcIdx) := io.mask
  cvt.io.in.bits.data.src(cfg.vconfigIdx) := vconfig.asUInt
  cvt.io.in.bits.ctrlPipe.get.foreach(_ := ctrl)
  cvt.io.in.bits.dataPipe.get.foreach(_ := cvt.io.in.bits.data)
  cvt.io.in.bits.validPipe.get.foreach(_ := true.B)

  io.ready := cvt.io.in.ready
  io.outValid := cvt.io.out.valid
  io.result := cvt.io.out.bits.res.data
  io.fflags := cvt.io.out.bits.res.fflags.get
}

class MXFPIntegrationSpec extends XSTester {
  private val baseConfig = new DefaultConfig
  override implicit val config: Parameters = baseConfig.alterPartial({
    case XSCoreParamsKey => baseConfig(XSTileKey).head.copy(
      intPreg = IntPregParams(
        numEntries = 64,
        numRead = Some(14),
        numWrite = Some(8)
      )
    )
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = false,
      fpgaPlatform = false,
      enableXMR = false
    )
  })

  private def encode(target: Int, vd: Int, vs1: Int, vs2: Int, vm: Boolean = true): BigInt = {
    (BigInt(0x0e) << 26) |
      (BigInt(if (vm) 1 else 0) << 25) |
      (BigInt(vs2) << 20) |
      (BigInt(vs1) << 15) |
      (BigInt(target) << 12) |
      (BigInt(vd) << 7) |
      BigInt(0x5b)
  }

  private def pulse(dut: MXFPDecodeSplitHarness, inst: BigInt, vsew: Int, vlmul: Int): Unit = {
    dut.io.inst.poke(inst.U)
    dut.io.vsew.poke(vsew.U)
    dut.io.vlmul.poke(vlmul.U)
    dut.io.fsOff.poke(false.B)
    dut.io.vsOff.poke(false.B)
    dut.io.issue.poke(true.B)
    dut.io.inputReady.expect(true.B)
    dut.clock.step()
    dut.io.issue.poke(false.B)
  }

  private def collectUops(dut: MXFPDecodeSplitHarness, expected: Int): Seq[(Int, Int, Int, Int, Int, Boolean)] = {
    val result = collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int, Int, Boolean)]
    var cycles = 0
    while (result.size < expected && cycles < 8) {
      for (i <- dut.io.uopValid.indices if dut.io.uopValid(i).peek().litToBoolean) {
        result += ((
          dut.io.uopIdx(i).peek().litValue.toInt,
          dut.io.lsrc0(i).peek().litValue.toInt,
          dut.io.lsrc1(i).peek().litValue.toInt,
          dut.io.lsrc2(i).peek().litValue.toInt,
          dut.io.ldest(i).peek().litValue.toInt,
          dut.io.uopIllegal(i).peek().litToBoolean
        ))
      }
      dut.clock.step()
      cycles += 1
    }
    result.toSeq
  }

  behavior of "XSAI MXFP decode and split"

  it should "decode, split, route, and reject illegal MXFP instructions" in {
    test(new MXFPDecodeSplitHarness) { dut =>
      dut.io.issue.poke(false.B)
      dut.io.fsOff.poke(false.B)
      dut.io.vsOff.poke(false.B)
      dut.io.vsew.poke(VSew.e32)
      dut.io.vlmul.poke(VLmul.m8)

      for ((word, isFp4) <- Seq((BigInt("3800105b", 16), true), (BigInt("3800205b", 16), false))) {
        dut.io.inst.poke(word.U)
        dut.io.isComplex.expect(true.B)
        dut.io.isVcvt.expect(true.B)
        dut.io.isMxfp4.expect(isFp4.B)
        dut.io.isMxfp8.expect((!isFp4).B)
        dut.io.splitIsMxfp4.expect(isFp4.B)
        dut.io.splitIsMxfp8.expect((!isFp4).B)
        dut.io.vecWen.expect(true.B)
        dut.io.writesFflags.expect(true.B)
        dut.io.numUops.expect(8.U)
      }

      val vs1 = 8
      val vs2 = 20
      val vd = 4

      pulse(dut, encode(2, vd, vs1, vs2), vsew = 2, vlmul = 3)
      val mxfp8 = collectUops(dut, 8)
      mxfp8.size shouldBe 8
      for (((idx, src0, src1, oldVd, dest, illegal), i) <- mxfp8.zipWithIndex) {
        idx shouldBe i
        src0 shouldBe vs1 + i
        src1 shouldBe vs2
        oldVd shouldBe vd + i / 4
        dest shouldBe vd + i / 4
        illegal shouldBe false
      }

      pulse(dut, encode(1, vd, vs1, vs2), vsew = 2, vlmul = 3)
      val mxfp4 = collectUops(dut, 8)
      mxfp4.size shouldBe 8
      for (((idx, src0, src1, oldVd, dest, illegal), i) <- mxfp4.zipWithIndex) {
        idx shouldBe i
        src0 shouldBe vs1 + i
        src1 shouldBe vs2
        oldVd shouldBe vd
        dest shouldBe vd
        illegal shouldBe false
      }

      dut.io.issue.poke(false.B)
      dut.io.inst.poke(encode(2, vd = 4, vs1 = 8, vs2 = 20).U)
      dut.io.vsew.poke(VSew.e32)
      dut.io.vlmul.poke(VLmul.m8)
      dut.io.fsOff.poke(true.B)
      dut.io.vsOff.poke(false.B)
      dut.io.decodeIllegal.expect(true.B)

      pulse(dut, encode(2, vd = 4, vs1 = 8, vs2 = 20), vsew = 1, vlmul = 3)
      collectUops(dut, 8).forall(_._6) shouldBe true

      pulse(dut, encode(1, vd = 4, vs1 = 8, vs2 = 20), vsew = 2, vlmul = 7)
      collectUops(dut, 1).head._6 shouldBe true

      pulse(dut, encode(2, vd = 4, vs1 = 8, vs2 = 4), vsew = 2, vlmul = 3)
      collectUops(dut, 8).forall(_._6) shouldBe true
    }
  }

  behavior of "XSAI VCVT MXFP coupling"

  private def packedFp32(values: Seq[Long]): BigInt =
    values.zipWithIndex.foldLeft(BigInt(0)) { case (acc, (value, index)) =>
      acc | (BigInt(value) << (32 * index))
    }

  private def issueCvt(
    dut: MXFPVCVTHarness,
    isFp4: Boolean,
    source: BigInt,
    oldVd: BigInt,
    mask: BigInt,
    vm: Boolean,
    vma: Boolean,
    vta: Boolean,
    vl: Int,
    uopIdx: Int
  ): Unit = {
    dut.io.isFp4.poke(isFp4.B)
    dut.io.source.poke(source.U)
    dut.io.scale.poke(127.U)
    dut.io.oldVd.poke(oldVd.U)
    dut.io.mask.poke(mask.U)
    dut.io.vm.poke(vm.B)
    dut.io.vma.poke(vma.B)
    dut.io.vta.poke(vta.B)
    dut.io.vl.poke(vl.U)
    dut.io.vstart.poke(0.U)
    dut.io.vlmul.poke(VLmul.m8)
    dut.io.uopIdx.poke(uopIdx.U)
    dut.io.frm.poke(0.U)
    dut.io.valid.poke(true.B)
    dut.io.ready.expect(true.B)
    dut.clock.step()
    dut.io.valid.poke(false.B)
    var cycles = 0
    while (!dut.io.outValid.peek().litToBoolean && cycles < 6) {
      dut.clock.step()
      cycles += 1
    }
    dut.io.outValid.expect(true.B)
  }

  it should "route operands and merge MXFP8 bytes and MXFP4 nibbles" in {
    test(new MXFPVCVTHarness) { dut =>
      dut.io.valid.poke(false.B)
      val source = packedFp32(Seq(0x3f800000L, 0x40000000L, 0x3f800000L, 0x40000000L))
      val mxfp8OldVd = BigInt("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 16)
      issueCvt(
        dut, isFp4 = false, source = source, oldVd = mxfp8OldVd, mask = 0,
        vm = true, vma = false, vta = false, vl = 32, uopIdx = 4
      )
      dut.io.result.expect(((mxfp8OldVd & ~BigInt("ffffffff", 16)) | BigInt("40384038", 16)).U)
      dut.io.fflags.expect(0.U)
      dut.clock.step()

      val oldVd = BigInt("55555555555555555555555555555555", 16)
      issueCvt(
        dut, isFp4 = true, source = source, oldVd = oldVd, mask = 0x50,
        vm = false, vma = false, vta = false, vl = 32, uopIdx = 1
      )
      val masked = (oldVd & ~(BigInt("ffff", 16) << 16)) | (BigInt("5252", 16) << 16)
      dut.io.result.expect(masked.U)
      dut.clock.step()

      issueCvt(
        dut, isFp4 = true, source = source, oldVd = oldVd, mask = 0,
        vm = true, vma = false, vta = true, vl = 6, uopIdx = 1
      )
      val allOnes = (BigInt(1) << 128) - 1
      val tailed = (allOnes & ~((BigInt(1) << 24) - 1)) | BigInt("425555", 16)
      dut.io.result.expect(tailed.U)
    }
  }
}
