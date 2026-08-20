package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey}
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuType}
import xiangshan.backend.fu.vector.Bundles.{VConfig, VLmul, VSew}
import xiangshan.backend.fu.wrapper.VCVT
import xiangshan.backend.regfile.IntPregParams
import yunsuan.VfcvtType
import yunsuan.vector.VectorConvert.CVT_xx8

class XX8DecodeSplitHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val inst = Input(UInt(32.W))
    val vsew = Input(UInt(2.W))
    val vlmul = Input(UInt(3.W))
    val issue = Input(Bool())
    val fsOff = Input(Bool())

    val inputReady = Output(Bool())
    val isComplex = Output(Bool())
    val isVcvt = Output(Bool())
    val format = Output(UInt(2.W))
    val isXX8Split = Output(Bool())
    val decodeIllegal = Output(Bool())
    val numUops = Output(UInt(log2Up(MaxUopSize + 1).W))

    val uopValid = Output(Vec(RenameWidth, Bool()))
    val uopIdx = Output(Vec(RenameWidth, UInt(log2Up(MaxUopSize).W)))
    val source = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val oldVd = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
    val dest = Output(Vec(RenameWidth, UInt(LogicRegsWidth.W)))
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

  private val decoded = decode.io.deq.decodedInst
  io.isComplex := decode.io.deq.isComplex
  io.isVcvt := decoded.fuType === FuType.vfcvt.U
  io.format := MuxLookup(decoded.fuOpType, 3.U)(Seq(
    VfcvtType.vfncvtxx8_int8 -> 0.U,
    VfcvtType.vfncvtxx8_e4m3 -> 1.U,
    VfcvtType.vfncvtxx8_e5m2 -> 2.U
  ))
  io.isXX8Split := decoded.uopSplitType === UopSplitType.VEC_XX8
  io.decodeIllegal := decoded.exceptionVec(ExceptionNO.EX_II)
  io.numUops := decode.io.deq.uopInfo.numOfUop

  private val split = Module(new DecodeUnitComp)
  split.io.redirect := false.B
  split.io.csrCtrl := 0.U.asTypeOf(split.io.csrCtrl)
  split.io.vtypeBypass := 0.U.asTypeOf(split.io.vtypeBypass)
  split.io.in.valid := io.issue
  split.io.in.bits.simpleDecodedInst := decoded
  split.io.in.bits.uopInfo := decode.io.deq.uopInfo
  io.inputReady := split.io.in.ready

  for (index <- 0 until RenameWidth) {
    val out = split.io.out.complexDecodedInsts(index)
    out.ready := true.B
    io.uopValid(index) := out.valid
    io.uopIdx(index) := out.bits.uopIdx
    io.source(index) := out.bits.lsrc(1)
    io.oldVd(index) := out.bits.lsrc(2)
    io.dest(index) := out.bits.ldest
    io.uopIllegal(index) := out.bits.exceptionVec(ExceptionNO.EX_II)
  }
}

class XX8VCVTHarness(implicit p: Parameters) extends XSModule {
  private val cfg = FuConfig.VfcvtCfg
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val format = Input(UInt(2.W))
    val source = Input(UInt(128.W))
    val oldVd = Input(UInt(128.W))
    val mask = Input(UInt(128.W))
    val vm = Input(Bool())
    val vma = Input(Bool())
    val vta = Input(Bool())
    val vl = Input(UInt(8.W))
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
  ctrl.fuOpType := MuxLookup(io.format, VfcvtType.vfncvtxx8_int8)(Seq(
    0.U -> VfcvtType.vfncvtxx8_int8,
    1.U -> VfcvtType.vfncvtxx8_e4m3,
    2.U -> VfcvtType.vfncvtxx8_e5m2
  ))
  ctrl.vecWen.get := true.B
  ctrl.fpu.get.rm := 7.U
  ctrl.vpu.get.vsew := VSew.e32
  ctrl.vpu.get.vlmul := VLmul.m8
  ctrl.vpu.get.vm := io.vm
  ctrl.vpu.get.vma := io.vma
  ctrl.vpu.get.vta := io.vta
  ctrl.vpu.get.vstart := 0.U
  ctrl.vpu.get.vuopIdx := io.uopIdx
  ctrl.vpu.get.fpu.isFpToVecInst := false.B

  private val vconfig = WireInit(0.U.asTypeOf(new VConfig))
  vconfig.vtype.vsew := VSew.e32
  vconfig.vtype.vlmul := VLmul.m8
  vconfig.vtype.vma := io.vma
  vconfig.vtype.vta := io.vta
  vconfig.vl := io.vl

  cvt.io.in.bits.data.src(1) := io.source
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

class XX8IntegrationSpec extends XSTester with Matchers {
  private val baseConfig = new DefaultConfig
  override implicit val config: Parameters = baseConfig.alterPartial({
    case XSCoreParamsKey => baseConfig(XSTileKey).head.copy(
      intPreg = IntPregParams(numEntries = 64, numRead = Some(14), numWrite = Some(8))
    )
  }).alter((site, here, up) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false,
      enablePerf = false,
      fpgaPlatform = false,
      enableXMR = false
    )
  })

  private def encode(format: Int, vd: Int, vs2: Int, vm: Boolean = true): BigInt =
    (BigInt(0x0f) << 26) |
      (BigInt(if (vm) 1 else 0) << 25) |
      (BigInt(vs2) << 20) |
      (BigInt(0x14) << 15) |
      (BigInt(format) << 12) |
      (BigInt(vd) << 7) |
      BigInt(0x2b)

  behavior of "vfncvtxx8 decode and split"

  it should "decode all formats and map four source uops per destination register" in {
    test(new XX8DecodeSplitHarness) { dut =>
      dut.io.issue.poke(false.B)
      dut.io.fsOff.poke(false.B)
      dut.io.vsew.poke(VSew.e32)
      dut.io.vlmul.poke(VLmul.m8)

      for ((format, expected) <- Seq(0 -> 0, 1 -> 1, 3 -> 2)) {
        dut.io.inst.poke(encode(format, vd = 4, vs2 = 16).U)
        dut.io.isComplex.expect(true.B)
        dut.io.isVcvt.expect(true.B)
        dut.io.format.expect(expected.U)
        dut.io.isXX8Split.expect(true.B)
        dut.io.decodeIllegal.expect(false.B)
        dut.io.numUops.expect(8.U)
      }

      dut.io.inst.poke(encode(2, vd = 4, vs2 = 16).U)
      dut.io.decodeIllegal.expect(true.B)
      dut.io.inst.poke(encode(1, vd = 4, vs2 = 16).U)
      dut.io.fsOff.poke(true.B)
      dut.io.decodeIllegal.expect(true.B)
      dut.io.fsOff.poke(false.B)

      dut.io.inst.poke(encode(1, vd = 4, vs2 = 16).U)
      dut.io.issue.poke(true.B)
      dut.io.inputReady.expect(true.B)
      dut.clock.step()
      dut.io.issue.poke(false.B)
      dut.io.fsOff.poke(false.B)

      val observed = collection.mutable.ArrayBuffer.empty[(Int, Int, Int, Int, Boolean)]
      var cycles = 0
      while (observed.size < 8 && cycles < 8) {
        for (index <- dut.io.uopValid.indices if dut.io.uopValid(index).peek().litToBoolean) {
          observed += ((
            dut.io.uopIdx(index).peek().litValue.toInt,
            dut.io.source(index).peek().litValue.toInt,
            dut.io.oldVd(index).peek().litValue.toInt,
            dut.io.dest(index).peek().litValue.toInt,
            dut.io.uopIllegal(index).peek().litToBoolean
          ))
        }
        dut.clock.step()
        cycles += 1
      }

      observed.size shouldBe 8
      for (((uopIdx, source, oldVd, dest, illegal), index) <- observed.zipWithIndex) {
        uopIdx shouldBe index
        source shouldBe 16 + index
        oldVd shouldBe 4 + index / 4
        dest shouldBe 4 + index / 4
        illegal shouldBe false
      }

    }
  }

  it should "mark illegal SEW and LMUL configurations during splitting" in {
    test(new XX8DecodeSplitHarness) { dut =>
      dut.io.issue.poke(false.B)
      dut.io.inst.poke(encode(1, vd = 4, vs2 = 16).U)

      for ((vsew, vlmul) <- Seq((VSew.e16, VLmul.m8), (VSew.e32, VLmul.mf4))) {
        dut.io.vsew.poke(vsew)
        dut.io.vlmul.poke(vlmul)
        dut.io.issue.poke(true.B)
        dut.io.inputReady.expect(true.B)
        dut.clock.step()
        dut.io.issue.poke(false.B)

        var sawIllegal = false
        var cycles = 0
        while (!sawIllegal && cycles < 8) {
          sawIllegal = dut.io.uopValid.indices.exists { index =>
            dut.io.uopValid(index).peek().litToBoolean &&
              dut.io.uopIllegal(index).peek().litToBoolean
          }
          dut.clock.step()
          cycles += 1
        }
        sawIllegal shouldBe true
      }
    }
  }

  behavior of "vfncvtxx8 numeric conversion"

  private def issueNumeric(dut: CVT_xx8, src: BigInt, opType: UInt, rm: Int = 0): Unit = {
    dut.io.src.poke(src.U)
    dut.io.opType.poke(opType)
    dut.io.rm.poke(rm.U)
    dut.io.fire.poke(true.B)
    dut.clock.step()
    dut.io.fire.poke(false.B)
    dut.clock.step(2)
  }

  it should "implement INT8 and FP8 rounding and special values" in {
    test(new CVT_xx8) { dut =>
      issueNumeric(dut, BigInt("3fc00000", 16), VfcvtType.vfncvtxx8_int8)
      dut.io.result.expect("h02".U)
      dut.io.fflags.expect("b00001".U)

      issueNumeric(dut, BigInt("43000000", 16), VfcvtType.vfncvtxx8_int8)
      dut.io.result.expect("h7f".U)
      dut.io.fflags.expect("b10000".U)

      issueNumeric(dut, BigInt("3f000000", 16), VfcvtType.vfncvtxx8_int8, rm = 3)
      dut.io.result.expect(1.U)
      dut.io.fflags.expect(1.U)

      issueNumeric(dut, BigInt("3f800000", 16), VfcvtType.vfncvtxx8_e4m3)
      dut.io.result.expect("h38".U)
      dut.io.fflags.expect(0.U)

      issueNumeric(dut, BigInt("43f00000", 16), VfcvtType.vfncvtxx8_e4m3)
      dut.io.result.expect("h7e".U)
      dut.io.fflags.expect("b00101".U)

      issueNumeric(dut, BigInt("7f800001", 16), VfcvtType.vfncvtxx8_e4m3)
      dut.io.result.expect("h7f".U)
      dut.io.fflags.expect("b10000".U)

      issueNumeric(dut, BigInt("7f800000", 16), VfcvtType.vfncvtxx8_e5m2)
      dut.io.result.expect("h7c".U)
      dut.io.fflags.expect(0.U)

      issueNumeric(dut, BigInt("47800000", 16), VfcvtType.vfncvtxx8_e5m2, rm = 1)
      dut.io.result.expect("h7b".U)
      dut.io.fflags.expect("b00101".U)
    }
  }

  behavior of "vfncvtxx8 VCVT integration"

  private def packedFp32(values: Seq[Long]): BigInt =
    values.zipWithIndex.foldLeft(BigInt(0)) { case (result, (value, index)) =>
      result | (BigInt(value) << (32 * index))
    }

  it should "pack results and preserve masked bytes" in {
    test(new XX8VCVTHarness) { dut =>
      dut.io.valid.poke(false.B)
      dut.io.format.poke(1.U)
      dut.io.source.poke(packedFp32(Seq(0x3f800000L, 0x40000000L, 0xbf800000L, 0xc0000000L)).U)
      dut.io.oldVd.poke(BigInt("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", 16).U)
      dut.io.mask.poke("h00050000".U)
      dut.io.vm.poke(false.B)
      dut.io.vma.poke(false.B)
      dut.io.vta.poke(false.B)
      dut.io.vl.poke(32.U)
      dut.io.uopIdx.poke(4.U)
      dut.io.frm.poke(0.U)
      dut.io.valid.poke(true.B)
      dut.io.ready.expect(true.B)
      dut.clock.step()
      dut.io.valid.poke(false.B)

      var cycles = 0
      while (!dut.io.outValid.peek().litToBoolean && cycles < 8) {
        dut.clock.step()
        cycles += 1
      }
      dut.io.outValid.expect(true.B)
      dut.io.result.expect(BigInt("aaaaaaaaaaaaaaaaaaaaaaaaaab8aa38", 16).U)
      dut.io.fflags.expect(0.U)
    }
  }
}
