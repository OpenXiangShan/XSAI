package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import cute.MatrixIsaParams
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.tile.XLen
import utility._
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles.MSew

object McfgHelpers {
  def entryCount: Int = 8

  sealed trait McfgTypeFamily

  case object McfgInt extends McfgTypeFamily

  case object McfgFloat extends McfgTypeFamily

  sealed abstract class McfgSemanticType(
    val typeCode: Int,
    val typeName: String,
    val elementWidth: Int,
    val family: McfgTypeFamily,
    val amuPayload: Int)

  case object Int4 extends McfgSemanticType(0, "int4", 4, McfgInt, intPayload(signed = true, WidthE4))

  case object UInt4 extends McfgSemanticType(1, "uint4", 4, McfgInt, intPayload(signed = false, WidthE4))

  case object Int8 extends McfgSemanticType(2, "int8", 8, McfgInt, intPayload(signed = true, WidthE8))

  case object UInt8 extends McfgSemanticType(3, "uint8", 8, McfgInt, intPayload(signed = false, WidthE8))

  case object Int32 extends McfgSemanticType(4, "int32", 32, McfgInt, WidthE32)

  case object Nvfp4 extends McfgSemanticType(5, "nvfp4", 4, McfgFloat, fpPayload(isAlt = false, WidthE4))

  case object Mxfp4 extends McfgSemanticType(6, "mxfp4", 4, McfgFloat, fpPayload(isAlt = true, WidthE4))

  case object Fp8E5M2 extends McfgSemanticType(7, "fp8e5m2", 8, McfgFloat, fpPayload(isAlt = false, WidthE8))

  case object Fp8E4M3 extends McfgSemanticType(8, "fp8e4m3", 8, McfgFloat, fpPayload(isAlt = true, WidthE8))

  case object Fp16 extends McfgSemanticType(9, "fp16", 16, McfgFloat, fpPayload(isAlt = false, WidthE16))

  case object Bf16 extends McfgSemanticType(10, "bf16", 16, McfgFloat, fpPayload(isAlt = true, WidthE16))

  case object Tf32 extends McfgSemanticType(11, "tf32", 32, McfgFloat, fpPayload(isAlt = true, WidthE32))

  case object Fp32 extends McfgSemanticType(12, "fp32", 32, McfgFloat, fpPayload(isAlt = false, WidthE32))

  case class McfgDecoded(typeCode: Int, tableSel: Int, semanticType: Option[McfgSemanticType]) {
    def legal: Boolean = semanticType.isDefined
    def typeName: Option[String] = semanticType.map(_.typeName)
    def elementWidth: Option[Int] = semanticType.map(_.elementWidth)
    def isFloat: Boolean = semanticType.exists(_.family == McfgFloat)
    def isInteger: Boolean = semanticType.exists(_.family == McfgInt)
    def amuPayload: Option[Int] = semanticType.map(_.amuPayload)
  }

  private val WidthE8 = 0
  private val WidthE16 = 1
  private val WidthE32 = 2
  private val WidthE4 = 3

  val semanticTypes: Seq[McfgSemanticType] = Seq(
    Int4,
    UInt4,
    Int8,
    UInt8,
    Int32,
    Nvfp4,
    Mxfp4,
    Fp8E5M2,
    Fp8E4M3,
    Fp16,
    Bf16,
    Tf32,
    Fp32
  )

  private val semanticTypesByCode: Map[Int, McfgSemanticType] = semanticTypes.map(t => t.typeCode -> t).toMap

  private def intPayload(signed: Boolean, widthCode: Int): Int = (if (signed) 4 else 0) | widthCode

  private def fpPayload(isAlt: Boolean, widthCode: Int): Int = (if (isAlt) 4 else 0) | widthCode

  def decode(typeCode: Int, tableSel: Int): McfgDecoded = {
    McfgDecoded(typeCode, tableSel, Option.when(tableSel == 0)(semanticTypesByCode.get(typeCode)).flatten)
  }

  def decodeRaw(raw: BigInt): McfgDecoded = decode((raw & 0xf).toInt, ((raw >> 4) & 0xf).toInt)

  def selectorToIndex(sel: UInt): UInt = sel(2, 0)

  def isDefinedTypeCode(typeCode: UInt): Bool = {
    MuxLookup(typeCode, false.B)(semanticTypes.map(t => t.typeCode.U -> true.B))
  }

  def isPdfDefined(typeCode: UInt, tableSel: UInt): Bool = tableSel === 0.U && isDefinedTypeCode(typeCode)

  def isSupported(typeCode: UInt, tableSel: UInt): Bool = tableSel === 0.U && isDefinedTypeCode(typeCode)

  def isFloatType(typeCode: UInt): Bool = {
    MuxLookup(typeCode, false.B)(semanticTypes.map(t => t.typeCode.U -> (t.family == McfgFloat).B))
  }

  def isIntegerType(typeCode: UInt): Bool = {
    MuxLookup(typeCode, false.B)(semanticTypes.map(t => t.typeCode.U -> (t.family == McfgInt).B))
  }

  def elementWidth(typeCode: UInt): UInt = {
    MuxLookup(typeCode, 0.U(6.W))(semanticTypes.map(t => t.typeCode.U -> t.elementWidth.U(6.W)))
  }

  def amuPayload(typeCode: UInt): UInt = {
    MuxLookup(typeCode, 0.U(3.W))(semanticTypes.map(t => t.typeCode.U -> t.amuPayload.U(3.W)))
  }

  def msew(typeCode: UInt): UInt = {
    MuxLookup(elementWidth(typeCode), MSew.e8)(Seq(
      4.U  -> MSew.e4,
      8.U  -> MSew.e8,
      16.U -> MSew.e16,
      32.U -> MSew.e32
    ))
  }

  def isMmaTripleSupported(
    aType: McfgSemanticType,
    bType: McfgSemanticType,
    cType: McfgSemanticType,
    matrixExtension: MatrixIsaParams
  ): Boolean = {
    val supportsInt8 = matrixExtension.enableInt8Int32 && cType == Int32 && isInt8Type(aType) && isInt8Type(bType)
    val supportsInt4 = matrixExtension.enableInt4Int32 && cType == Int32 && isInt4Type(aType) && isInt4Type(bType)
    val supportsFp8Fp16 = matrixExtension.enableFp8Fp16 && cType == Fp16 && sameFp8Type(aType, bType)
    val supportsFp8Bf16 = matrixExtension.enableFp8Bf16 && cType == Bf16 && sameFp8Type(aType, bType)
    val supportsFp8Fp32 = matrixExtension.enableFp8Fp32 && cType == Fp32 && sameFp8Type(aType, bType)
    val supportsFp16Fp16 = matrixExtension.enableFp16Fp16 && aType == Fp16 && bType == Fp16 && cType == Fp16
    val supportsFp16Fp32 = matrixExtension.enableFp16Fp32 && aType == Fp16 && bType == Fp16 && cType == Fp32
    val supportsBf16Fp32 = matrixExtension.enableBf16Fp32 && aType == Bf16 && bType == Bf16 && cType == Fp32
    val supportsTf32Fp32 = matrixExtension.enableTf32Fp32 && aType == Tf32 && bType == Tf32 && cType == Fp32
    val supportsFp32Fp32 = matrixExtension.enableFp32Fp32 && aType == Fp32 && bType == Fp32 && cType == Fp32

    supportsInt8 || supportsInt4 || supportsFp8Fp16 || supportsFp8Bf16 || supportsFp8Fp32 ||
      supportsFp16Fp16 || supportsFp16Fp32 || supportsBf16Fp32 || supportsTf32Fp32 || supportsFp32Fp32
  }

  def isMmaTripleSupportedRaw(aRaw: BigInt, bRaw: BigInt, cRaw: BigInt, matrixExtension: MatrixIsaParams): Boolean = {
    val decoded = Seq(decodeRaw(aRaw), decodeRaw(bRaw), decodeRaw(cRaw)).map(_.semanticType)
    decoded match {
      case Seq(Some(aType), Some(bType), Some(cType)) => isMmaTripleSupported(aType, bType, cType, matrixExtension)
      case _ => false
    }
  }

  def isMmaTripleSupported(
    aTypeCode: UInt,
    bTypeCode: UInt,
    cTypeCode: UInt,
    matrixExtension: MatrixIsaParams
  ): Bool = {
    val supported = Seq(
      Option.when(matrixExtension.enableInt8Int32)(isInt8Type(aTypeCode) && isInt8Type(bTypeCode) && cTypeCode === Int32.typeCode.U),
      Option.when(matrixExtension.enableInt4Int32)(isInt4Type(aTypeCode) && isInt4Type(bTypeCode) && cTypeCode === Int32.typeCode.U),
      Option.when(matrixExtension.enableFp8Fp16)(sameFp8Type(aTypeCode, bTypeCode) && cTypeCode === Fp16.typeCode.U),
      Option.when(matrixExtension.enableFp8Bf16)(sameFp8Type(aTypeCode, bTypeCode) && cTypeCode === Bf16.typeCode.U),
      Option.when(matrixExtension.enableFp8Fp32)(sameFp8Type(aTypeCode, bTypeCode) && cTypeCode === Fp32.typeCode.U),
      Option.when(matrixExtension.enableFp16Fp16)(aTypeCode === Fp16.typeCode.U && bTypeCode === Fp16.typeCode.U && cTypeCode === Fp16.typeCode.U),
      Option.when(matrixExtension.enableFp16Fp32)(aTypeCode === Fp16.typeCode.U && bTypeCode === Fp16.typeCode.U && cTypeCode === Fp32.typeCode.U),
      Option.when(matrixExtension.enableBf16Fp32)(aTypeCode === Bf16.typeCode.U && bTypeCode === Bf16.typeCode.U && cTypeCode === Fp32.typeCode.U),
      Option.when(matrixExtension.enableTf32Fp32)(aTypeCode === Tf32.typeCode.U && bTypeCode === Tf32.typeCode.U && cTypeCode === Fp32.typeCode.U),
      Option.when(matrixExtension.enableFp32Fp32)(aTypeCode === Fp32.typeCode.U && bTypeCode === Fp32.typeCode.U && cTypeCode === Fp32.typeCode.U)
    ).flatten

    if (supported.nonEmpty) supported.reduce(_ || _) else false.B
  }

  private def isInt8Type(mcfgType: McfgSemanticType): Boolean = mcfgType == Int8 || mcfgType == UInt8

  private def isInt4Type(mcfgType: McfgSemanticType): Boolean = mcfgType == Int4 || mcfgType == UInt4

  private def sameFp8Type(aType: McfgSemanticType, bType: McfgSemanticType): Boolean = {
    (aType == Fp8E5M2 && bType == Fp8E5M2) || (aType == Fp8E4M3 && bType == Fp8E4M3)
  }

  private def isInt8Type(typeCode: UInt): Bool = typeCode === Int8.typeCode.U || typeCode === UInt8.typeCode.U

  private def isInt4Type(typeCode: UInt): Bool = typeCode === Int4.typeCode.U || typeCode === UInt4.typeCode.U

  private def sameFp8Type(aTypeCode: UInt, bTypeCode: UInt): Bool = {
    (aTypeCode === Fp8E5M2.typeCode.U && bTypeCode === Fp8E5M2.typeCode.U) ||
      (aTypeCode === Fp8E4M3.typeCode.U && bTypeCode === Fp8E4M3.typeCode.U)
  }
}

class McfgEntry(implicit p: Parameters) extends Bundle {
  val raw = UInt(p(XLen).W)

  def typeCode: UInt = raw(3, 0)
  def tableSel: UInt = raw(7, 4)
  def payload: UInt = raw(p(XLen) - 1, 8)
  def supported: Bool = McfgHelpers.isSupported(typeCode, tableSel)
  def definedByPdf: Bool = McfgHelpers.isPdfDefined(typeCode, tableSel)
}

class McfgState(implicit p: Parameters) extends Bundle {
  val entries = Vec(McfgHelpers.entryCount, new McfgEntry)
}

class McfgCommit(implicit p: Parameters) extends Bundle {
  val sel = UInt(3.W)
  val entry = new McfgEntry
}

class McfgReadView(implicit p: Parameters) extends Bundle {
  val sel = UInt(3.W)
  val entry = new McfgEntry
}

class McfgGenIO(implicit p: Parameters) extends XSBundle {
  val walkToArchMcfg = Input(Bool())
  val walkMcfg = Flipped(Vec(CommitWidth, Valid(new McfgCommit)))
  val mcfg = Output(new McfgState)
  val commitMcfg = Flipped(Vec(CommitWidth, Valid(new McfgCommit)))
}

class McfgGen(implicit p: Parameters) extends XSModule {
  val io = IO(new McfgGenIO)

  private val mcfgArch = RegInit(0.U.asTypeOf(new McfgState))
  private val mcfgSpec = RegInit(0.U.asTypeOf(new McfgState))

  private val mcfgArchNext = WireInit(mcfgArch)
  private val mcfgSpecNext = WireInit(mcfgSpec)

  mcfgArch := mcfgArchNext
  mcfgSpec := mcfgSpecNext

  private def updateMany(state: McfgState, commits: Vec[Valid[McfgCommit]]): McfgState = {
    val next = WireInit(state)
    for (commit <- commits) {
      when(commit.valid) {
        next.entries(McfgHelpers.selectorToIndex(commit.bits.sel)) := commit.bits.entry
      }
    }
    next
  }

  private val hasCommitMcfg = VecInit(io.commitMcfg.map(_.valid)).asUInt.orR
  private val hasWalkMcfg = VecInit(io.walkMcfg.map(_.valid)).asUInt.orR
  private val walkBase = WireInit(mcfgSpec)

  mcfgArchNext := updateMany(mcfgArch, io.commitMcfg)
  walkBase := Mux(io.walkToArchMcfg, mcfgArchNext, mcfgSpec)

  when(hasCommitMcfg) {
    mcfgSpecNext := updateMany(mcfgSpec, io.commitMcfg)
  }

  when(hasWalkMcfg) {
    mcfgSpecNext := updateMany(walkBase, io.walkMcfg)
  }.elsewhen(io.walkToArchMcfg) {
    mcfgSpecNext := mcfgArchNext
  }

  io.mcfg := mcfgSpec
}
