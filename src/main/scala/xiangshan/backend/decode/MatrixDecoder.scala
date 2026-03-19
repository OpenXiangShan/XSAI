package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import cute.MatrixIsaParams
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MSETtilexOpType, UopSplitType, SelImm, MldstOpType, MarithOpType, MmvefOpType, FenceOpType, CSROpType}
import freechips.rocketchip.amba.ahb.AHBParameters.transBits
import xiangshan.MmulOpType
import scala.collection.mutable.ArrayBuffer

// Set mtilem/n/k
case class MSETTXINST(txi: Boolean, fuOp: BitPat, flushPipe: Boolean, blockBack: Boolean, selImm: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val src1: BitPat = if (txi) SrcType.imm else SrcType.xp
    XSDecode(src1, SrcType.X, SrcType.X, FuType.msetmtilexiwf, fuOp, selImm, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = blockBack, flushPipe = flushPipe).generate()
  }
}

case class MLS(fuOp: BitPat, transposed: Boolean = false) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mls
    val src1: BitPat = SrcType.xp
    val src2: BitPat = SrcType.xp
    val src3: BitPat = SrcType.mx
    // src4: BitPat = SrcType.mp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.IMM_MATRIXREG, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MMUL(fuOp: BitPat) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.mma
    val src1: BitPat = SrcType.no
    val src2: BitPat = SrcType.no
    val src3: BitPat = SrcType.mx // always mtilem
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.IMM_MATRIXREG, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class MARITH(fuOp: BitPat, hasSrc1: Boolean = true, hasSrc2: Boolean = true) extends XSDecodeBase {
  def generate(): List[BitPat] = {
    val fu = FuType.marith
    val src1: BitPat = if (hasSrc1) SrcType.mx else SrcType.X
    val src2: BitPat = if (hasSrc2) SrcType.mx else SrcType.X
    XSDecode(src1, src2, SrcType.X, fu, fuOp, SelImm.IMM_MATRIXREG, UopSplitType.X,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

object MatrixDecoder extends DecodeConstants {
  val mset: Array[(BitPat, XSDecodeBase)] = Array(
    MINIT      -> XSDecode(SrcType.X, SrcType.X, SrcType.X,
      FuType.csr, CSROpType.minit, SelImm.X, xWen = F, noSpec = T, blockBack = T),
    // Set tilem/n/k
    MSETTILEM  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilem_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEMI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilem_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEN  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilen_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILENI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilen_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEK  -> MSETTXINST(txi = F, fuOp = MSETtilexOpType.umsettilek_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEKI -> MSETTXINST(txi = T, fuOp = MSETtilexOpType.umsettilek_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
  )

  private val mlsAB8: Array[(BitPat, XSDecodeBase)] = Array(
    // Load left matrix, A
    MLAE8 -> MLS(MldstOpType.mlae8),
    // Load right matrix, B
    MLBE8 -> MLS(MldstOpType.mlbe8),
    // Load transposed left matrix, A
    MLATE8 -> MLS(MldstOpType.mlate8),
    // Load transposed right matrix, B
    MLBTE8 -> MLS(MldstOpType.mlbte8),
    // Store left matrix, A
    MSAE8 -> MLS(MldstOpType.msae8),
    // Store right matrix, B
    MSBE8 -> MLS(MldstOpType.msbe8),
    // Store transposed left matrix, A
    MSATE8 -> MLS(MldstOpType.msate8),
    // Store transposed right matrix, B
    MSBTE8 -> MLS(MldstOpType.msbte8),
  )

  private val mlsAB16: Array[(BitPat, XSDecodeBase)] = Array(
    MLAE16 -> MLS(MldstOpType.mlae16),
    MLBE16 -> MLS(MldstOpType.mlbe16),
    MLATE16 -> MLS(MldstOpType.mlate16),
    MLBTE16 -> MLS(MldstOpType.mlbte16),
    MSAE16 -> MLS(MldstOpType.msae16),
    MSBE16 -> MLS(MldstOpType.msbe16),
    MSATE16 -> MLS(MldstOpType.msate16),
    MSBTE16 -> MLS(MldstOpType.msbte16),
  )

  private val mlsAB32: Array[(BitPat, XSDecodeBase)] = Array(
    MLAE32 -> MLS(MldstOpType.mlae32),
    MLBE32 -> MLS(MldstOpType.mlbe32),
    MLATE32 -> MLS(MldstOpType.mlate32),
    MLBTE32 -> MLS(MldstOpType.mlbte32),
    MSAE32 -> MLS(MldstOpType.msae32),
    MSBE32 -> MLS(MldstOpType.msbe32),
    MSATE32 -> MLS(MldstOpType.msate32),
    MSBTE32 -> MLS(MldstOpType.msbte32),
  )

  private val mlsAB64: Array[(BitPat, XSDecodeBase)] = Array(
    MLAE64 -> MLS(MldstOpType.mlae64),
    MLBE64 -> MLS(MldstOpType.mlbe64),
    MLATE64 -> MLS(MldstOpType.mlate64),
    MLBTE64 -> MLS(MldstOpType.mlbte64),
    MSAE64 -> MLS(MldstOpType.msae64),
    MSBE64 -> MLS(MldstOpType.msbe64),
    MSATE64 -> MLS(MldstOpType.msate64),
    MSBTE64 -> MLS(MldstOpType.msbte64),
  )

  private val mlsDst8: Array[(BitPat, XSDecodeBase)] = Array(
    MLCE8 -> MLS(MldstOpType.mlce8),
    MLCTE8 -> MLS(MldstOpType.mlcte8),
    MSCE8 -> MLS(MldstOpType.msce8),
    MSCTE8 -> MLS(MldstOpType.mscte8),
  )

  private val mlsDst16: Array[(BitPat, XSDecodeBase)] = Array(
    MLCE16 -> MLS(MldstOpType.mlce16),
    MLCTE16 -> MLS(MldstOpType.mlcte16),
    MSCE16 -> MLS(MldstOpType.msce16),
    MSCTE16 -> MLS(MldstOpType.mscte16),
  )

  private val mlsDst32: Array[(BitPat, XSDecodeBase)] = Array(
    MLCE32 -> MLS(MldstOpType.mlce32),
    MLCTE32 -> MLS(MldstOpType.mlcte32),
    MSCE32 -> MLS(MldstOpType.msce32),
    MSCTE32 -> MLS(MldstOpType.mscte32),
  )

  private val mlsDst64: Array[(BitPat, XSDecodeBase)] = Array(
    MLCE64 -> MLS(MldstOpType.mlce64),
    MLCTE64 -> MLS(MldstOpType.mlcte64),
    MSCE64 -> MLS(MldstOpType.msce64),
    // Store transposed output matrix, C
    MSCTE64 -> MLS(MldstOpType.mscte64),
  )

  private val mlsWholeTile: Array[(BitPat, XSDecodeBase)] = Array(
    // Load/store whole tile matrix, always kept regardless of precision config.
    MLME8 -> MLS(MldstOpType.mlme8),
    MLME16 -> MLS(MldstOpType.mlme16),
    MLME32 -> MLS(MldstOpType.mlme32),
    MLME64 -> MLS(MldstOpType.mlme64),
    MSME8 -> MLS(MldstOpType.msme8),
    MSME16 -> MLS(MldstOpType.msme16),
    MSME32 -> MLS(MldstOpType.msme32),
    MSME64 -> MLS(MldstOpType.msme64),
  )

  private def mlsByMatrixExtension(matrixExtension: MatrixIsaParams): Array[(BitPat, XSDecodeBase)] = {
    val enabled = ArrayBuffer[(BitPat, XSDecodeBase)]()

    if (matrixExtension.enable8BitSrc || matrixExtension.enable4BitSrc) {
      enabled ++= mlsAB8
    }
    if (matrixExtension.enable16BitSrc) {
      enabled ++= mlsAB16
    }
    if (matrixExtension.enable32BitSrc) {
      enabled ++= mlsAB32
    }
    if (matrixExtension.enable64BitSrc) {
      enabled ++= mlsAB64
    }

    if (matrixExtension.enable8BitDst) {
      enabled ++= mlsDst8
    }
    if (matrixExtension.enable16BitDst) {
      enabled ++= mlsDst16
    }
    if (matrixExtension.enable32BitDst) {
      enabled ++= mlsDst32
    }
    if (matrixExtension.enable64BitDst) {
      enabled ++= mlsDst64
    }

    enabled ++= mlsWholeTile
    enabled.toArray
  }

  private val mls: Array[(BitPat, XSDecodeBase)] =
    mlsAB8 ++ mlsAB16 ++ mlsAB32 ++ mlsAB64 ++
      mlsDst8 ++ mlsDst16 ++ mlsDst32 ++ mlsDst64 ++
      mlsWholeTile

  private val mmulFp8Fp16: Array[(BitPat, XSDecodeBase)] = Array(
    // e5m2/e4m3 -> fp16
    MFMACC_H_E5 -> MMUL(MmulOpType.mma_e5m2_fp16),
    MFMACC_H_E4 -> MMUL(MmulOpType.mma_e4m3_fp16),
  )

  private val mmulFp8Bf16: Array[(BitPat, XSDecodeBase)] = Array(
    // e5m2/e4m3 -> bf16
    MFMACC_BF16_E5 -> MMUL(MmulOpType.mma_e5m2_bf16),
    MFMACC_BF16_E4 -> MMUL(MmulOpType.mma_e4m3_bf16),
  )

  private val mmulFp8Fp32: Array[(BitPat, XSDecodeBase)] = Array(
    // e5m2/e4m3 -> fp32
    MFMACC_S_E5 -> MMUL(MmulOpType.mma_e5m2_fp32),
    MFMACC_S_E4 -> MMUL(MmulOpType.mma_e4m3_fp32),
  )

  private val mmulFp16Fp16: Array[(BitPat, XSDecodeBase)] = Array(
    MFMACC_H -> MMUL(MmulOpType.mma_fp16_fp16),
  )

  private val mmulFp16Fp32: Array[(BitPat, XSDecodeBase)] = Array(
    MFMACC_S_H -> MMUL(MmulOpType.mma_fp16_fp32),
  )

  private val mmulBf16Fp32: Array[(BitPat, XSDecodeBase)] = Array(
    MFMACC_S_BF16 -> MMUL(MmulOpType.mma_bf16_fp32),
  )

  private val mmulTf32Fp32: Array[(BitPat, XSDecodeBase)] = Array(
    MFMACC_S_TF32 -> MMUL(MmulOpType.mma_tf32_fp32),
  )

  private val mmulFp32Fp32: Array[(BitPat, XSDecodeBase)] = Array(
    MFMACC_S -> MMUL(MmulOpType.mma_fp32_fp32),
  )

  private val mmulInt8Int32: Array[(BitPat, XSDecodeBase)] = Array(
    // int8/uint8 combinations -> int32
    MMACC_W_B -> MMUL(MmulOpType.mma_int8_int32),
    MMACCU_W_B -> MMUL(MmulOpType.mma_uint8_int32),
    MMACCUS_W_B -> MMUL(MmulOpType.mma_usint8_int32),
    MMACCSU_W_B -> MMUL(MmulOpType.mma_suint8_uint32),
  )

  private val mmulInt4Int32: Array[(BitPat, XSDecodeBase)] = Array(
    // int4/uint4 combinations -> int32
    PMMACC_W_B -> MMUL(MmulOpType.mma_int4_int32),
    PMMACCU_W_B -> MMUL(MmulOpType.mma_uint4_uint32),
    PMMACCUS_W_B -> MMUL(MmulOpType.mma_usint4_uint32),
    PMMACCSU_W_B -> MMUL(MmulOpType.mma_suint4_int32),
  )

  private val mmulInt84Int32: Array[(BitPat, XSDecodeBase)] = Array(
    // int8/uint8 mixed with int4/uint4 -> int32
    MMACC_W_BP -> MMUL(MmulOpType.placeholder),
    MMACCU_W_BP -> MMUL(MmulOpType.placeholder),
  )

  private def mmulByMatrixExtension(matrixExtension: MatrixIsaParams): Array[(BitPat, XSDecodeBase)] = {
    val enabled = ArrayBuffer[(BitPat, XSDecodeBase)]()

    if (matrixExtension.enableFp8Fp16) {
      enabled ++= mmulFp8Fp16
    }
    if (matrixExtension.enableFp8Bf16) {
      enabled ++= mmulFp8Bf16
    }
    if (matrixExtension.enableFp8Fp32) {
      enabled ++= mmulFp8Fp32
    }
    if (matrixExtension.enableFp16Fp16) {
      enabled ++= mmulFp16Fp16
    }
    if (matrixExtension.enableFp16Fp32) {
      enabled ++= mmulFp16Fp32
    }
    if (matrixExtension.enableBf16Fp32) {
      enabled ++= mmulBf16Fp32
    }
    if (matrixExtension.enableTf32Fp32) {
      enabled ++= mmulTf32Fp32
    }
    if (matrixExtension.enableFp32Fp32) {
      enabled ++= mmulFp32Fp32
    }
    if (matrixExtension.enableInt8Int32) {
      enabled ++= mmulInt8Int32
    }
    if (matrixExtension.enableInt4Int32) {
      enabled ++= mmulInt4Int32
    }
    if (matrixExtension.enableInt84Int32) {
      enabled ++= mmulInt84Int32
    }

    enabled.toArray
  }

  private val mmul: Array[(BitPat, XSDecodeBase)] =
    mmulFp8Fp16 ++ mmulFp8Bf16 ++ mmulFp8Fp32 ++
      mmulFp16Fp16 ++ mmulFp16Fp32 ++ mmulBf16Fp32 ++ mmulTf32Fp32 ++ mmulFp32Fp32 ++
      mmulInt8Int32 ++ mmulInt4Int32 ++ mmulInt84Int32

  val marith: Array[(BitPat, XSDecodeBase)] = Array(
    MZERO -> MARITH(MarithOpType.mzero1r, hasSrc1 = false, hasSrc2 = false),
  )

  val msync: Array[(BitPat, XSDecodeBase)] = Array(
    MSYNCRESET -> XSDecode(SrcType.pc, SrcType.imm, SrcType.X,
      FuType.fence, FenceOpType.msyncregreset, SelImm.IMM_MSETVAL,
      noSpec = T
    ),
    MRELEASE -> XSDecode(SrcType.pc, SrcType.imm, SrcType.X,
      FuType.mrelease, "b0".U, SelImm.IMM_MSETVAL
    ),
    MACQUIRE -> XSDecode(SrcType.xp, SrcType.imm, SrcType.X,
      FuType.fence, FenceOpType.macquire, SelImm.IMM_MSETVAL,
      noSpec = T, blockBack = T, flushPipe = T
    )
  )

  def table(matrixExtension: MatrixIsaParams): Array[(BitPat, List[BitPat])] = {
    val decodeArrayWithConfig = mset ++ mlsByMatrixExtension(matrixExtension) ++ mmulByMatrixExtension(matrixExtension) ++ marith ++ msync
    decodeArrayWithConfig.map(x => (x._1, x._2.generate()))
  }

  // Keep a full static superset for DecodeConstants compatibility.
  // DecodeUnit uses table(matrixExtension) as the effective matrix decode path.
  override val decodeArray: Array[(BitPat, XSDecodeBase)] = mset ++ mls ++ mmul ++ marith ++ msync
}
