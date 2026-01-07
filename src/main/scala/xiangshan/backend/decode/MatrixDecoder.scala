package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MSETtilexOpType, UopSplitType, SelImm, MldstOpType, MarithOpType, MmvefOpType, FenceOpType, CSROpType}
import freechips.rocketchip.amba.ahb.AHBParameters.transBits
import xiangshan.MmulOpType

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

  val mls: Array[(BitPat, XSDecodeBase)] = Array(
    // Load left matrix, A
    MLAE8 -> MLS(MldstOpType.mlae8),
    MLAE16 -> MLS(MldstOpType.mlae16),
    MLAE32 -> MLS(MldstOpType.mlae32),
    MLAE64 -> MLS(MldstOpType.mlae64),
    // Load right matrix, B
    MLBE8 -> MLS(MldstOpType.mlbe8),
    MLBE16 -> MLS(MldstOpType.mlbe16),
    MLBE32 -> MLS(MldstOpType.mlbe32),
    MLBE64 -> MLS(MldstOpType.mlbe64),
    // Load output matrix, C
    MLCE8 -> MLS(MldstOpType.mlce8),
    MLCE16 -> MLS(MldstOpType.mlce16),
    MLCE32 -> MLS(MldstOpType.mlce32),
    MLCE64 -> MLS(MldstOpType.mlce64),
    // Load a whole tile matrix from memory without considering the size
    MLME8 -> MLS(MldstOpType.mlme8),
    MLME16 -> MLS(MldstOpType.mlme16),
    MLME32 -> MLS(MldstOpType.mlme32),
    MLME64 -> MLS(MldstOpType.mlme64),
    // Load transposed left matrix, A
    MLATE8 -> MLS(MldstOpType.mlate8),
    MLATE16 -> MLS(MldstOpType.mlate16),
    MLATE32 -> MLS(MldstOpType.mlate32),
    MLATE64 -> MLS(MldstOpType.mlate64),
    // Load transposed right matrix, B
    MLBTE8 -> MLS(MldstOpType.mlbte8),
    MLBTE16 -> MLS(MldstOpType.mlbte16),
    MLBTE32 -> MLS(MldstOpType.mlbte32),
    MLBTE64 -> MLS(MldstOpType.mlbte64),
    // Load transposed output matrix, C
    MLCTE8 -> MLS(MldstOpType.mlcte8),
    MLCTE16 -> MLS(MldstOpType.mlcte16),
    MLCTE32 -> MLS(MldstOpType.mlcte32),
    MLCTE64 -> MLS(MldstOpType.mlcte64),
    // Store left matrix, A
    MSAE8 -> MLS(MldstOpType.msae8),
    MSAE16 -> MLS(MldstOpType.msae16),
    MSAE32 -> MLS(MldstOpType.msae32),
    MSAE64 -> MLS(MldstOpType.msae64),
    // Store right matrix, B
    MSBE8 -> MLS(MldstOpType.msbe8),
    MSBE16 -> MLS(MldstOpType.msbe16),
    MSBE32 -> MLS(MldstOpType.msbe32),
    MSBE64 -> MLS(MldstOpType.msbe64),
    // Store output matrix, C
    MSCE8 -> MLS(MldstOpType.msce8),
    MSCE16 -> MLS(MldstOpType.msce16),
    MSCE32 -> MLS(MldstOpType.msce32),
    MSCE64 -> MLS(MldstOpType.msce64),
    // Store a whole tile matrix to memory without considering the size
    MSME8 -> MLS(MldstOpType.msme8),
    MSME16 -> MLS(MldstOpType.msme16),
    MSME32 -> MLS(MldstOpType.msme32),
    MSME64 -> MLS(MldstOpType.msme64),
    // Store transposed left matrix, A
    MSATE8 -> MLS(MldstOpType.msate8),
    MSATE16 -> MLS(MldstOpType.msate16),
    MSATE32 -> MLS(MldstOpType.msate32),
    MSATE64 -> MLS(MldstOpType.msate64),
    // Store transposed right matrix, B
    MSBTE8 -> MLS(MldstOpType.msbte8),
    MSBTE16 -> MLS(MldstOpType.msbte16),
    MSBTE32 -> MLS(MldstOpType.msbte32),
    MSBTE64 -> MLS(MldstOpType.msbte64),
    // Store transposed output matrix, C
    MSCTE8 -> MLS(MldstOpType.mscte8),
    MSCTE16 -> MLS(MldstOpType.mscte16),
    MSCTE32 -> MLS(MldstOpType.mscte32),
    MSCTE64 -> MLS(MldstOpType.mscte64),
  )

  val mmul: Array[(BitPat, XSDecodeBase)] = Array(
    // e5m2 -> fp16
    MFMACC_H_E5 -> MMUL(MmulOpType.mma_e5m2_fp16),
    // e4m3 -> fp16
    MFMACC_H_E4 -> MMUL(MmulOpType.mma_e4m3_fp16),
    // e5m2 -> bf16
    MFMACC_BF16_E5 -> MMUL(MmulOpType.mma_e5m2_bf16),
    // e4m3 -> bf16
    MFMACC_BF16_E4 -> MMUL(MmulOpType.mma_e4m3_bf16),
    // e5m2 -> fp32
    MFMACC_S_E5 -> MMUL(MmulOpType.mma_e5m2_fp32),
    // e4m3 -> fp32
    MFMACC_S_E4 -> MMUL(MmulOpType.mma_e4m3_fp32),
    // fp16 -> fp16
    MFMACC_H -> MMUL(MmulOpType.mma_fp16_fp16),
    // fp16 -> fp32
    MFMACC_S_H -> MMUL(MmulOpType.mma_fp16_fp32),
    // bf16 -> fp32
    MFMACC_S_BF16 -> MMUL(MmulOpType.mma_bf16_fp32),
    // tf32 -> fp32
    MFMACC_S_TF32 -> MMUL(MmulOpType.placeholder),
    // fp32 -> fp32
    MFMACC_S -> MMUL(MmulOpType.placeholder),

    // int8, int8 -> int32
    MMACC_W_B -> MMUL(MmulOpType.mma_int8_int32),
    // uint8, uint8 -> int32
    MMACCU_W_B -> MMUL(MmulOpType.mma_uint8_int32),
    // uint8, int8 -> int32
    MMACCUS_W_B -> MMUL(MmulOpType.mma_usint8_int32),
    // int8, uint8 -> int32
    MMACCSU_W_B -> MMUL(MmulOpType.mma_suint8_uint32),
    // int4, int4 -> int32
    PMMACC_W_B -> MMUL(MmulOpType.mma_int4_int32),
    // uint4, uint4 -> int32
    PMMACCU_W_B -> MMUL(MmulOpType.mma_uint4_uint32),
    // uint4, int4 -> int32
    PMMACCUS_W_B -> MMUL(MmulOpType.mma_usint4_uint32),
    // int4, uint4 -> int32
    PMMACCSU_W_B -> MMUL(MmulOpType.mma_suint4_int32),
    
    // int8, int4 -> int32
    MMACC_W_BP -> MMUL(MmulOpType.placeholder),
    // uint8, uint4 -> int32
    MMACCU_W_BP -> MMUL(MmulOpType.placeholder),
  )

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

  override val decodeArray: Array[(BitPat, XSDecodeBase)] = mset ++ mls ++ mmul ++ marith ++ msync
}