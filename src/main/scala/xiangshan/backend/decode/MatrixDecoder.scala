package xiangshan.backend.decode

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import cute.{CuteParamsKey, MatrixIsaParams}
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.fu.FuType
import xiangshan.{SrcType, MSetOpType, UopSplitType, SelImm, MldstOpType, MarithOpType, FenceOpType, CSROpType}
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
  // Proposal-12 kind=11 is reserved except these two non-transposed loads.
  val MLASCALE = BitPat("b0011000??????????00100???0101011")
  val MLBSCALE = BitPat("b1011000??????????00100???0101011")
  // Explicit experimental encodings, not ordinary loads or mcfg reserved bits.
  val MLACVT_F32_MX = BitPat("b0011010??????????00100???0101011")
  val MLBCVT_F32_MX = BitPat("b1011010??????????00100???0101011")
  // Experimental custom-1 candidates, NOT an official Zames allocation.
  // funct7=0x1c/0x5c, funct3=1, rd=tr0..3; base/stride remain rs1/rs2.
  val MLADYN_F32_MX = BitPat("b0011100??????????001000??0101011")
  val MLBDYN_F32_MX = BitPat("b1011100??????????001000??0101011")

  // Non-matrix CPU configurations and isolated harnesses need not define CUTE.
  def convertF32Enabled(p: Parameters): Boolean =
    p.lift(CuteParamsKey).exists(_.ExperimentalMxfpConvertLoad)
  def dynamicQuantEnabled(p: Parameters): Boolean =
    p.lift(CuteParamsKey).exists(c => c.ExperimentalMxfpConvertLoad && c.ExperimentalMxfpDynamicQuant)

  val mset: Array[(BitPat, XSDecodeBase)] = Array(
    // mcfg control
    MSETCFG    -> XSDecode(SrcType.xp, SrcType.X, SrcType.X,
                           FuType.mcfg, MSetOpType.msetcfg, SelImm.X, xWen = F,
                           noSpec = F, blockBack = T, flushPipe = T),
    MGETCFG    -> XSDecode(SrcType.no, SrcType.X, SrcType.X,
                           FuType.mcfg, MSetOpType.mgetcfg, SelImm.X, xWen = T,
                           noSpec = F, blockBack = F),
    MINIT      -> XSDecode(SrcType.X, SrcType.X, SrcType.X,
      FuType.csr, CSROpType.minit, SelImm.X, xWen = F, noSpec = T, blockBack = T),
    // Set tilem/n/k
    MSETTILEM  -> MSETTXINST(txi = F, fuOp = MSetOpType.settilem_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEMI -> MSETTXINST(txi = T, fuOp = MSetOpType.settilem_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEN  -> MSETTXINST(txi = F, fuOp = MSetOpType.settilen_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILENI -> MSETTXINST(txi = T, fuOp = MSetOpType.settilen_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
    MSETTILEK  -> MSETTXINST(txi = F, fuOp = MSetOpType.settilek_x, flushPipe = F, blockBack = F, selImm = SelImm.X),
    MSETTILEKI -> MSETTXINST(txi = T, fuOp = MSetOpType.settilek_i, flushPipe = F, blockBack = F, selImm = SelImm.IMM_MSET),
  )

  private val mls: Array[(BitPat, XSDecodeBase)] = Array(
    // Load left matrix, A
    MLA -> MLS(MldstOpType.mla),
    // Load right matrix, B
    MLB -> MLS(MldstOpType.mlb),
    MLASCALE -> MLS(MldstOpType.mlascale),
    MLBSCALE -> MLS(MldstOpType.mlbscale),
    // Load transposed left matrix, A
    MLAT -> MLS(MldstOpType.mlat),
    // Load transposed right matrix, B
    MLBT -> MLS(MldstOpType.mlbt),
    // Store left matrix, A
    MSA -> MLS(MldstOpType.msa),
    // Store right matrix, B
    MSB -> MLS(MldstOpType.msb),
    // Store transposed left matrix, A
    MSAT -> MLS(MldstOpType.msat),
    // Store transposed right matrix, B
    MSBT -> MLS(MldstOpType.msbt),

    MLC -> MLS(MldstOpType.mlc),
    MLCT -> MLS(MldstOpType.mlct),
    MSC -> MLS(MldstOpType.msc),
    MSCT -> MLS(MldstOpType.msct),

    // Load/store whole tile matrix, always kept regardless of precision config.
    MLA_WHOLE -> MLS(MldstOpType.mlaWhole),
    MLB_WHOLE -> MLS(MldstOpType.mlbWhole),
    MLC_WHOLE -> MLS(MldstOpType.mlcWhole),
    MSA_WHOLE -> MLS(MldstOpType.msaWhole),
    MSB_WHOLE -> MLS(MldstOpType.msbWhole),
    MSC_WHOLE -> MLS(MldstOpType.mscWhole),
  )

  private val mmul: Array[(BitPat, XSDecodeBase)] = Array(
    MMACC -> MMUL(MmulOpType.placeholder)
  )

  private val convertingLoads: Array[(BitPat, XSDecodeBase)] = Array(
    MLACVT_F32_MX -> MLS(MldstOpType.mlacvtF32Mx),
    MLBCVT_F32_MX -> MLS(MldstOpType.mlbcvtF32Mx)
  )

  private val dynamicLoads: Array[(BitPat, XSDecodeBase)] = Array(
    MLADYN_F32_MX -> MLS(MldstOpType.mladynF32Mx),
    MLBDYN_F32_MX -> MLS(MldstOpType.mlbdynF32Mx)
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
      noSpec = T, blockBack = T, flushPipe = F
    ),
    MFENCE -> XSDecode(SrcType.pc, SrcType.imm, SrcType.X,
      FuType.fence, FenceOpType.mfence, SelImm.X,
      noSpec = T, blockBack = T, flushPipe = F
      // It only flushes the SBuffer in XSAI, but younger stores must not enter the SQ.
    )
  )

  def table(matrixExtension: MatrixIsaParams, experimentalMxfpConvertLoad: Boolean = false,
            experimentalMxfpDynamicQuant: Boolean = false): Array[(BitPat, List[BitPat])] = {
    val decodeArrayWithConfig = mset ++ mls ++
      (if (experimentalMxfpConvertLoad) convertingLoads else Array.empty[(BitPat, XSDecodeBase)]) ++
      (if (experimentalMxfpConvertLoad && experimentalMxfpDynamicQuant) dynamicLoads else Array.empty[(BitPat, XSDecodeBase)]) ++
      mmul ++ marith ++ msync
    decodeArrayWithConfig.map(x => (x._1, x._2.generate()))
  }

  // Keep a full static superset for DecodeConstants compatibility.
  // DecodeUnit uses the explicitly gated table(...) as the effective path.
  override val decodeArray: Array[(BitPat, XSDecodeBase)] =
    mset ++ mls ++ convertingLoads ++ dynamicLoads ++ mmul ++ marith ++ msync
}
