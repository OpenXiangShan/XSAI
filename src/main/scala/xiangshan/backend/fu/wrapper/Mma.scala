package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.{AmuMmaIO, MtypeMSew, AmuCtrlIO}
import xiangshan.MmulOpType

class Mma(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val mtilem = in.data.src(2)
  protected val mtilen = in.data.src(3)
  protected val mtilek = in.data.src(4)
  protected val realFuOpType = WireInit(in.ctrl.fuOpType)

  val illegal_regidx = !in.data.imm(2) || in.data.imm(5) || in.data.imm(8)
  
  private val mtilenChecks = Seq(
    Option.when(MatrixExtension.enable4BitDst)(
      MmulOpType.isToE4(in.ctrl.fuOpType) -> (mtilek > (ARLEN / 4).U)
    ),
    Option.when(MatrixExtension.enable8BitDst)(
      MmulOpType.isToE8(in.ctrl.fuOpType) -> (mtilek > (ARLEN / 8).U)
    ),
    Option.when(MatrixExtension.enable16BitDst)(
      MmulOpType.isToE16(in.ctrl.fuOpType) -> (mtilek > (ARLEN / 16).U)
    ),
    Option.when(MatrixExtension.enable32BitDst)(
      MmulOpType.isToE32(in.ctrl.fuOpType) -> (mtilek > (ARLEN / 32).U)
    ),
  ).flatten
  private val mtilekChecks = Seq(
    Option.when(MatrixExtension.enable4BitSrc)(
      MmulOpType.isFromE4(in.ctrl.fuOpType) -> (mtilek > (TRLEN / 4).U)
    ),
    Option.when(MatrixExtension.enable8BitSrc)(
      MmulOpType.isFromE8(in.ctrl.fuOpType) -> (mtilek > (TRLEN / 8).U)
    ),
    Option.when(MatrixExtension.enable16BitSrc)(
      MmulOpType.isFromE16(in.ctrl.fuOpType) -> (mtilek > (TRLEN / 16).U)
    ),
    Option.when(MatrixExtension.enable32BitSrc)(MmulOpType.isFromE32(in.ctrl.fuOpType) -> (mtilek > (TRLEN / 32).U)),
  ).flatten

  private val fromTypeSignChecks = Seq(
    Option.when(MatrixExtension.enable4BitSrc)(MmulOpType.isFromE4(realFuOpType) -> "b0".U),
    Option.when(MatrixExtension.enable8BitSrc)(MmulOpType.isFromE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt),
    Option.when(MatrixExtension.enable16BitSrc)(MmulOpType.isFromE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt),
    Option.when(MatrixExtension.enable32BitSrc)(MmulOpType.isFromE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt),
  ).flatten

  private val toTypeSignChecks = Seq(
    Option.when(MatrixExtension.enable4BitDst)(MmulOpType.isToE4(realFuOpType) -> "b0".U),
    Option.when(MatrixExtension.enable8BitDst)(MmulOpType.isToE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt),
    Option.when(MatrixExtension.enable16BitDst)(MmulOpType.isToE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt),
    Option.when(MatrixExtension.enable32BitDst)(MmulOpType.isToE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt),
  ).flatten

  val illegal_mtilem = mtilem > ROWNUM.U
  val illegal_mtilen = mtilen > ROWNUM.U && (if (mtilenChecks.nonEmpty) Mux1H(mtilenChecks) else false.B)
  val illegal_mtilek = if (mtilekChecks.nonEmpty) Mux1H(mtilekChecks) else false.B

  val output = Wire(new AmuMmaIO)
  dontTouch(output)
  output.ms1    := in.data.imm(5, 3)
  output.ms2    := in.data.imm(8, 6)
  output.md     := in.data.imm(2, 0)

  output.typed  := Cat(
    MmulOpType.isFloat(realFuOpType) && MmulOpType.isToE16(realFuOpType) && MmulOpType.isBf16(realFuOpType),
    MmulOpType.getToType(realFuOpType)
  )
  output.types1  := Cat(
    Mux(MmulOpType.isFloat(realFuOpType),
      Mux1H(Seq(
        MmulOpType.isFromE4(realFuOpType) -> "b0".U,
        MmulOpType.isFromE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt,
        MmulOpType.isFromE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt,
        MmulOpType.isFromE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt,
      )),
      MmulOpType.ms1sign(realFuOpType).asUInt
    ),
    MmulOpType.getFromType(realFuOpType)
  )
  output.types2  := Cat(
    Mux(MmulOpType.isFloat(realFuOpType),
      Mux1H(Seq(
        MmulOpType.isToE4(realFuOpType) -> "b0".U,
        MmulOpType.isToE8(realFuOpType) -> MmulOpType.isE4m3(realFuOpType).asUInt,
        MmulOpType.isToE16(realFuOpType) -> MmulOpType.isBf16(realFuOpType).asUInt,
        MmulOpType.isToE32(realFuOpType) -> MmulOpType.isTf32(realFuOpType).asUInt,
      )),
      MmulOpType.ms2sign(realFuOpType).asUInt
    ),
    MmulOpType.getFromType(realFuOpType)
  )

  output.sat    := io.xmsaten.get.asBool
  output.rm     := Mux(MmulOpType.isFloat(realFuOpType), io.xmfrm.get, io.xmxrm.get)
  output.mtilem := mtilem
  output.mtilen := mtilen
  output.mtilek := mtilek
  output.isfp   := MmulOpType.isFloat(realFuOpType)
  
  out.res.data := output.asUInt

  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.mmaOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
  out.ctrl.exceptionVec.get := 0.U.asTypeOf(out.ctrl.exceptionVec.get)
  out.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := illegal_regidx || illegal_mtilem || illegal_mtilen || illegal_mtilek
  if (env.EnableDifftest) {
    // It will be filled in ROB.
    out.ctrl.amuCtrl.get.pc.get := DontCare
    out.ctrl.amuCtrl.get.coreid.get := DontCare
  }
}
