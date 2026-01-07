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

  val illegal_regidx = !in.data.imm(2) || in.data.imm(5) || in.data.imm(8)
  val illegal_mtilem = mtilem <= ROWNUM.U
  val illegal_mtilen = mtilen <= ROWNUM.U && Mux1H(Seq(
    MmulOpType.isToE4(in.ctrl.fuOpType)  -> (mtilek <= (ARLEN / 4).U),
    MmulOpType.isToE8(in.ctrl.fuOpType)  -> (mtilek <= (ARLEN / 8).U),
    MmulOpType.isToE16(in.ctrl.fuOpType) -> (mtilek <= (ARLEN / 16).U),
    MmulOpType.isToE32(in.ctrl.fuOpType) -> (mtilek <= (ARLEN / 32).U),
  ))
  val illegal_mtilek = Mux1H(Seq(
    MmulOpType.isFromE4(in.ctrl.fuOpType)  -> (mtilek <= (TRLEN / 4).U),
    MmulOpType.isFromE8(in.ctrl.fuOpType)  -> (mtilek <= (TRLEN / 8).U),
    MmulOpType.isFromE16(in.ctrl.fuOpType) -> (mtilek <= (TRLEN / 16).U),
    MmulOpType.isFromE32(in.ctrl.fuOpType) -> (mtilek <= (TRLEN / 32).U),
  ))

  protected val realFuOpType = WireInit(in.ctrl.fuOpType)

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
  // out.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := illegal_regidx || illegal_mtilem || illegal_mtilen || illegal_mtilek
}