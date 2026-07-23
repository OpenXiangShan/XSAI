package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.{AmuCtrlIO, AmuReleaseIO2CUTE}

class Mrelease(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  val msyncRegsNum = p(XSCoreParamsKey).MsyncRegs
  require(Seq(8, 16, 32).contains(msyncRegsNum), "MsyncRegs only supports 8/16/32")

  val msyncIdx = in.data.imm(4, 0)
  val illegalMsyncIdx = msyncRegsNum match {
    case 8  => msyncIdx(4, 3).orR
    case 16 => msyncIdx(4)
    case 32 => false.B
  }

  val output = Wire(new AmuReleaseIO2CUTE)
  output.msyncRd := msyncIdx

  out.res.data := 0.U
  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.releaseOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
  out.ctrl.exceptionVec.get := 0.U.asTypeOf(out.ctrl.exceptionVec.get)
  out.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := illegalMsyncIdx
  if (env.EnableDifftest) {
    // It will be filled in ROB.
    out.ctrl.amuCtrl.get.pc.get := DontCare
    out.ctrl.amuCtrl.get.coreid.get := DontCare
  }
}
