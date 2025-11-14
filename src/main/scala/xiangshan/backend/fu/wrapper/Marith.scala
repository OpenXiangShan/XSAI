package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.matrix.Bundles.{AmuArithIO, AmuCtrlIO}

class Marith(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val mtilex0 = in.data.src(0)
  protected val mtilex1 = in.data.src(1)

  val output = Wire(new AmuArithIO)
  dontTouch(output)
  output.md     := in.data.imm(3, 0)
  output.opType := in.ctrl.fuOpType

  out.res.data := output.asUInt

  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.arithOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
}