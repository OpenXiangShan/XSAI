package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{ExceptionNO, MSetOpType, CSROpType}
import xiangshan.backend.decode.{Imm_MSET, Imm_VSETIVLI, Imm_VSETVLI, McfgHelpers}
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.{MsetMtilexModule}
import chisel3.util.switch

class MSetMtilexRiWmf(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  val atx = Mux(MSetOpType.isMsettilexi(io.in.bits.ctrl.fuOpType),
    Imm_MSET().getAtx(io.in.bits.data.src(0)),
    io.in.bits.data.src(0)
  )
  io.out.bits.res.data := atx

  assert(cfg.writeMxRf, "MSetMtilexRiWmf must write mtilex")
}

class MSetMtilexRmfWmf(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  io.out.bits.res.data := io.in.bits.data.src(2)
}

class Mcfg(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  val isMgetcfg = MSetOpType.isMgetcfg(io.in.bits.ctrl.fuOpType)
  val mcfgRaw = io.in.bits.data.src(0)
  val unsupported = MSetOpType.isMsetcfg(io.in.bits.ctrl.fuOpType) && !McfgHelpers.isSupported(mcfgRaw(3, 0), mcfgRaw(7, 4))

  io.out.bits.res.data := Mux(isMgetcfg, io.in.bits.ctrl.mcfgReadRaw.getOrElse(0.U), mcfgRaw)
  io.out.bits.ctrl.exceptionVec.get := 0.U.asTypeOf(io.out.bits.ctrl.exceptionVec.get)
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := unsupported
}
