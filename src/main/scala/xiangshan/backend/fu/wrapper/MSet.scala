package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import utility.ZeroExt
import xiangshan.{MSETtilexOpType, CSROpType}
import xiangshan.backend.decode.{Imm_MSET, Imm_VSETIVLI, Imm_VSETVLI}
import xiangshan.backend.decode.isa.bitfield.InstMType
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.fu.{MsetMtilexModule}
import chisel3.util.switch

class MSetMtilexRiWmf(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  val isMsettilex = MSETtilexOpType.isMsettilex(io.in.bits.ctrl.fuOpType)
  
  val atx = Mux(MSETtilexOpType.isMsettilexi(io.in.bits.ctrl.fuOpType),
    Imm_MSET().getAtx(io.in.bits.data.src(0)),
    io.in.bits.data.src(0)
  )
  io.out.bits.res.data := atx

  assert(cfg.writeMxRf, "MSetMtilexRiWmf must write mtilex")
  // io.mtilex.get.bits := atx
  // io.mtilex.get.valid := io.out.valid && isMsettilex
}

class MSetMtilexRmfWmf(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready

  io.out.bits.res.data := io.in.bits.data.src(2)
}
