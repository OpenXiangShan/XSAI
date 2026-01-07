package xiangshan.backend.fu

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles._

class MsetMtilexModuleIO(implicit p: Parameters) extends XSBundle {
  // TODO: use a correct width
  // 'mlWidth' is just a placeholder
  private val mlWidth = p(XSCoreParamsKey).mlWidth

  val in = Input(new Bundle {
    // Application tilem/n/k
    // TODO: XLEN could be too large for it. Vector uses such a large width so I keep it for now.
    //       Check whether it's necessary.
    val atx   : UInt = UInt(XLEN.W)
  })

  val out = Output(new Bundle {
    val mtilex  : UInt = UInt(XLEN.W)
  })
}

class MsetMtilexModule(implicit p: Parameters) extends XSModule {
  val io = IO(new MsetMtilexModuleIO)

  private val atx   = io.in.atx
  io.out.mtilex := atx
}
