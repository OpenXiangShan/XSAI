package amewrapper

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import coupledL2.{MatrixField, AmeChannelField, AmeIndexField}

// AME LazyModule
class AMEModule(implicit p: Parameters) extends LazyModule {
  // Create 8 matrix nodes for MLU's Cacheline interfaces
  val matrix_nodes = (0 until 1).flatMap { i =>
    (0 until 8).map { j =>
      TLClientNode(Seq(
        TLMasterPortParameters.v1(
          clients = Seq(TLMasterParameters.v1(
            name = s"matrix${i}_${j}",
            sourceId = IdRange(0, 1)
          )),
          requestFields = Seq(MatrixField(2), AmeChannelField(), AmeIndexField())
        )
      ))
    }
  }

  lazy val module = new AMEImp(this)
}

// AME Object for easier instantiation
object AMEModule {
  def apply()(implicit p: Parameters): AMEModule = {
    val ame = LazyModule(new AMEModule())
    ame
  }
} 