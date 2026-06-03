package xiangshan.backend.rob

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan._
import xiangshan.backend.decode.McfgCommit
import xiangshan.backend.rob.RobBundles.RobCommitEntryBundle

class MCfgBufferToDecode(implicit p: Parameters) extends XSBundle {
  val isResumeMcfg = Bool()
  val walkToArchMcfg = Bool()
  val walkMcfg = Vec(CommitWidth, Valid(new McfgCommit))
  val commitMcfg = Vec(CommitWidth, Valid(new McfgCommit))
}

class MCfgBufferIO(implicit p: Parameters) extends XSBundle {
  val commit = Input(Vec(CommitWidth, Valid(new RobCommitEntryBundle)))
  val walk = Input(Vec(CommitWidth, Valid(new RobCommitEntryBundle)))
  val walkStart = Input(Bool())
  val walkActive = Input(Bool())
  val toDecode = Output(new MCfgBufferToDecode)
}

class MCfgBuffer(implicit p: Parameters) extends XSModule {
  val io = IO(new MCfgBufferIO)

  private def connectMcfgUpdate(
    sink: Valid[McfgCommit],
    source: Valid[RobCommitEntryBundle]
  ): Unit = {
    val isMsetcfg = source.bits.isMsetcfg.getOrElse(false.B)
    val unsupported = source.bits.mcfgIllegalUnsupported.getOrElse(false.B)
    val valid = source.valid && isMsetcfg && !unsupported

    sink.valid := valid
    sink.bits.sel := source.bits.mcfgReadView.get.sel
    sink.bits.entry := source.bits.mcfgReadView.get.entry
  }

  io.toDecode.walkToArchMcfg := io.walkStart
  io.toDecode.isResumeMcfg := io.walkActive || VecInit(io.toDecode.walkMcfg.map(_.valid)).asUInt.orR

  io.toDecode.commitMcfg.zip(io.commit).foreach { case (sink, source) =>
    connectMcfgUpdate(sink, source)
  }
  io.toDecode.walkMcfg.zip(io.walk).foreach { case (sink, source) =>
    connectMcfgUpdate(sink, source)
  }
}
