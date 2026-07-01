package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import xiangshan.backend.fu.{FuConfig, FuncUnit, PipedFuncUnit}
import xiangshan.backend.decode.{McfgEntry, McfgHelpers}
import xiangshan.backend.fu.matrix.Bundles.{AmuMmaIO, AmuCtrlIO}

class Mma(cfg: FuConfig)(implicit p: Parameters) extends PipedFuncUnit(cfg) {
  protected val in = io.in.bits
  protected val out = io.out.bits

  connect0LatencyCtrlSingal
  io.out.valid := io.in.valid
  io.in.ready := io.out.ready
  
  protected val mtilem = in.data.src(2)
  protected val mtilen = in.data.src(3)
  protected val mtilek = in.data.src(4)
  private val mmaMcfgReadRaw = in.ctrl.mmaMcfgReadRaw.get

  private def mcfgEntryFromRaw(raw: UInt): McfgEntry = {
    val entry = Wire(new McfgEntry)
    entry.raw := raw
    entry
  }

  private val aMcfgEntry = mcfgEntryFromRaw(mmaMcfgReadRaw(0))
  private val bMcfgEntry = mcfgEntryFromRaw(mmaMcfgReadRaw(1))
  private val cMcfgEntry = mcfgEntryFromRaw(mmaMcfgReadRaw(2))
  private val aTypeCode = aMcfgEntry.typeCode
  private val bTypeCode = bMcfgEntry.typeCode
  private val cTypeCode = cMcfgEntry.typeCode

  private def elementLimit(totalBits: Int, typeCode: UInt): UInt =
    MuxLookup(McfgHelpers.elementWidth(typeCode), 0.U)(Seq(
      4.U -> (totalBits / 4).U,
      8.U -> (totalBits / 8).U,
      16.U -> (totalBits / 16).U,
      32.U -> (totalBits / 32).U,
    ))

  private val legalMcfgEntries = aMcfgEntry.supported && bMcfgEntry.supported && cMcfgEntry.supported
  private val legalMmaTypes = McfgHelpers.isMmaTripleSupported(aTypeCode, bTypeCode, cTypeCode, MatrixExtension)
  private val illegal_mcfg = !legalMcfgEntries || !legalMmaTypes

  val illegal_regidx = !in.data.imm(2) || in.data.imm(5) || in.data.imm(8)
  val illegal_mtilem = mtilem > ROWNUM.U
  val illegal_mtilen = mtilen > ROWNUM.U || mtilen > elementLimit(ARLEN, cTypeCode)
  val illegal_mtilek = mtilek > elementLimit(TRLEN, aTypeCode)

  val output = Wire(new AmuMmaIO)
  dontTouch(output)
  output.ms1    := in.data.imm(5, 3)
  output.ms2    := in.data.imm(8, 6)
  output.md     := in.data.imm(2, 0)
  output.typed  := McfgHelpers.amuPayload(cTypeCode)
  output.types1 := McfgHelpers.amuPayload(aTypeCode)
  output.types2 := McfgHelpers.amuPayload(bTypeCode)
  output.sat    := io.msaten.get.asBool
  output.rm     := Mux(McfgHelpers.isFloatType(cTypeCode), io.mfrm.get, io.mxrm.get)
  output.mtilem := mtilem
  output.mtilen := mtilen
  output.mtilek := mtilek
  output.isfp   := McfgHelpers.isFloatType(cTypeCode)
  
  out.res.data := output.asUInt

  out.ctrl.amuCtrl.get.op   := AmuCtrlIO.mmaOp()
  out.ctrl.amuCtrl.get.data := output.asUInt
  out.ctrl.exceptionVec.get := 0.U.asTypeOf(out.ctrl.exceptionVec.get)
  out.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) :=
    illegal_regidx || illegal_mcfg || illegal_mtilem || illegal_mtilen || illegal_mtilek
  if (env.EnableDifftest) {
    // It will be filled in ROB.
    out.ctrl.amuCtrl.get.pc.get := DontCare
    out.ctrl.amuCtrl.get.coreid.get := DontCare
  }
}
