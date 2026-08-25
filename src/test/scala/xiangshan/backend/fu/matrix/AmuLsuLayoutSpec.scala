package xiangshan.backend.fu.matrix

import chisel3._
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultMatrixConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._
import xiangshan.backend.fu.matrix.Bundles.{AmuLsuIO => XsAmuLsuIO}

class AmuLsuLayoutHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val raw = Input(UInt(127.W))
    val xs = Output(new XsAmuLsuIO)
    val cute = Output(new _root_.cute.Bundles.AmuLsuIO)
  })

  io.xs := io.raw.asTypeOf(new XsAmuLsuIO)
  io.cute := io.raw.asTypeOf(new _root_.cute.Bundles.AmuLsuIO)
}

class AmuLsuLayoutSpec extends AnyFlatSpec with ChiselScalatestTester {
  private val matrixConfig = new DefaultMatrixConfig(1)
  private implicit val p: Parameters = matrixConfig.alter((site, here, _) => {
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = here(DebugOptionsKey).EnableDebug,
      enablePerf = here(DebugOptionsKey).EnablePerfDebug,
      fpgaPlatform = here(DebugOptionsKey).FPGAPlatform,
      enableXMR = here(DebugOptionsKey).EnableXMR
    )
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = here(DebugOptionsKey).EnablePerfDebug && !here(DebugOptionsKey).FPGAPlatform,
      enablePerfDB = here(DebugOptionsKey).EnableRollingDB && !here(DebugOptionsKey).FPGAPlatform,
      perfLevel = XSPerfLevel.withName(here(DebugOptionsKey).PerfLevel),
      perfDBHartID = 0
    )
    case XSCoreParamsKey => site(XSTileKey).head
  })

  behavior of "AmuLsuIO"

  it should "append packedB above the unchanged legacy bits on both sides" in {
    test(new AmuLsuLayoutHarness) { dut =>
      val packedB = BigInt(1)
      val ms = BigInt(0xa)
      val ls = BigInt(1)
      val transpose = BigInt(0)
      val isacc = BigInt(1)
      val isA = BigInt(0)
      val isB = BigInt(1)
      val baseAddr = BigInt("123456789abc", 16)
      val stride = BigInt("fedcba987654", 16)
      val row = BigInt(0x12f)
      val column = BigInt(0x0d5)
      val widths = BigInt(0x4)

      val raw = (packedB << 126) |
        (ms << 122) |
        (ls << 121) |
        (transpose << 120) |
        (isacc << 119) |
        (isA << 118) |
        (isB << 117) |
        (baseAddr << 69) |
        (stride << 21) |
        (row << 12) |
        (column << 3) |
        widths

      dut.io.raw.poke(raw.U)

      dut.io.xs.packedB.expect(true.B)
      dut.io.xs.ms.expect(ms.U)
      dut.io.xs.ls.expect(true.B)
      dut.io.xs.transpose.expect(false.B)
      dut.io.xs.isacc.expect(true.B)
      dut.io.xs.isA.expect(false.B)
      dut.io.xs.isB.expect(true.B)
      dut.io.xs.baseAddr.expect(baseAddr.U)
      dut.io.xs.stride.expect(stride.U)
      dut.io.xs.row.expect(row.U)
      dut.io.xs.column.expect(column.U)
      dut.io.xs.widths.expect(widths.U)

      dut.io.cute.packedB.expect(true.B)
      dut.io.cute.ms.expect(ms.U)
      dut.io.cute.ls.expect(true.B)
      dut.io.cute.transpose.expect(false.B)
      dut.io.cute.isacc.expect(true.B)
      dut.io.cute.isA.expect(false.B)
      dut.io.cute.isB.expect(true.B)
      dut.io.cute.baseAddr.expect(baseAddr.U)
      dut.io.cute.stride.expect(stride.U)
      dut.io.cute.row.expect(row.U)
      dut.io.cute.column.expect(column.U)
      dut.io.cute.widths.expect(widths.U)
    }
  }
}
