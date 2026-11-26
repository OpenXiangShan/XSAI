package xiangshan.backend.decode

import chisel3._
import chiseltest._
import cute.MatrixIsaParams
import org.scalatest.flatspec.AnyFlatSpec

class McfgFp4LegalitySpec extends AnyFlatSpec with ChiselScalatestTester {
  private val int8 = 2
  private val int32 = 4
  private val nvfp4 = 5
  private val mxfp4 = 6
  private val bf16 = 10
  private val fp32 = 12

  private def raw(typeCode: Int): BigInt = BigInt(typeCode)

  private def supports(
    aType: Int,
    bType: Int,
    cType: Int,
    matrixExtension: MatrixIsaParams
  ): Boolean = {
    McfgHelpers.isMmaTripleSupportedRaw(
      raw(aType),
      raw(bType),
      raw(cType),
      matrixExtension
    )
  }

  private class LegalityTop extends Module {
    val io = IO(new Bundle {
      val aType = Input(UInt(4.W))
      val bType = Input(UInt(4.W))
      val cType = Input(UInt(4.W))
      val supported = Output(Bool())
    })

    io.supported := McfgHelpers.isMmaTripleSupported(
      io.aType,
      io.bType,
      io.cType,
      MatrixIsaParams(
        enableInt8Int32 = true,
        enableNvfp4Fp32 = true,
        enableMxfp4Fp32 = true
      )
    )
  }

  it should "keep NVFP4 and MXFP4 illegal when disabled" in {
    val disabled = MatrixIsaParams(enableInt8Int32 = true)
    assert(!supports(nvfp4, nvfp4, fp32, disabled))
    assert(!supports(mxfp4, mxfp4, fp32, disabled))
    assert(supports(int8, int8, int32, disabled))
  }

  it should "enable only the matching FP4 triple" in {
    val nv = MatrixIsaParams(enableNvfp4Fp32 = true)
    assert(supports(nvfp4, nvfp4, fp32, nv))
    assert(!supports(mxfp4, mxfp4, fp32, nv))
    assert(!supports(nvfp4, mxfp4, fp32, nv))
    assert(!supports(nvfp4, nvfp4, bf16, nv))

    val mx = MatrixIsaParams(enableMxfp4Fp32 = true)
    assert(supports(mxfp4, mxfp4, fp32, mx))
    assert(!supports(nvfp4, nvfp4, fp32, mx))
  }

  it should "decode the same static legality through Chisel wires" in {
    test(new LegalityTop) { dut =>
      def check(aType: Int, bType: Int, cType: Int, expected: Boolean): Unit = {
        dut.io.aType.poke(aType.U)
        dut.io.bType.poke(bType.U)
        dut.io.cType.poke(cType.U)
        assert(dut.io.supported.peek().litToBoolean == expected)
      }

      check(int8, int8, int32, expected = true)
      check(nvfp4, nvfp4, fp32, expected = true)
      check(mxfp4, mxfp4, fp32, expected = true)
      check(nvfp4, mxfp4, fp32, expected = false)
      check(mxfp4, mxfp4, bf16, expected = false)
    }
  }
}
