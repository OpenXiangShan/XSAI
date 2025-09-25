package cute

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config._
import scala.util.Random
import freechips.rocketchip.util.UIntToAugmentedUInt
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import xiangshan.XSCoreParamsKey
import system.SoCParamsKey
import system.SoCParameters

/**
 * XSCuteTop smoke test.
 */
class CUTESmokeTest extends AnyFreeSpec with Matchers with CuteConsts {
  
  // Test configuration parameters
  implicit val config: Parameters = new Config((site, here, up) => {
    case BuildYGAC => (p: Parameters) => new MyACCModule {
      // Default implementation for testing
    }
    case CuteParamsKey => CuteParams.baseParams.copy(
        Debug = CuteDebugParams.AllDebugOn
    )
    case MonitorsEnabled => false
    case XSCoreParamsKey => xiangshan.XSCoreParameters()
    case SoCParamsKey => SoCParameters()
    case x: Any => throw new Exception(s"Unhandled key: ${x} in config")
  })

  "TestTopImpl smoke test" - {

    "Matrix multiplication Marco instruction test" in {

      simulate(new CuteTestHarness) { dut =>
        dut.reset.poke(true.B)
        dut.clock.step(50)
        dut.reset.poke(false.B)
        // while (!dut.io.success.peek().litToBoolean) {
        //   dut.clock.step(1)
        // }
        // 5000 cycles is enough to reach MMA finish point.
        dut.clock.step(5000)
      }
    }
  }
}