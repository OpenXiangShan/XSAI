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
 * XSCuteTop smoke test with Matrix Data Memory Model integration.
 */
class CUTESmokeTest extends AnyFreeSpec with Matchers with CuteConsts {

  // Test configuration parameters matching CuteTestHarness
  val M = 128
  val N = 128
  val K = 64
  val BaseA = 0x10000000L
  val BaseB = 0x20000000L
  val BaseC = 0x30000000L
  val BaseD = 0x40000000L
  val EA = 1  // 1 byte for int8
  val EB = 1  // 1 byte for int8
  val EC = 4  // 4 bytes for int32
  val ED = 4  // 4 bytes for int32
  val StrideA = K * EA
  val StrideB = K * EB
  val StrideC = N * EC
  val StrideD = N * ED


  /**
   * Process memory requests through the MatrixDataMemoryModel
   * This function monitors dut.io.req for valid requests and processes them
   * according to the SRAM timing model (same-cycle response).
   *
   * @param model The memory model instance
   * @param dut The CuteTestHarness device under test
   * @return true if request was processed, false if no valid request
   */
  def processMemoryRequest(model: MatrixDataMemoryModel, dut: CuteTestHarness): Boolean = {
    var anyRequestProcessed = false

    // Process all 8 channels using Scala loop
    for (channel <- 0 until 8) {
      // Check if there's a valid request on this channel
      if (dut.io.req(channel).valid.peek().litToBoolean) {
        anyRequestProcessed = true
        val addr = dut.io.req(channel).addr.peek().litValue.toLong
        val isWrite = dut.io.req(channel).write.peek().litToBoolean
        val bytes = dut.io.req(channel).bytes.peek().litValue.toInt

        // Use peekString to get the full data as a string, then convert to BigInt
        val data = dut.io.req(channel).data.peek().litValue

        // Log that we received a request from Chisel
        println(s"[CUTESmokeTest] Received request from Chisel [channel=$channel]: addr=0x${addr.toHexString}, write=$isWrite, bytes=$bytes, data=0x${data.toString(16)}")

        if (isWrite) {
          // Handle write request with full BigInt data
          val (success, warning) = model.handleWriteBigInt(addr, data, bytes)
          warning.foreach(msg => println(s"[CUTESmokeTest] $msg"))
          if (!success) {
            println(s"[CUTESmokeTest] ERROR: Write failed at addr 0x${addr.toHexString}, data=0x${data.toString(16)}, bytes=$bytes")
          }
        } else {
          // Handle read request
          val (responseData, valid, warning) = model.handleReadBigInt(addr, bytes)
          warning.foreach(msg => println(s"[CUTESmokeTest] $msg"))

          if (!valid) {
            println(s"[CUTESmokeTest] ERROR: Read failed at addr 0x${addr.toHexString}, bytes=$bytes")
          }

          // Log response we're sending back to Chisel
          println(s"[CUTESmokeTest] Sending response to Chisel [channel=$channel]: valid=true, data=0x${responseData.toString(16)}")

          // Set response data in same cycle (SRAM timing) - convert BigInt to UInt
          dut.io.resp(channel).valid.poke(true.B)
          dut.io.resp(channel).data.poke(responseData.U)
        }
      } else {
        // No valid request on this channel, deassert response
        dut.io.resp(channel).valid.poke(false.B)
      }
    }

    anyRequestProcessed
  }
  
  // Test configuration parameters
  implicit val config: Parameters = new Config((site, here, up) => {
    case BuildYGAC => (p: Parameters) => new MyACCModule {
      // Default implementation for testing
    }
    case CuteParamsKey => CuteParams.CUTE_8Tops_128SCP.copy(
      Debug = CuteDebugParams.AllDebugOn,
      v3config = Cutev3extParams(
        TaskCtrl_AutoClear = true,
      )
    )
    case MonitorsEnabled => false
    case XSCoreParamsKey => xiangshan.XSCoreParameters()
    case SoCParamsKey => SoCParameters()
  })

  "TestTopImpl smoke test" - {

    "Matrix multiplication with Memory Model test" in {
      // Create and initialize the memory model
      // Note: StrideC and StrideD should be 512 bytes (128 * 4 bytes for int32)
      val model = MatrixDataMemoryModel.custom(
        M = M, N = N, K = K,
        StrideA = StrideA, StrideB = StrideB, StrideC = 512L, StrideD = 512L,
        BaseA = BaseA, BaseB = BaseB, BaseC = BaseC, BaseD = BaseD,
        EA = EA, EB = EB, EC = EC, ED = ED,
        beatBytes = 64
      )
      model.initialize(new Random(42))

      println("[CUTESmokeTest] Starting matrix multiplication test with memory model")

      simulate(new CuteTestHarness) { dut =>
        // Initialize response signals for all 8 channels
        for (channel <- 0 until 8) {
          dut.io.resp(channel).valid.poke(false.B)
          dut.io.resp(channel).data.poke(0.U)
        }

        dut.reset.poke(true.B)
        dut.clock.step(50)
        dut.reset.poke(false.B)

        var cycleCount = 0
        var readCount = 0
        var writeCount = 0
        val maxCycles = 100000
        var requestProcessed = false

        println("[CUTESmokeTest] Starting simulation loop")

        while (!dut.io.success.peek().litToBoolean && cycleCount < maxCycles) {
          // Process memory requests if any
          requestProcessed = processMemoryRequest(model, dut)

          if (requestProcessed) {
            // Check all channels for write requests
            for (channel <- 0 until 8) {
              if (dut.io.req(channel).valid.peek().litToBoolean) {
                val isWrite = dut.io.req(channel).write.peek().litToBoolean
                if (isWrite) {
                  writeCount += 1
                } else {
                  readCount += 1
                }
              }
            }
          }

          // Step clock
          dut.clock.step(1)
          cycleCount += 1

          // Print progress every 100 cycles
          if (cycleCount % 100 == 0) {
            println(s"[CUTESmokeTest] Cycle $cycleCount: Reads=$readCount, Writes=$writeCount")
          }
        }

        dut.clock.step(10) // Finish any pending operations

        println(s"[CUTESmokeTest] Test completed after $cycleCount cycles")
        println(s"[CUTESmokeTest] Total memory operations: $readCount reads, $writeCount writes")

        // Compare D matrix with reference result
        val (allMatch, mismatchCount, firstMismatch) = model.compareWithReference()

        if (!allMatch) {
          // Print detailed information for debugging
          model.printSummary()
          println("\n[Detailed comparison]")
          firstMismatch.foreach { idx =>
            val m = idx / N
            val n = idx % N
            println(s"First mismatch at position [$m][$n]:")
            println(s"  D matrix value: ${model.D(idx)}")
            println(s"  Reference value: ${model.ReferenceResult(idx)}")
            println(s"  Difference: ${model.D(idx) - model.ReferenceResult(idx)}")
          }
        }

        // Assert that the result matches
        assert(allMatch, s"Matrix multiplication result mismatch: $mismatchCount elements differ")
      }
    }

    // "Matrix multiplication Marco instruction test (original)" in {

    //   simulate(new CuteTestHarness) { dut =>
    //     dut.reset.poke(true.B)
    //     dut.clock.step(50)
    //     dut.reset.poke(false.B)
    //     while (!dut.io.success.peek().litToBoolean) {
    //       dut.clock.step(1)
    //     }
    //     // 5000 cycles is enough to reach MMA finish point.
    //     // dut.clock.step(10000)
    //   }
    // }
  }
}