package cute

import chisel3._
import chiseltest._
import freechips.rocketchip.diplomacy.MonitorsEnabled
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config._
import scala.util.Random
import top.{ArgParser, DefaultMatrix2T64MxfpConfig, DefaultMatrix2TConfig, DefaultMatrix2TMxfpConfig, DefaultMatrix2TopsConfig, DefaultMatrixConfig, YamlParser}
import utility.LogUtilsOptions
import utility.LogUtilsOptionsKey
import utility.PerfCounterOptions
import utility.PerfCounterOptionsKey
import utility.XSPerfLevel
import xiangshan.DebugOptionsKey
import xiangshan.XSCoreParamsKey
import xiangshan.XSTileKey
import xiangshan.{MatAcc, MatAccKey}
import system.SoCParamsKey

/** Parameter-only regressions: no RTL generation or Verilator smoke test is needed. */
class DefaultMatrixConfigParametersSpec extends AnyFreeSpec with Matchers {
  "DefaultMatrix 2TOPS selection" - {
    "keeps the short name identical to the long name for one and two cores with MX disabled" in {
      for (n <- Seq(1, 2)) {
        val selected = new DefaultMatrix2TConfig(n)
        val longName = new DefaultMatrix2TopsConfig(n)
        selected(XSTileKey) mustBe longName(XSTileKey)
        selected(SoCParamsKey) mustBe longName(SoCParamsKey)
        selected(DebugOptionsKey) mustBe longName(DebugOptionsKey)
        selected(MatAccKey) mustBe longName(MatAccKey)
        selected(CuteParamsKey) mustBe longName(CuteParamsKey)
        selected(CuteParamsKey).MatrixExtension.enableMxfp8Fp32 mustBe false
        selected(CuteParamsKey).MatrixExtension.enableMxfp4Fp32 mustBe false
        selected(CuteParamsKey).MatrixExtension.enableNvfp4Fp32 mustBe false
      }
    }

    "preserves the complete DefaultMatrix CPU and cache parameters for one and two cores" in {
      for (n <- Seq(1, 2)) {
        val baseline = new DefaultMatrixConfig(n)
        val selected = new DefaultMatrix2TopsConfig(n)
        selected(XSTileKey) mustBe baseline(XSTileKey)
        selected(SoCParamsKey) mustBe baseline(SoCParamsKey)
        selected(DebugOptionsKey) mustBe baseline(DebugOptionsKey)
        selected(MatAccKey) mustBe MatAcc.CUTE
        selected(CuteParamsKey) mustBe baseline(CuteParamsKey).copy(
          Matrix_MN = CuteParams.CUTE_2Tops.Matrix_MN
        )
      }
    }

    "keeps the default array at eight and proves 1024 INT8 operations per cycle for the opt-in array" in {
      val baseline = new DefaultMatrixConfig(1)
      val selected = new DefaultMatrix2TopsConfig(1)
      val core = selected(XSTileKey).head
      val cp = selected(CuteParamsKey)
      core.TLEN mustBe 128 * 64 * 8
      core.TRLEN mustBe 64 * 8
      core.MELEN mustBe 32
      cp.Tensor_MN mustBe 128
      cp.Tensor_K mustBe 64
      cp.Matrix_MN mustBe 4
      cp.ReduceWidthByte mustBe 32
      cp.ReduceWidth mustBe 256
      cp.outsideDataWidth mustBe 512
      cp.L2NBanks mustBe core.L2NBanks
      cp.MatrixExtension mustBe core.MatrixExtension
      cp.MatrixExtension.enableInt8Int32 mustBe true
      cp.MatrixExtension.enableMxfp8Fp32 mustBe false
      cp.MatrixExtension.enableMxfp4Fp32 mustBe false
      cp.MatrixExtension.enableNvfp4Fp32 mustBe false
      // MatrixTE instantiates Matrix_MN squared PEs; each accepts ReduceWidth / 8 INT8 MACs.
      // One MAC counts as one multiply plus one add. This is a peak roof, not measured throughput.
      val int8OpsPerCycle = cp.Matrix_MN * cp.Matrix_MN * (cp.ReduceWidth / 8) * 2
      int8OpsPerCycle mustBe 1024
      baseline(CuteParamsKey).Matrix_MN mustBe 8
      val base = baseline(CuteParamsKey)
      base.Matrix_MN * base.Matrix_MN * (base.ReduceWidth / 8) * 2 mustBe 4096
      println(s"[DefaultMatrix2TopsConfig] TLEN=${core.TLEN} TRLEN=${core.TRLEN} MELEN=${core.MELEN} " +
        s"INT8_ops_per_cycle=$int8OpsPerCycle CuteParams=$cp")
    }

    "remains selectable through the existing one-Int config constructor and command-line parser" in {
      for (name <- Seq(
        "DefaultMatrix2TConfig", "top.DefaultMatrix2TConfig",
        "DefaultMatrix2TopsConfig", "top.DefaultMatrix2TopsConfig"
      )) {
        val selected = ArgParser.getConfigByName(name)
        selected(CuteParamsKey).Matrix_MN mustBe 4
        selected(XSTileKey).size mustBe 1
        for (n <- Seq(1, 2)) {
          val (parsed, _, _) = ArgParser.parse(Array("--config", name, "--num-cores", n.toString))
          val (longName, _, _) = ArgParser.parse(Array(
            "--config", "DefaultMatrix2TopsConfig", "--num-cores", n.toString
          ))
          parsed(XSTileKey).map(_.HartId) mustBe (0 until n)
          parsed(XSTileKey) mustBe longName(XSTileKey)
          parsed(SoCParamsKey) mustBe longName(SoCParamsKey)
          parsed(CuteParamsKey) mustBe longName(CuteParamsKey)
        }
      }
    }

    "retains late core-geometry, synchronization, cache-bank and difftest overrides" in {
      val selected = new DefaultMatrix2TopsConfig(2).alter((site, here, up) => {
        case XSTileKey => up(XSTileKey).map(_.copy(
          TLEN = 64 * 64 * 8, TRLEN = 64 * 8, MsyncRegs = 16, L2NBanks = 4
        ))
        case DebugOptionsKey => up(DebugOptionsKey).copy(EnableDifftest = true)
      })
      val cp = selected(CuteParamsKey)
      cp.Matrix_MN mustBe 4
      cp.Tensor_MN mustBe 64
      cp.Tensor_K mustBe 64
      cp.MsyncRegs mustBe 16
      cp.L2NBanks mustBe 4
      cp.EnableDifftest mustBe true
    }

    "selects explicit 2T64 MX geometry without changing the historical 128-SCP preset" in {
      for (n <- Seq(1, 2)) {
        val baseline = new DefaultMatrix2TMxfpConfig(n)
        val selected = new DefaultMatrix2T64MxfpConfig(n)
        baseline(XSTileKey).foreach(_.TLEN mustBe 128 * 64 * 8)
        selected(XSTileKey) mustBe baseline(XSTileKey).map(_.copy(TLEN = 64 * 64 * 8))
        selected(SoCParamsKey) mustBe baseline(SoCParamsKey)
        selected(DebugOptionsKey) mustBe baseline(DebugOptionsKey)
        selected(MatAccKey) mustBe MatAcc.CUTE
        selected(CuteParamsKey) mustBe baseline(CuteParamsKey).copy(Tensor_MN = 64)
        val cp = selected(CuteParamsKey)
        cp.Tensor_MN mustBe 64
        cp.Tensor_K mustBe 64
        cp.Matrix_MN mustBe 4
        cp.ReduceWidthByte mustBe 32
        cp.outsideDataWidth mustBe 512
        cp.ABMatrixRegSize mustBe 4096
        cp.CMatrixRegSize mustBe 16384
        cp.MatrixExtension mustBe selected(XSTileKey).head.MatrixExtension
        cp.MatrixExtension.enableMxfp4Fp32 mustBe true
        cp.MatrixExtension.enableMxfp8Fp32 mustBe true
        cp.MatrixExtension.enableNvfp4Fp32 mustBe false
      }
      for (name <- Seq("DefaultMatrix2T64MxfpConfig", "top.DefaultMatrix2T64MxfpConfig")) {
        val selected = ArgParser.getConfigByName(name)
        selected(CuteParamsKey).Tensor_MN mustBe 64
        val (parsed, _, _) = ArgParser.parse(Array("--config", name, "--num-cores", "1"))
        parsed(XSTileKey).size mustBe 1
        parsed(XSTileKey).head.TRLEN mustBe 512
        parsed(XSTileKey).head.MELEN mustBe 32
        parsed(CuteParamsKey).Tensor_MN mustBe 64
        parsed(CuteParamsKey).MatrixExtension.enableMxfp4Fp32 mustBe true
      }
      println("[DefaultMatrix2T64MxfpConfig] Tensor_MN=64 Tensor_K=64 Matrix_MN=4 " +
        "ReduceWidthByte=32 TR_bytes=4096 accumulator_bytes=16384 MXFP4=enabled")
    }

    "preserves the existing YAML cache overlay without changing the selected compute array" in {
      val yaml = new java.io.File(getClass.getResource("/config/Default.yml").toURI).getPath
      val baseline = YamlParser.parseYaml(new DefaultMatrixConfig(1), yaml)
      val selected = YamlParser.parseYaml(new DefaultMatrix2TopsConfig(1), yaml)
      selected(XSTileKey) mustBe baseline(XSTileKey)
      selected(SoCParamsKey) mustBe baseline(SoCParamsKey)
      selected(CuteParamsKey) mustBe baseline(CuteParamsKey).copy(Matrix_MN = 4)
      selected(CuteParamsKey).L2NBanks mustBe 4
    }
  }
}

/**
 * XSCuteTop smoke test with Matrix Data Memory Model integration.
 */
class CUTESmokeTest extends AnyFreeSpec with ChiselScalatestTester with Matchers with CuteConsts {

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
  implicit val config: Parameters = new DefaultMatrixConfig(1).alter((site, here, up) => {
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
    case BuildYGAC => (p: Parameters) => new MyACCModule {
      // Default implementation for testing
    }
    case MonitorsEnabled => false
    case XSCoreParamsKey => site(XSTileKey).head
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

      test(new CuteTestHarness).withAnnotations(Seq(VerilatorBackendAnnotation)) { dut =>
        dut.clock.setTimeout(0)

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
