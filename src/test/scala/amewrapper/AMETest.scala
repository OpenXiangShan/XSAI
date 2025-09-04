// package amewrapper

// import chisel3._
// import chisel3.experimental.BundleLiterals._
// import chisel3.simulator.EphemeralSimulator._
// import org.scalatest.freespec.AnyFreeSpec
// import org.scalatest.matchers.must.Matchers
// import org.chipsalliance.cde.config._
// import freechips.rocketchip.diplomacy._
// import scala.util.Random
// import circt.stage.{ChiselStage, FirtoolOption}
// import chisel3.stage.ChiselGeneratorAnnotation
// import AME._
// import utility.sram._
// import common._
// import RegFile._
// import MMAU._

// // Test Helper Object
// object AMEModuleTestHelper {
//   def writeTestDataToAll(testData: Seq[Seq[UInt]], allAddr: Int, dut: AMEImp): Unit = {
//     dut.io.writeAll.act.poke(true.B)
//     dut.io.writeAll.addr.poke(allAddr.U)

//     for ((bankData, bankIdx) <- testData.zipWithIndex) {
//       for ((data, setIdx) <- bankData.zipWithIndex) {
//         dut.io.writeAll.w(bankIdx).req.bits.setIdx.poke(setIdx.U)
//         dut.io.writeAll.w(bankIdx).req.bits.data.head.poke(data)
//         dut.io.writeAll.w(bankIdx).req.valid.poke(true.B)
//         dut.clock.step()
//         dut.io.writeAll.w(bankIdx).req.valid.poke(false.B)
//       }
//     }
//     dut.io.writeAll.act.poke(false.B)
//   }

//   def readTestDataFromAll(expectData: Seq[Seq[UInt]], allAddr: Int, dut: AMEImp): Unit = {
//     dut.io.readAll.act.poke(true.B)
//     dut.io.readAll.addr.poke(allAddr.U)
//     println(s"Reading \"all\" address: $allAddr")

//     for ((bankData, bankIdx) <- expectData.zipWithIndex) {
//       for ((data, setIdx) <- bankData.zipWithIndex) {
//         dut.io.readAll.r(bankIdx).req.bits.setIdx.poke(setIdx.U)
//         dut.io.readAll.r(bankIdx).req.valid.poke(true.B)
//         dut.clock.step()

//         val readValue = dut.io.readAll.r(bankIdx).resp.data.head.asUInt.peek()
//         val status = if (readValue.litValue == data.litValue) "PASS" else "FAILED"
//         println(f"Bank $bankIdx, Set $setIdx - Read value: 0x${readValue.litValue.toString(16)}%s, Expected: 0x${data.litValue.toString(16)}%s [$status]")
//         dut.io.readAll.r(bankIdx).resp.data.head.asUInt.expect(data)
//         dut.io.readAll.r(bankIdx).req.valid.poke(false.B)
//       }
//     }
//     dut.io.readAll.act.poke(false.B)
//   }

//   def AMEStart(dut: AMEImp, mtilem: Int, mtilen: Int, mtilek: Int, 
//                ms1: Int, ms2: Int, md: Int, rs1: Int, rs2: Int, 
//                valid: Bool, is_mmacc: Bool, is_mlbe8: Bool): Unit = {
//     dut.io.Uop_io.mtileConfig_io.mtilem.poke(mtilem.U)
//     dut.io.Uop_io.mtileConfig_io.mtilen.poke(mtilen.U)
//     dut.io.Uop_io.mtileConfig_io.mtilek.poke(mtilek.U)
//     dut.io.Uop_io.Operands_io.ms1.poke(ms1.U)
//     dut.io.Uop_io.Operands_io.ms2.poke(ms2.U)
//     dut.io.Uop_io.Operands_io.md.poke(md.U)
//     dut.io.Uop_io.Operands_io.rs1.poke(rs1.U)
//     dut.io.Uop_io.Operands_io.rs2.poke(rs2.U)
//     dut.io.Uop_io.ShakeHands_io.valid.poke(valid)
//     dut.io.Uop_io.InsType_io.is_mmacc.poke(is_mmacc)
//     dut.io.Uop_io.InsType_io.is_mlbe8.poke(is_mlbe8)
//   }

//   def AMEStop(dut: AMEImp): Unit = {
//     AMEStart(dut, 0, 0, 0, 0, 0, 0, 0, 0, false.B, false.B, false.B)
//   }
// }

// // Main Test Class
// class TestTop_AME(implicit p: Parameters) extends LazyModule {
//   override lazy val desiredName: String = "TestTop"
//   val delayFactor = 0.5

//   val ame = LazyModule(new AMEModule())//TODO: IO not fully initialized
  
//   lazy val module = new LazyModuleImp(this) {
//     val timer = WireDefault(0.U(64.W))
//     val logEnable = WireDefault(false.B)
//     val clean = WireDefault(false.B)
//     val dump = WireDefault(false.B)

//     dontTouch(timer)
//     dontTouch(logEnable)
//     dontTouch(clean)
//     dontTouch(dump)
//     ame.module.io.Uop_io := DontCare
//     ame.module.io.writeAll := DontCare
//     ame.module.io.readAll := DontCare
//     // ame.module.io.matrix_data_in := 
//     ame.module.io.sigDone := DontCare
//   }
// }

// class AMETest extends AnyFreeSpec with Matchers {
//   "AMEModule should execute matrix operations correctly" in {
//     val config = new Config((site, here, up) => {
//       case AMEConfigKey => AMEParams(
//         numTrRegs = 2,
//         numAccRegs = 1,
//         dataWidth = 32,
//         matrixSize = 16
//       )
//     })

//     val top = DisableMonitors(p => LazyModule(new AMEModule()(p)))(config)
//     simulate(top.module) { dut =>
//       val ame = dut

//       // Initialize test data
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.A, 0, ame)
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.B, 1, ame)
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.Ctmp, 4, ame)
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.Ctmp, 5, ame)
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.Ctmp, 6, ame)
//       AMEModuleTestHelper.writeTestDataToAll(AMETestData.Ctmp, 7, ame)

//       dut.clock.step(1000) // Initial cycles

//       // Test Instruction 1
//       var cycleCountMMAU = 0
//       var cycleCountReady = 0

//       // Start first matrix operation
//       AMEModuleTestHelper.AMEStart(ame, 32, 32, 64, 0, 1, 4, 0, 0, true.B, true.B, false.B)

//       // Wait for ready
//       while(!ame.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountReady += 1
//       }
//       dut.clock.step(1)
//       println("Instruction 1 executing")

//       // Queue next instruction
//       AMEModuleTestHelper.AMEStart(ame, 32, 32, 64, 0, 1, 5, 0, 0, true.B, true.B, false.B)

//       // Wait for completion
//       while(!ame.io.sigDone.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountMMAU += 1
//       }
//       println(s"Instruction 1: Ready cycles = $cycleCountReady, MMAU cycles = $cycleCountMMAU")

//       // Test Instruction 2
//       cycleCountReady = 0
//       cycleCountMMAU = 0

//       while(!ame.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountReady += 1
//       }
//       dut.clock.step(1)
//       println("Instruction 2 executing")

//       // Queue next instruction
//       AMEModuleTestHelper.AMEStart(ame, 32, 32, 64, 0, 1, 6, 0, 0, true.B, true.B, false.B)

//       while(!ame.io.sigDone.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountMMAU += 1
//       }
//       println(s"Instruction 2: Ready cycles = $cycleCountReady, MMAU cycles = $cycleCountMMAU")

//       // Test Instruction 3
//       cycleCountReady = 0
//       cycleCountMMAU = 0

//       while(!ame.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountReady += 1
//       }
//       dut.clock.step(1)
//       println("Instruction 3 executing")

//       while(!ame.io.sigDone.peek().litToBoolean) {
//         dut.clock.step(1)
//         cycleCountMMAU += 1
//       }
//       println(s"Instruction 3: Ready cycles = $cycleCountReady, MMAU cycles = $cycleCountMMAU")

//       // Stop AME
//       AMEModuleTestHelper.AMEStop(ame)

//       // Verify results
//       AMEModuleTestHelper.readTestDataFromAll(AMETestData.C, 4, ame)
//       AMEModuleTestHelper.readTestDataFromAll(AMETestData.C, 5, ame)
//       AMEModuleTestHelper.readTestDataFromAll(AMETestData.C, 6, ame)
//     }
//   }
// }
// private[amewrapper] object TestTopAMEFirtoolOptions {
//   def apply() = Seq(
//     FirtoolOption("--disable-annotation-unknown"),
//     FirtoolOption("--repl-seq-mem"),
//     FirtoolOption("--repl-seq-mem-file=TestTop.sv.conf"),
//     FirtoolOption("--lowering-options=explicitBitcast")
//   )
// }
// // Test Object for standalone execution
// object TestTop_AME extends App {
//   val config = new Config((site, here, up) => {
//     case AMEConfigKey => AMEParams(
//       numTrRegs = 2,
//       numAccRegs = 1,
//       dataWidth = 32,
//       matrixSize = 16
//     )
//   })

//   val top = DisableMonitors(p => LazyModule(new TestTop_AME()(p)))(config)

//   (new ChiselStage).execute(
//     args, 
//     ChiselGeneratorAnnotation(() => top.module) +: 
//         TestTopAMEFirtoolOptions()
//       )

// } 