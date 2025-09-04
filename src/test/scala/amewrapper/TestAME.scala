package amewrapper

import AME.{AME, connectPort}
import AME._
import coupledL2._
import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.util.Random

import utility.sram._
import common._
import RegFile._
import MMAU._


// Define companion object
object AMETestHelper {
  def writeTestDataToAll(testData: Seq[Seq[UInt]], allAddr: Int, dut: AME): Unit = {
    dut.io.writeAll.act.poke(true.B)
    dut.io.writeAll.addr.poke(allAddr.U)

    for ((bankData, bankIdx) <- testData.zipWithIndex) {
      for ((data, setIdx) <- bankData.zipWithIndex) {
        dut.io.writeAll.w(bankIdx).req.bits.setIdx.poke(setIdx.U)
        dut.io.writeAll.w(bankIdx).req.bits.data.head.poke(data)
        dut.io.writeAll.w(bankIdx).req.valid.poke(true.B)
        dut.clock.step()
        dut.io.writeAll.w(bankIdx).req.valid.poke(false.B)
      }
    }
    dut.io.writeAll.act.poke(false.B)
  }

  def readTestDataFromAll(expectData: Seq[Seq[UInt]], allAddr: Int, dut: AME): Unit = {
    dut.io.readAll.act.poke(true.B)
    dut.io.readAll.addr.poke(allAddr.U)
    println(s"Reading \"all\" address: $allAddr")

    for ((bankData, bankIdx) <- expectData.zipWithIndex) {
      for ((data, setIdx) <- bankData.zipWithIndex) {
        dut.io.readAll.r(bankIdx).req.bits.setIdx.poke(setIdx.U)
        dut.io.readAll.r(bankIdx).req.valid.poke(true.B)
        dut.clock.step()

        val readValue = dut.io.readAll.r(bankIdx).resp.data.head.asUInt.peek()
        val status = if (readValue.litValue == data.litValue) "PASS" else "FAILED"
        println(f"Bank $bankIdx, Set $setIdx - Read value: 0x${readValue.litValue.toString(16)}%s, Expected: 0x${data.litValue.toString(16)}%s [$status]")
        dut.io.readAll.r(bankIdx).resp.data.head.asUInt.expect(data)
        dut.io.readAll.r(bankIdx).req.valid.poke(false.B)
      }
    }
    dut.io.readAll.act.poke(false.B)
  }

  def AMEStart(dut: AME, mtilem: Int, mtilen: Int, mtilek: Int,
               ms1: Int, ms2: Int, md: Int, rs1: Int, rs2: Int,
               valid: Bool, is_mmacc: Bool, is_mlbe8: Bool): Unit = {
    dut.io.Uop_io.mtileConfig_io.mtilem.poke(mtilem.U)
    dut.io.Uop_io.mtileConfig_io.mtilen.poke(mtilen.U)
    dut.io.Uop_io.mtileConfig_io.mtilek.poke(mtilek.U)
    dut.io.Uop_io.Operands_io.ms1.poke(ms1.U)
    dut.io.Uop_io.Operands_io.ms2.poke(ms2.U)
    dut.io.Uop_io.Operands_io.md.poke(md.U)
    dut.io.Uop_io.Operands_io.rs1.poke(rs1.U)
    dut.io.Uop_io.Operands_io.rs2.poke(rs2.U)
    dut.io.Uop_io.ShakeHands_io.valid.poke(valid)
    dut.io.Uop_io.InsType_io.is_mmacc.poke(is_mmacc)
    dut.io.Uop_io.InsType_io.is_mlbe8.poke(is_mlbe8)
  }

  def AMEStop(dut: AME): Unit = {
    AMEStart(dut, 0, 0, 0, 0, 0, 0, 0, 0, false.B, false.B, false.B)
  }
}
//完整Expander版本
class TestTop_L2L3_AME_Test extends AnyFreeSpec with Matchers {
  "AME should PASS" in {
    simulate(new AME) { dut =>
      /*  提前手动写入A、B、C*/
      AMETestHelper.writeTestDataToAll(AMETestData.A, 0, dut)
      AMETestHelper.writeTestDataToAll(AMETestData.B, 1, dut)
      AMETestHelper.writeTestDataToAll(AMETestData.Ctmp, 4, dut)
      AMETestHelper.writeTestDataToAll(AMETestData.Ctmp, 5, dut)
      AMETestHelper.writeTestDataToAll(AMETestData.Ctmp, 6, dut)
      AMETestHelper.writeTestDataToAll(AMETestData.Ctmp, 7, dut)

      dut.clock.step(1000) //随便跑几个cycle




      // // ins1
      var cycleCountMMAU = 0
      var cycleCountReady = 0

      AMETestHelper.AMEStart(dut, 32, 32, 64, 0, 1, 4, 0, 0, true.B, true.B, false.B)  //启动AME，配置矩阵形状，确定操作数矩阵标号（ABC标号范围均是0～7)

      while(!dut.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean){ //等到ready
        dut.clock.step(1)
        cycleCountReady += 1
      }

      dut.clock.step(1) //ready后需要主动前进一个时钟周期

println(s"ins 1 excuting")


      AMETestHelper.AMEStart(dut, 32, 32, 64, 0, 1, 5, 0, 0, true.B, true.B, false.B)  //下一条指令ins2，应该没有响应


      while(!dut.io.sigDone.peek().litToBoolean){ //等到执行完毕
        dut.clock.step(1)
        cycleCountMMAU += 1
      }


      println(s"cycleCountReady = $cycleCountReady , cycleCountMMAU = $cycleCountMMAU")


      // ins2

      cycleCountReady = 0
      cycleCountMMAU = 0  //计数清零


      // AMETestHelper.AMEStart(dut, 32, 32, 64, 0, 1, 5, true.B, true.B)  //下一条指令，换一个C

      while(!dut.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean){ //等到ready
        dut.clock.step(1)
        cycleCountReady += 1
      }

      dut.clock.step(1) //ready后需要主动前进一个时钟周期

println(s"ins 2 excuting")

      AMETestHelper.AMEStart(dut, 32, 32, 64, 0, 1, 6, 0, 0, true.B, true.B, false.B)  //下一条指令ins3，应该没有响应

      while(!dut.io.sigDone.peek().litToBoolean){ //等到执行完毕
        dut.clock.step(1)
        cycleCountMMAU += 1
      }

      println(s"cycleCountReady = $cycleCountReady , cycleCountMMAU = $cycleCountMMAU")


      // ins3

      cycleCountReady = 0
      cycleCountMMAU = 0  //计数清零


      // AMETestHelper.AMEStart(dut, 32, 32, 64, 0, 1, 6, true.B, true.B)  //下一条指令，换一个C

      while(!dut.io.Uop_io.ShakeHands_io.ready.peek().litToBoolean){ //等到ready
        dut.clock.step(1)
        cycleCountReady += 1
      }

      dut.clock.step(1) //ready后需要主动前进一个时钟周期

println(s"ins 3 excuting")

      while(!dut.io.sigDone.peek().litToBoolean){ //等到执行完毕
        dut.clock.step(1)
        cycleCountMMAU += 1
      }

      println(s"cycleCountReady = $cycleCountReady , cycleCountMMAU = $cycleCountMMAU")





      AMETestHelper.AMEStop(dut)  //停止，无效指令，valid = false

      AMETestHelper.readTestDataFromAll(AMETestData.C, 4, dut) //验证结果是否正确
      AMETestHelper.readTestDataFromAll(AMETestData.C, 5, dut) //验证结果是否正确
      AMETestHelper.readTestDataFromAll(AMETestData.C, 6, dut) //验证结果是否正确
      // AMETestHelper.readTestDataFromAll(AMETestData.C, 7, dut) //验证结果是否正确





    }
  }
}