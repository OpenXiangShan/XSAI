package amewrapper

import chisel3._
import chisel3.util.{log2Ceil, Decoupled, ValidIO, DecoupledIO}
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.dataview._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import scala.util.Random
import circt.stage.{ChiselStage, FirtoolOption}
import chisel3.stage.ChiselGeneratorAnnotation
import scala.util.Random

/**
 * Test Module for BarrelShift
 * This module instantiates BarrelShift with 8 inputs and allows testing different shift amounts
 */
class BarrelShiftTestModule extends Module {
  val io = IO(new Bundle {
    val in = Input(Vec(8, UInt(32.W)))
    val shiftAmount = Input(UInt(3.W))  // 3 bits for shift amount 0-7
    val out = Output(Vec(8, UInt(32.W)))
  })

  // Apply barrel shift using imported functions
  val shifted = BarrelShift(io.in, io.shiftAmount)

  // Connect output
  for (i <- 0 until 8) {
    io.out(i) := shifted(i)
  }
}

protected class DummyRoutableData extends Bundle {
  val data = UInt(32.W)
  val sink = UInt(3.W)
  val source = UInt(3.W)
}

/**
  * Give a barrel shifter to route the bank to the channel.
  * The shift amount is updated every cycle.
  *
  * @param outer
  * @param p
  */
class PollingShifter extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Vec(8, Decoupled(new DummyRoutableData)))
    val out = Vec(8, Decoupled(new DummyRoutableData))
    val shiftAmount = Output(UInt(log2Ceil(in.length).W))
  })

  class ShiftDataView extends Bundle {
    val valid = Bool()
    val bits = new DummyRoutableData
  }

  val dataViews = io.in.map(x => Wire(new ShiftDataView))
  dataViews.zip(io.in).foreach { case (view, port) =>
    view.valid := port.valid
    view.bits := port.bits
  }

  val shiftAmount = RegInit(0.U(log2Ceil(io.in.length).W))
  val shifted = BarrelShift(dataViews, shiftAmount)
  io.shiftAmount := shiftAmount

  shiftAmount := shiftAmount + 1.U

  for (i <- 0 until io.out.length) {
    io.out(i).valid := shifted(i).valid && shifted(i).bits.sink === i.U
    io.out(i).bits.data := shifted(i).bits.data
    io.out(i).bits.sink := shifted(i).bits.sink
    io.out(i).bits.source := shifted(i).bits.source
    io.in(i).ready := (i.U(3.W) +% shiftAmount) === io.in(i).bits.sink
  }
}

class PollingShifterTest extends AnyFreeSpec with Matchers {
  "PollingShifter should perform correct circular shifting" in {
    simulate(new PollingShifter) { dut =>
      val in = dut.io.in
      val out = dut.io.out
      val sinks = scala.util.Random.shuffle(Range(0, 8))
      in.zip(sinks).foreach { case (port, sink) =>
        port.valid.poke(true)
        port.bits.sink.poke(sink.U)
        port.bits.source.poke(sinks.indexOf(sink).U)
        port.bits.data.poke((0xdeafbeffL + sink).U)
      }

      for (_ <- 0 until 9) {
        // print io.out info
        println(s"======= shift amount: ${dut.io.shiftAmount.peek().litValue}")
        for (i <- 0 until 8) {
          println(f"in[$i] valid: ${in(i).valid.peek().litValue} ready: ${in(i).ready.peek().litValue} sink: ${in(i).bits.sink.peek().litValue} source: ${in(i).bits.source.peek().litValue} data: ${in(i).bits.data.peek().litValue.toInt}%x")
        }
        for (i <- 0 until 8) {
          println(f"out[$i] valid: ${out(i).valid.peek().litValue} sink: ${out(i).bits.sink.peek().litValue} source: ${out(i).bits.source.peek().litValue} data: ${out(i).bits.data.peek().litValue.toInt}%x")
        }
        dut.clock.step(1)
      }
    }
  }
}

/**
 * Test Helper Object
 */
object BarrelShiftTestHelper {
  def printShiftResult(shiftAmount: Int, input: Seq[Int], output: Seq[Int]): Unit = {
    println(s"Shift Amount: $shiftAmount")
    println(s"Input:  [${input.mkString(", ")}]")
    println(s"Output: [${output.mkString(", ")}]")
    println("-" * 50)
  }

  def expectedBarrelShift(input: Seq[Int], shiftAmount: Int): Seq[Int] = {
    val n = input.length
    val actualShift = shiftAmount % n
    if (actualShift == 0) {
      input
    } else {
      // The current BarrelShift implementation works as follows:
      // For each shift enable (i <= shiftAmount), it applies ShiftMux once
      // ShiftMux moves the last element to the front and shifts everything right
      // So for shift amount k, we need to apply this operation k times
      var result = input
      for (_ <- 0 until actualShift) {
        result = result.last +: result.init
      }
      result
    }
  }
}

/**
 * Main Test Class
 */
class BarrelShiftTest extends AnyFreeSpec with Matchers {
  "BarrelShift should perform correct circular shifting" in {
    simulate(new BarrelShiftTestModule) { dut =>
      // Test data: 8 different values
      val testInput = Seq(1, 2, 3, 4, 5, 6, 7, 8)

      // Set input values
      for (i <- 0 until 8) {
        dut.io.in(i).poke(testInput(i).U)
      }

      // Test all possible shift amounts (0-7)
      for (shiftAmount <- 0 until 8) {
        // Set shift amount
        dut.io.shiftAmount.poke(shiftAmount.U)

        // Step one cycle to propagate values
        dut.clock.step(1)

        // Read output values
        val output = (0 until 8).map(i => dut.io.out(i).peek().litValue.toInt)

        // Calculate expected result
        val expected = BarrelShiftTestHelper.expectedBarrelShift(testInput, shiftAmount)

        // Print results for debugging
        BarrelShiftTestHelper.printShiftResult(shiftAmount, testInput, output)

        // Verify results
        for (i <- 0 until 8) {
          output(i) mustBe expected(i)
        }

        println(s"Shift amount $shiftAmount: PASS")
        println()
      }
    }
  }

  "BarrelShift should handle distinct data correctly" in {
    simulate(new BarrelShiftTestModule) { dut =>
      // Test with different input values
      val testInput = Seq(0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x11, 0x22)

      // Set input values
      for (i <- 0 until 8) {
        dut.io.in(i).poke(testInput(i).U)
      }

      // Test specific shift amounts
      val testShifts = Seq(0, 1, 4, 7)

      for (shiftAmount <- testShifts) {
        dut.io.shiftAmount.poke(shiftAmount.U)
        dut.clock.step(1)

        val output = (0 until 8).map(i => dut.io.out(i).peek().litValue.toInt)
        val expected = BarrelShiftTestHelper.expectedBarrelShift(testInput, shiftAmount)

        println(s"Distinct data test - Shift amount: $shiftAmount")
        println(s"Input:  [${testInput.map(x => f"0x$x%02X").mkString(", ")}]")
        println(s"Output: [${output.map(x => f"0x$x%02X").mkString(", ")}]")
        println(s"Expected: [${expected.map(x => f"0x$x%02X").mkString(", ")}]")

        for (i <- 0 until 8) {
          output(i) mustBe expected(i)
        }

        println(s"Distinct data shift amount $shiftAmount: PASS")
        println("-" * 50)
      }
    }
  }
}