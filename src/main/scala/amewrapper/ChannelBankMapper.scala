package amewrapper

import chisel3._
import chisel3.util._
// import freechips.rocketchip.config._
import org.chipsalliance.cde.config.{Parameters, Field}
import AME._
import AME.{AME, connectPort}
import utility.sram._
import common._
import RegFile._
import MMAU._
import Expander._
import ScoreBoard._
import MLU._
import AME._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import coupledL2.{MatrixField, MatrixKey, MatrixDataBundle}


abstract class ChannelBankMapper(outer: AMEImp)(implicit p: Parameters) {
  val mlu_readback_io: Vec[Cacheline_ReadBack_IO] = outer.ame.io.MLU_L2_io.Cacheline_ReadBack_io
  val msu_write_io    = outer.ame.io.MSU_L2_io.Cacheline_Write_io
  val matrix_data_in  = outer.io.matrix_data_in
  val channel_id_bits = 3

  def getId(b: MatrixDataBundle): UInt = {
    val id = b.sourceId
    assert(id < 32.U, "id must be less than 32")
    id
  }

  def getChannel(b: MatrixDataBundle): UInt = {
    val channel = b.channel
    assert(channel < 8.U, "channel must be less than 8")
    channel
  }
}

/**
  * Basically maps bank {{i}} to channel {{i}}, which is not correct.
  */
class ChannelBankDirectMapper(outer: AMEImp)(implicit p: Parameters) extends ChannelBankMapper(outer) {
    for (i <- 0 until mlu_readback_io.length) {
    matrix_data_in(i).ready := true.B  // AME is receiver in M channel

    when(matrix_data_in(i).valid) {
      mlu_readback_io(i).valid := true.B
      mlu_readback_io(i).data := matrix_data_in(i).bits.data.data
      mlu_readback_io(i).id := getId(matrix_data_in(i).bits)
    }.otherwise {
      mlu_readback_io(i).valid := false.B
      mlu_readback_io(i).data := 0.U
      mlu_readback_io(i).id := 0.U
    }
  }
}

object ChannelBankDirectMap {
  def apply(outer: AMEImp)(implicit p: Parameters): ChannelBankMapper = {
    new ChannelBankDirectMapper(outer)
  }
}

/**
  * This mapper assumes all banks can be routed to all channels.
  * It uses {{NBanks}} {{NBanks}}-in-1-out arbiters.
  */
class ChannelBankFullMapper(outer: AMEImp)(implicit p: Parameters) extends ChannelBankMapper(outer) {
  val arbiterCount = mlu_readback_io.length
  val readback_arbiters = Seq.fill(arbiterCount)(Module(new Arbiter(new MatrixDataBundle(), mlu_readback_io.length)))

  for (i <- 0 until mlu_readback_io.length) {
    val id = getId(matrix_data_in(i).bits)
    val channel_id = getChannel(matrix_data_in(i).bits)

    // Full connection
    for (j <- 0 until arbiterCount) {
      readback_arbiters(j).io.in(i).valid         := matrix_data_in(i).valid && channel_id === j.U
      readback_arbiters(j).io.in(i).bits          := matrix_data_in(i).bits
      readback_arbiters(j).io.in(i).bits.sourceId := id
    }

    matrix_data_in(i).ready := readback_arbiters.zipWithIndex.map {
      case (arbiter, j) => arbiter.io.in(i).ready && (channel_id === j.U)
    }.reduce(_ || _)
  }

  for (i <- 0 until arbiterCount) {
    mlu_readback_io(i).valid          := readback_arbiters(i).io.out.valid
    mlu_readback_io(i).data           := readback_arbiters(i).io.out.bits.data.data
    mlu_readback_io(i).id             := readback_arbiters(i).io.out.bits.sourceId
    readback_arbiters(i).io.out.ready := true.B
  }
}

object ChannelBankFullMap {
  def apply(outer: AMEImp)(implicit p: Parameters): ChannelBankMapper = {
    new ChannelBankFullMapper(outer)
  }
}

object ShiftMux {
  def apply[T <: Data](in: Seq[T], shiftEnable: Bool, offset: Int = 1): Seq[T] = {
    assert(offset > 0, "offset must be greater than 0")
    // shift left
    // val newSeq: Seq[T] = in.slice(offset, in.length) ++ in.slice(0, offset)
    // shift right
    val newSeq: Seq[T] = in.slice(in.length - offset, in.length) ++ in.slice(0, in.length - offset)

    (in zip newSeq).map { case (keep, shift) =>
        Mux(shiftEnable, shift, keep)
    }
  }
}

/**
  * @param in
  * @param shiftAmount: 0 means no shift, less than {{in.length}}
  */
object BarrelShift {
    def apply[T <: Data](in: Seq[T], shiftAmount: UInt): Seq[T] = {
        // Convert UInt amount to the same count of 1s (not asBools or OH)
        // val shift_enables: Seq[Bool] = (1 until in.length).map(i => i.U <= shiftAmount)
        val amount = Wire(UInt(log2Ceil(in.length).W))
        amount := shiftAmount
        val shift_enables = amount.asBools
        println(s"shift level: ${shift_enables.length}")

        val shift_result = shift_enables.zipWithIndex.foldLeft(in) { case (acc, (enable, index)) =>
            ShiftMux(acc, enable, 1 << index)
        }

        shift_result
    }
}

/**
  * Give a barrel shifter to route the bank to the channel.
  * The shift amount is updated every cycle.
  *
  * @param outer
  * @param p
  */
class ChannelBankPollingMapper(outer: AMEImp)(implicit p: Parameters) extends ChannelBankMapper(outer) {
  val shiftAmount = RegInit(0.U(log2Ceil(matrix_data_in.length).W))

  class ShiftDataView extends Bundle {
    val valid = Bool()
    val bits = new MatrixDataBundle
  }

  val dataViews = matrix_data_in.map(x => Wire(new ShiftDataView))
  dataViews.zip(matrix_data_in).foreach { case (view, port) =>
    view.valid := port.valid
    view.bits := port.bits
    dontTouch(view)
  }

  val shifted_readback = BarrelShift(dataViews, shiftAmount)

  shiftAmount := shiftAmount + 1.U

  for (i <- 0 until mlu_readback_io.length) {
    val id = getId(shifted_readback(i).bits)
    val channel = getChannel(shifted_readback(i).bits)
    mlu_readback_io(i).valid := shifted_readback(i).valid && channel === i.U
    mlu_readback_io(i).data := shifted_readback(i).bits.data.data
    mlu_readback_io(i).id := id

    // Potential combinational loop here.
    matrix_data_in(i).ready := (i.U(channel_id_bits.W) +% shiftAmount) === matrix_data_in(i).bits.channel
  }
}