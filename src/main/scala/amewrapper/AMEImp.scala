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
import coupledL2.{MatrixField, MatrixKey, MatrixDataBundle, AmeChannelKey, AmeIndexKey}
import utility.XSPerfAccumulate

// AME Parameters case class
case class AMEParams(
  numTrRegs: Int = 2,      // Number of Tr registers
  numAccRegs: Int = 1,     // Number of Acc registers
  dataWidth: Int = 32,     // Data width
  matrixSize: Int = 16     // Matrix size (NxN)
)

// AME Configuration Object
// object AMEConfigKey {
//   val AMEConfig = new Field[AMEParams]
// }
case object AMEConfigKey extends Field[AMEParams]// 需要定义(AMEParam())

// AME IO Bundle
class AMEIO(implicit p: Parameters) extends Bundle {
  val Uop_io = new Uop_IO
  val writeAll = new RegFileAllWrite_IO
  val readAll = new RegFileAllRead_IO
  val sigDone = Output(Bool())
  val matrix_data_in = Vec(8, Flipped(DecoupledIO(new MatrixDataBundle())))  
  // val matrix_data_in = Vec(8, DecoupledIO(new MatrixDataBundle()))  // For M channel responses
  val amuRelease = Decoupled(new AmuRelease_IO)
}

// AME Implementation
class AMEImp(outer: AMEModule)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = IO(new AMEIO)
  
  // Get TileLink interfaces for all 8 matrix nodes
  val (tls, edges) = outer.matrix_nodes.map(_.out.head).unzip

  val ame = Module(new AME)
  ame.io.Uop_io <> io.Uop_io
  ame.io.writeAll <> io.writeAll
  ame.io.readAll <> io.readAll
  ame.io.amuRelease_io <> io.amuRelease
  io.sigDone := ame.io.sigDone

  // M channel handling (for Get responses)
  val mlu_read_io = ame.io.MLU_L2_io.Cacheline_Read_io
  val mlu_readback_io = ame.io.MLU_L2_io.Cacheline_ReadBack_io
  val msu_write_io = ame.io.MSU_L2_io.Cacheline_Write_io
  val msu_writeback_io = ame.io.MSU_L2_io.Cacheline_WriteBack_io
  val matrix_data_in = io.matrix_data_in

  val ame_data_bits = Consts.L2_DATA_LEN
  val tl_data_bits = tls(0).params.dataBits
  val write_beats = ame_data_bits / tl_data_bits
  println(s"[AMEModule] write beats: $write_beats, ame_data_bits: $ame_data_bits, tl_data_bits: $tl_data_bits")
  require(write_beats == 2, "write_beats must be 2")

  // Use mlu_read_io length for unified processing when iterating through mlu_read_io
  // Since msu_write_io length is less than mlu_read_io, extra entries will be optimized away
  val write_inflight = RegInit(VecInit(Seq.fill(mlu_read_io.length)(false.B)))
  require(msu_write_io.length <= mlu_read_io.length, "guard write_inflight indexing")

  val channel_bank_mapper: ChannelBankMapper = new ChannelBankPollingMapper(this)

  tls.foreach { tl =>
    tl.a.valid := false.B
    tl.a.bits := 0.U.asTypeOf(tl.a.bits)

    // D channel - Only used for Put responses
    tl.d.ready := true.B

    // We don't use B, C channels for this interface
    tl.b.ready := true.B
    tl.c.valid := false.B
    tl.c.bits := 0.U.asTypeOf(tl.c.bits)
    tl.e.valid := false.B
    tl.e.bits := 0.U.asTypeOf(tl.e.bits)
  }

  // Connect each MLU Cacheline interface to its corresponding TileLink node
  for (i <- 0 until ame.io.MLU_L2_io.Cacheline_Read_io.length) {
    println(s"Connecting MLU Cacheline interface $i to TileLink node")
    val tl = tls(i)
    val edge = edges(i)

    // A channel - Get request handling
    // tl.a.bits.user(MatrixKey):= 1.U
    mlu_read_io(i).ready := tl.a.ready && !write_inflight(i)

    when(mlu_read_io(i).valid) {
      val (legal, get_bits) = edge.Get(
        fromSource = 0.U,
        toAddress = mlu_read_io(i).addr,
        lgSize = 6.U // 64 bytes = 2^6
      )
      
      tl.a.valid := legal
      tl.a.bits := get_bits
      tl.a.bits.user(MatrixKey):= 1.U  // Mark as Matrix request
      tl.a.bits.user(AmeChannelKey) := i.U
      tl.a.bits.user(AmeIndexKey) := mlu_read_io(i).id
    }.otherwise {
      tl.a.bits.user(MatrixKey):= 0.U
      tl.a.valid := false.B
    }

    // D channel - Only used for Put responses
    tl.d.ready := true.B

    // We don't use B, C channels for this interface
    tl.b.ready := true.B
    tl.c.valid := false.B
    tl.e.valid := false.B
  }

  // MSU_L2 <> TileLink
  for (i <- 0 until ame.io.MSU_L2_io.Cacheline_Write_io.length) {
    val tl = tls(i)
    val edge = edges(i)

    val beat_count = RegInit(0.U(2.W))
    val next_beat_data = RegInit(0.U(tl_data_bits.W))  // Only latch the hi-half of msu data.
    val msu_addr_reg = RegInit(0.U(Consts.L2_ADDR_LEN.W))

    // MSU_L2_WRITE to TileLink Put
    msu_write_io(i).ready := tl.a.ready && !mlu_read_io(i).valid && !write_inflight(i)
    val msu_write_fire = msu_write_io(i).valid && msu_write_io(i).ready

    // Present the first beat to tilelink A when mlu does not require A.
    when (!mlu_read_io(i).valid && msu_write_io(i).valid && !write_inflight(i)) {
      val (legal, put_bits) = edge.Put(
        fromSource = 0.U,
        toAddress = msu_write_io(i).addr,
        lgSize = log2Ceil(Consts.L2_DATA_LEN / 8).U,
        data = msu_write_io(i).data
      )
      assert(legal, "MSU_L2_WRITE to TileLink Put is illegal")

      tl.a.valid := legal
      tl.a.bits := put_bits
      tl.a.bits.user(MatrixKey) := 1.U  // Mark as Matrix request
    }

    // Register state when msu rised tl_a fires.
    when (tl.a.fire && msu_write_fire) {
        msu_addr_reg := msu_write_io(i).addr
        next_beat_data := msu_write_io(i).data(ame_data_bits - 1, tl_data_bits)
        write_inflight(i) := true.B
    }

    // Next beat.
    when (write_inflight(i)) {
      val (legal, put_bits) = edge.Put(
        fromSource = 0.U,
        toAddress = msu_addr_reg,
        lgSize = log2Ceil(Consts.L2_DATA_LEN / 8).U,
        data = next_beat_data
      )
      assert(legal, "MSU_L2_WRITE to TileLink Put is illegal")

      tl.a.valid := legal
      tl.a.bits := put_bits
      tl.a.bits.user(MatrixKey) := 1.U  // Mark as Matrix request
      tl.a.bits.user(AmeChannelKey) := i.U
      tl.a.bits.user(AmeIndexKey) := 0.U

      when (tl.a.fire) {
        write_inflight(i) := false.B
        beat_count := 0.U
        next_beat_data := 0.U(tl_data_bits.W)
        msu_addr_reg := 0.U(Consts.L2_ADDR_LEN.W)
      }
    }

    // TileLink Ack to MSU_L2_WRITEBACK
    msu_writeback_io(i).valid := false.B
    when (tl.d.valid && (tl.d.bits.opcode === TLMessages.AccessAck || tl.d.bits.opcode === TLMessages.ReleaseAck)) {
      msu_writeback_io(i).valid := true.B
      // printf(s"MSU_L2_WRITEACK: channel=$i\n")
    }
  }

  /**
    * Performance monitoring
    */
  val tick_data_stall = VecInit(io.matrix_data_in.map(x => x.valid && !x.ready))
  val tick_data_stall_count = PopCount(tick_data_stall)
  XSPerfAccumulate("AME_CYCLES_DATA_STALL", tick_data_stall_count)
}