package cute

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}
import org.chipsalliance.cde.config.Parameters

private object CutePerfDeltaParams {
  val Width = 24
}

private class CutePerfDelta(implicit p: Parameters) extends CuteBundle {
  val deltas = Vec(
    CutePerfEventCounts.Backend + CutePerfEventCounts.Mem,
    UInt(CutePerfDeltaParams.Width.W)
  )
}

/** Transfers CUTE performance-event deltas without assuming a fixed clock ratio. */
class CutePerfAsyncCrossing(params: AsyncQueueParams)(implicit p: Parameters) extends RawModule {
  val io = IO(new Bundle {
    val sourceClock = Input(Clock())
    val sourceReset = Input(Bool())
    val source = Input(new CutePerfToCoreIO)
    val sinkClock = Input(Clock())
    val sinkReset = Input(Bool())
    val sink = Output(new CutePerfToCoreIO)
  })

  private val eventCount = CutePerfEventCounts.Backend + CutePerfEventCounts.Mem
  private val deltaWidth = CutePerfDeltaParams.Width
  require(params.depth == 1, "CUTE performance delta crossing expects a depth-1 AsyncQueue")

  private val crossing = Module(new AsyncQueue(new CutePerfDelta, params))
  crossing.io.enq_clock := io.sourceClock
  crossing.io.enq_reset := io.sourceReset
  crossing.io.deq_clock := io.sinkClock
  crossing.io.deq_reset := io.sinkReset

  // CUTE-side event values describe the increment in the current accelerator cycle.
  // Accumulate new events separately while a frozen delta packet waits for the
  // asynchronous queue, so the Decoupled payload stays stable under backpressure.
  val sourceEvents = io.source.backendEvents ++ io.source.memEvents
  require(sourceEvents.forall(_.getWidth <= deltaWidth))

  val pendingDelta = withClockAndReset(io.sourceClock, io.sourceReset.asAsyncReset) {
    RegInit(VecInit(Seq.fill(eventCount)(0.U(deltaWidth.W))))
  }
  val txDelta = withClockAndReset(io.sourceClock, io.sourceReset.asAsyncReset) {
    RegInit(VecInit(Seq.fill(eventCount)(0.U(deltaWidth.W))))
  }
  val txValid = withClockAndReset(io.sourceClock, io.sourceReset.asAsyncReset) {
    RegInit(false.B)
  }
  val accumulatedDelta = Wire(Vec(eventCount, UInt((deltaWidth + 1).W)))

  for (i <- 0 until eventCount) {
    accumulatedDelta(i) := pendingDelta(i) +& sourceEvents(i)
  }
  val accumulatedNonZero = accumulatedDelta
    .map(sum => sum(deltaWidth - 1, 0).orR)
    .reduce(_ || _)
  val accumulatedOverflow = accumulatedDelta
    .map(sum => sum(deltaWidth))
    .reduce(_ || _)

  crossing.io.enq.valid := txValid
  crossing.io.enq.bits.deltas := txDelta

  withClockAndReset(io.sourceClock, io.sourceReset.asAsyncReset) {
    assert(!accumulatedOverflow, "CUTE performance-event pending delta overflow")

    when(crossing.io.enq.fire) {
      // The old frozen packet left this cycle. Move all events accumulated while
      // it was stalled, including this cycle's events, directly into the next packet.
      when(accumulatedNonZero) {
        for (i <- 0 until eventCount) {
          txDelta(i) := accumulatedDelta(i)(deltaWidth - 1, 0)
          pendingDelta(i) := 0.U
        }
        txValid := true.B
      }.otherwise {
        txValid := false.B
      }
    }.elsewhen(!txValid && accumulatedNonZero) {
      // Freeze a new packet. It must remain unchanged until the queue accepts it.
      for (i <- 0 until eventCount) {
        txDelta(i) := accumulatedDelta(i)(deltaWidth - 1, 0)
        pendingDelta(i) := 0.U
      }
      txValid := true.B
    }.otherwise {
      // A packet is already waiting, so retain all newer events in the accumulator.
      for (i <- 0 until eventCount) {
        pendingDelta(i) := accumulatedDelta(i)(deltaWidth - 1, 0)
      }
    }
  }

  // The XSAI PerfEvent interface accepts at most 63 events of each type per
  // core cycle. Drain a received 24-bit delta over as many core cycles as needed.
  // Backpressure the asynchronous queue if accepting another packet would
  // overflow the sink-side accumulator.
  val sinkPending = withClockAndReset(io.sinkClock, io.sinkReset.asAsyncReset) {
    RegInit(VecInit(Seq.fill(eventCount)(0.U(deltaWidth.W))))
  }
  val sinkEvents = Wire(Vec(eventCount, UInt(6.W)))
  val sinkRemaining = Wire(Vec(eventCount, UInt(deltaWidth.W)))
  val sinkMerged = Wire(Vec(eventCount, UInt((deltaWidth + 1).W)))

  for (i <- 0 until eventCount) {
    sinkEvents(i) := Mux(sinkPending(i) > 63.U, 63.U, sinkPending(i)(5, 0))
    sinkRemaining(i) := sinkPending(i) - sinkEvents(i)
    sinkMerged(i) := sinkRemaining(i) +& crossing.io.deq.bits.deltas(i)
  }

  val sinkCanAccept = sinkMerged
    .map(sum => !sum(deltaWidth))
    .reduce(_ && _)
  crossing.io.deq.ready := !crossing.io.deq.valid || sinkCanAccept

  withClockAndReset(io.sinkClock, io.sinkReset.asAsyncReset) {
    for (i <- 0 until eventCount) {
      sinkPending(i) := Mux(
        crossing.io.deq.fire,
        sinkMerged(i)(deltaWidth - 1, 0),
        sinkRemaining(i)
      )
    }
  }

  io.sink.backendEvents := sinkEvents.take(CutePerfEventCounts.Backend)
  io.sink.memEvents := sinkEvents.drop(CutePerfEventCounts.Backend)
}
