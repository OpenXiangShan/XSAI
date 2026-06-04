package cute

import circt.stage._
import chisel3.stage.ChiselGeneratorAnnotation

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config._
import xiangshan.backend.fu.matrix.Bundles._
import xiangshan.{XSCoreParamsKey, XSCoreParameters, XSBundle}
import system.{SoCParamsKey, SoCParameters}
import coupledL2.AmeIndexKey

import cute._

/**
  * CuteTestharness conducts MLA, MLB, MLC, MMA, MSC, and MacroIssue through
  * ConfigStateMachine, and uses MMUMonitorStateMachine to mimic ideal memory
  * responses. The ObserveStateMachine observes the final state of the execution
  * in parallel with the MMUMonitorStateMachine.
  */


// Provide default values for all signals assigned within state machines
object StateMachineDefaults {
  def applyDefaults(top: XSCuteTestTopImpl): Unit = {
    // ConfigStateMachine default assignments
    top.reset := false.B
    top.io.ctrl2top.amuCtrl.valid := false.B
    top.io.ctrl2top.amuCtrl.bits.data := 0.U
    top.io.ctrl2top.amuCtrl.bits.op := 0.U
    
    // MMUMonitorStateMachine default assignments
    top.tls.map(_(0)).foreach { tl =>
      tl.a.ready := false.B
      tl.d.valid := false.B
      tl.d.bits.data := 0.U
      tl.d.bits.opcode := 0.U
      tl.d.bits.source := 0.U
    }

    // Multi-channel matrix_data_in default assignments
    for (i <- 0 until 8) {
      top.io.matrix_data_in(i).valid := false.B
      top.io.matrix_data_in(i).bits := 0.U.asTypeOf(top.io.matrix_data_in(i).bits)
    }

    // ObserveStateMachine has no direct assignment signals, only reads
  }
}

// Configuration state machine class
class ConfigStateMachine(top: XSCuteTestTopImpl)(implicit p: Parameters) extends CuteConsts {
  // State definitions
  object ConfigState extends ChiselEnum {
    val sIdle, sReset, sInit, sLoadATensor, sLoadBTensor, sLoadCTensor, sComputeMMA, sStoreDTensor, sRelease, sConfigDone = Value
  }
  
  // State registers
  val state = RegInit(ConfigState.sIdle)
  
  // Test parameters
  val ATensor_Base_Addr = 0x10000000L.U(64.W)
  val ATensor_M_Stride = 64.U(64.W)
  val BTensor_Base_Addr = 0x20000000L.U(64.W)
  val BTensor_M_Stride = 64.U(64.W)
  val CTensor_Base_Addr = 0x30000000L.U(64.W)
  val CTensor_M_Stride = (128 * 4).U(64.W)
  val DTensor_Base_Addr = 0x40000000L.U(64.W)
  val DTensor_M_Stride = (128 * 4).U(64.W)
  val M = 128.U(64.W)
  val N = 128.U(64.W)
  val K = 64.U(64.W)

  // Communication methods
  def start(): Unit = {
    state := ConfigState.sReset
  }
  
  def running(): Bool = {
    state =/= ConfigState.sIdle && state =/= ConfigState.sConfigDone
  }
  
  def done(): Bool = {
    state === ConfigState.sConfigDone
  }

  def issueAmuCtrl(op: UInt, cmd: XSBundle)(implicit p: Parameters): Bool = {
    top.io.ctrl2top.amuCtrl.valid := true.B
    top.io.ctrl2top.amuCtrl.bits.op := op
    top.io.ctrl2top.amuCtrl.bits.data := cmd.asUInt
    top.io.ctrl2top.amuCtrl.fire
  }
  
  // State machine logic
  def update(cycle_cnt: UInt): Unit = {
    switch(state) {
      is(ConfigState.sIdle) {
        // Wait for start() call
      }

      is(ConfigState.sReset) {
        top.reset := true.B
        state := ConfigState.sInit
        printf("ConfigSM: Reset completed at cycle %d\n", cycle_cnt)
      }
      
      is(ConfigState.sInit) {
        top.reset := false.B
        state := ConfigState.sLoadATensor
        printf("ConfigSM: Initialization completed at cycle %d\n", cycle_cnt)
      }
      
      is(ConfigState.sLoadATensor) {
        printf("ConfigSM: Load A Tensor at cycle %d\n", cycle_cnt)
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.ms       := 0.U
        mls.baseAddr := ATensor_Base_Addr
        mls.stride   := ATensor_M_Stride
        mls.row      := M
        mls.column   := K
        mls.widths   := MSew.e8
        mls.ls       := false.B
        mls.isA      := true.B
        when(issueAmuCtrl(AmuCtrlIO.mlsOp(), mls)) {
          state := ConfigState.sLoadBTensor
        }
      }
      
      is(ConfigState.sLoadBTensor) {
        printf("ConfigSM: Load B Tensor at cycle %d\n", cycle_cnt)
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.ms       := 1.U
        mls.baseAddr := BTensor_Base_Addr
        mls.stride   := BTensor_M_Stride
        mls.row      := K
        mls.column   := N
        mls.widths   := MSew.e8
        mls.ls       := false.B
        mls.isB      := true.B
        when(issueAmuCtrl(AmuCtrlIO.mlsOp(), mls)) {
          state := ConfigState.sLoadCTensor
        }
      }
      
      is(ConfigState.sLoadCTensor) {
        printf("ConfigSM: Load C Tensor at cycle %d\n", cycle_cnt)
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.ms       := 0.U
        mls.baseAddr := CTensor_Base_Addr
        mls.stride   := CTensor_M_Stride
        mls.row      := M
        mls.column   := N
        mls.widths   := MSew.e32
        mls.ls       := false.B
        mls.isacc    := true.B
        when(issueAmuCtrl(AmuCtrlIO.mlsOp(), mls)) {
          state := ConfigState.sComputeMMA
        }
      }
      
      is(ConfigState.sComputeMMA) {
        printf("ConfigSM: Compute MMA at cycle %d\n", cycle_cnt)
        val mma = WireInit(0.U.asTypeOf(new AmuMmaIO))
        mma.md := 0.U
        mma.sat := false.B
        mma.ms1 := 0.U
        mma.ms2 := 1.U
        mma.mtilem := M
        mma.mtilen := N
        mma.mtilek := K
        mma.types1 := MSew.e8
        mma.types2 := MSew.e8
        mma.typed := MSew.e32
        mma.isfp := false.B
        when(issueAmuCtrl(AmuCtrlIO.mmaOp(), mma)) {
          state := ConfigState.sStoreDTensor
        }
      }
      
      is(ConfigState.sStoreDTensor) {
        printf("ConfigSM: Store D Tensor at cycle %d\n", cycle_cnt)
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.baseAddr := DTensor_Base_Addr
        mls.stride   := DTensor_M_Stride
        mls.row      := M
        mls.column   := N
        mls.widths   := MSew.e32
        mls.ls       := true.B
        mls.isacc    := true.B
        when(issueAmuCtrl(AmuCtrlIO.mlsOp(), mls)) {
          state := ConfigState.sRelease
        }
      }

      is(ConfigState.sRelease) {
        printf("ConfigSM: MRelease at cycle %d\n", cycle_cnt)
        val release = WireInit(0.U.asTypeOf(AmuReleaseIO2CUTE()))
        release.msyncRd := 1.U
        when(issueAmuCtrl(AmuCtrlIO.releaseOp(), release)) {
          state := ConfigState.sConfigDone
        }
      }
      
      is(ConfigState.sConfigDone) {
        // Configuration complete, maintain state
      }
    }
  }
}

// Simple memory request/response bundles shared between test harness and MMU monitor
class DutMemReq extends Bundle {
  val valid = Bool()
  val addr  = UInt(64.W)
  val write = Bool()
  val bytes = UInt(9.W)  // The beatBytes value.
  val data  = UInt(256.W)
}

class DutMemResp extends Bundle {
  val valid = Bool()
  val data  = UInt(512.W)
}

// MMU monitoring state machine class
class MMUMonitorStateMachine(
  channel:      Int,
  tl:           TLBundle,
  edge:         TLEdgeIn,
  memReq:       DutMemReq,
  memResp:      DutMemResp,
  matrixDataIn: DecoupledIO[MatrixDataIO]
)(implicit p: Parameters) {
  val timer = RegInit(0.U(32.W))
  timer := timer + 1.U

  // 16-bit LFSRs for pseudo-random number generation, one per AML/MMU port (seeded for different streams)
  val randLFSRs = Seq.tabulate(8) { i =>
    val seed = ((i + 1) * 17) & 0xFFFF // Just some simple, spread-out seeds
    RegInit(seed.U(16.W))
  }
  for (i <- 0 until 8) {
    val lfsr = randLFSRs(i)
    randLFSRs(i) := Cat(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10))
  }

  // State definitions
  object MMUState extends ChiselEnum {
    val sIdle, sMonitoring, sDone = Value
  }

  class PendingResponse extends Bundle {
    val message = UInt(3.W)
    val sourceID = UInt(64.W)
    val tlSource = UInt(64.W)
    val dueTime = UInt(32.W)
    val size = UInt(4.W)           // echoed TL size field
    val beatCounter = UInt(4.W)
    val totalBeats = UInt(4.W)
    val data = UInt(512.W)
  }

  // T7: 为每个通道创建独立的响应状态
  class ChannelResponseState extends Bundle {
    val valid = Bool()
    val message = UInt(3.W)
    val sourceID = UInt(32.W)
    val data = UInt(512.W)
    val size = UInt(4.W)
    val dueCycle = UInt(32.W)
  }

  // State registers
  val pendingSize = 128
  val pendingBits = log2Ceil(pendingSize)
  val state = RegInit(MMUState.sIdle)

  // T7: 为通道 0 保留原有的队列逻辑（B/C 通道）
  val pendingResponses = Reg(Vec(pendingSize, new PendingResponse))
  val pendingHead = RegInit(0.U(pendingBits.W))
  val pendingTail = RegInit(0.U(pendingBits.W))
  val pendingCount = RegInit(0.U((pendingBits + 1).W))
  val beatCount = RegInit(0.U(4.W))

  // T7: 为 AML 通道（1-7）创建简单的延迟响应状态
  val amlResponseStates = Reg(Vec(8, new ChannelResponseState))
  val amlReqData = Reg(Vec(8, UInt(512.W)))
  val amlReqAddr = Reg(Vec(8, UInt(64.W)))
  val amlReqSourceID = Reg(Vec(8, UInt(32.W)))

  // Queue operation helper functions
  def enqueuePendingResponse(
    message: UInt,
    sourceID: UInt,
    tlSource: UInt,
    dueTime: UInt,
    size: UInt,
    beatCounter: UInt,
    totalBeats: UInt,
    tailPtr: UInt
  ): Unit = {
    pendingResponses(tailPtr).message := message
    pendingResponses(tailPtr).sourceID := sourceID
    pendingResponses(tailPtr).tlSource := tlSource
    pendingResponses(tailPtr).dueTime := dueTime
    pendingResponses(tailPtr).size := size
    pendingResponses(tailPtr).beatCounter := beatCounter
    pendingResponses(tailPtr).data := memResp.data  // T7: 使用对应通道的数据
    pendingResponses(tailPtr).totalBeats := totalBeats
  }

  def enqueueMultipleBeats(
    message: UInt,
    sourceID: UInt,
    tlSource: UInt,
    size: UInt,
    baseDelay: UInt,
    numBeats: UInt,
    baseTailPtr: UInt
  ): Unit = {
    val maxPossibleBeats = 8
    for (beatIdx <- 0 until maxPossibleBeats) {
      when (beatIdx.U < numBeats) {
        val currentPtr = (baseTailPtr + beatIdx.U) % pendingSize.U
        enqueuePendingResponse(
          message = message,
          sourceID = sourceID,
          tlSource = tlSource,
          dueTime = baseDelay + randLFSRs(beatIdx) & 0x1ff.U,
          size = size,
          beatCounter = beatIdx.U,
          totalBeats = numBeats,
          tailPtr = currentPtr
        )
      }
    }
  }

  def dequeuePendingResponse(): PendingResponse = {
    pendingResponses(pendingHead)
  }

  def generateResponseData(resp: PendingResponse): UInt = {
    val beatPattern = Cat(resp.beatCounter, resp.sourceID(7,0))
    Mux(resp.message === TLMessages.AccessAckData,
      Cat(0xdeadbeefL.U, beatPattern, 0xcafebabeL.U),
      0.U)
  }

  def printResponseLog(cycle_cnt: UInt, resp: PendingResponse): Unit = {
    when (resp.message === TLMessages.AccessAck) {
      printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Response sent:" +
          cf"  Response Message: ${resp.message} (Ack)" +
          cf"  Source ID: ${resp.sourceID}" +
          cf"  Size: ${resp.size}\n")
    }.otherwise {
      printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Response sent:" +
          cf"  Response Message: ${resp.message} (AckData)" +
          cf"  Beat: ${resp.beatCounter}/${resp.totalBeats}" +
          cf"  Response Data: 0x${resp.data}%x" +
          cf"  Source ID: ${resp.sourceID}" +
          cf"  Size: ${resp.size}\n")
    }
  }

  def updateQueuePointers(new_head_ptr: UInt, new_tail_ptr: UInt, cnt_inc: UInt, cnt_dec: UInt): Unit = {
    val pendingCountNew = pendingCount + cnt_inc - cnt_dec
    pendingHead := new_head_ptr
    pendingTail := new_tail_ptr
    pendingCount := pendingCountNew
  }

  // Communication methods
  def start(): Unit = {
    state := MMUState.sMonitoring
  }
  
  def running(): Bool = {
    state === MMUState.sMonitoring
  }
  
  def done(): Bool = {
    state === MMUState.sDone
  }
  
  // State machine logic
  def update(cycle_cnt: UInt): Unit = {
    switch(state) {
      is(MMUState.sIdle) {
        // Wait for start() call
      }

      is(MMUState.sMonitoring) {
        // T7: 每个 MMUSM 只负责一个特定的通道
        tl.a.ready := (pendingCount < (pendingSize - 8).U)
        val last = edge.last(tl.a)
        val countInc = WireInit(0.U(4.W))
        val countDec = WireInit(0.U(4.W))
        val newHeadPtr = WireInit(pendingHead)
        val newTailPtr = WireInit(pendingTail)

        // Check MMU requests
        when(tl.a.fire) {
          // Default response info for Put request.
          val respMsg = WireInit(TLMessages.AccessAck)
          val respBeats = WireInit(1.U(4.W))

          val opcode = tl.a.bits.opcode
          val is_write = opcode === TLMessages.PutFullData || opcode === TLMessages.PutPartialData
          val param = tl.a.bits.param
          val physAddr = tl.a.bits.address
          val requestData = tl.a.bits.data
          val ameIndex = tl.a.bits.user.lift(AmeIndexKey).getOrElse(233.U)
          val tlSource = tl.a.bits.source
          val sourceID = ameIndex
          val size = tl.a.bits.size
          val beatBytes = (tl.a.bits.data.getWidth / 8).U

          printf(cf"[TestHarness] Channel[$channel] REQ: ameIndex=$ameIndex, tl.a.bits.source=$tlSource, sourceID=$sourceID, addr=0x${physAddr}%x\n")

          memReq.valid := true.B
          memReq.addr := physAddr + beatBytes * beatCount
          memReq.write := is_write
          memReq.data := requestData
          memReq.bytes := Mux(is_write, beatBytes, 64.U)
          beatCount := beatCount + 1.U

          // Log request to outer memory interface
          printf(cf"[ChiselHarness] Channel[$channel] REQ: valid=1, addr=0x${memReq.addr}%x, write=${is_write}, bytes=${memReq.bytes}, data=0x${memReq.data}%x\n")

          when (!is_write) {
            assert(memResp.valid, s"read data should be valid for channel $channel.")
            // Log read response from outer memory interface
            printf(cf"[ChiselHarness] Channel[$channel] RESP: valid=${memResp.valid}, data=0x${memResp.data}%x\n")
          }

          printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Request detected:" +
              cf"  Physical Address: 0x${physAddr}%x" +
              cf"  opcode ${opcode} param ${param}" +
              cf"  Source ID: ${sourceID}" +
              cf"  Size: ${size}" +
              cf"  Beat Bytes: ${beatBytes}\n" +
              cf"  Data: 0x${requestData}%x\n")

          when (opcode === TLMessages.Get) {
            respMsg := TLMessages.AccessAckData
            val dummy_d = edge.AccessAck(tl.a.bits, 0.U)
            // respBeats := top.edgeIn.numBeats(dummy_d)
            assert(respBeats <= 8.U, "Unsupported number of beats > 8")
            assert(pendingCount + respBeats <= pendingSize.U, "Pending queue overflow")
          }

          when (last) {
            enqueueMultipleBeats(
              message = respMsg,
              sourceID = sourceID,
              tlSource = 0.U,
              size = size,
              baseDelay = timer,
              numBeats = respBeats,
              baseTailPtr = pendingTail
            )
            newTailPtr := (pendingTail + respBeats) % pendingSize.U
            countInc := respBeats
            beatCount := 0.U
          }
        }

        // Handle pending requests (only for channel 0)
        when(pendingCount > 0.U) {
          val resp = dequeuePendingResponse()

          // T7: 调试 - 打印 pendingCount 和 resp.sourceID
          printf(cf"[TestHarness $timer] Ch[$channel] pendingCount=$pendingCount, pendingHead=$pendingHead, resp{sourceID=${resp.sourceID}, message=${resp.message}, due=${resp.dueTime}}, matrix_data_ready ${matrixDataIn.ready}, tl_d_ready ${tl.d.ready}\n")

          def bindResponse(dest: DecoupledIO[TLBundleD]): Unit = {
            dest.valid := resp.dueTime <= timer
            dest.bits.opcode := resp.message
            dest.bits.size := resp.size
            dest.bits.source := resp.tlSource
            dest.bits.data := resp.data
            dest.bits.user.lift(AmeIndexKey).foreach(_ := resp.sourceID)
          }

          def bindMatrixDataResponse(dest: DecoupledIO[MatrixDataIO]): Unit = {
            dest.valid := resp.dueTime <= timer
            dest.bits.data := resp.data
            dest.bits.source := resp.sourceID
          }

          when (resp.message === TLMessages.AccessAckData) {
            bindMatrixDataResponse(matrixDataIn)
          } .elsewhen (resp.message === TLMessages.AccessAck) {
            bindResponse(tl.d)
          }

          when (tl.d.fire || matrixDataIn.fire) {
            newHeadPtr := (pendingHead + 1.U) % pendingSize.U
            countDec := 1.U
            printResponseLog(cycle_cnt, resp)
          }
        }

        updateQueuePointers(newHeadPtr, newTailPtr, countInc, countDec)
      }

      is(MMUState.sDone) {
        // Monitoring complete, maintain state
      }
    }
  }
}

// Observation state machine class
class ObserveStateMachine(top: XSCuteTestTopImpl)(implicit p: Parameters) {
  // State definitions
  object ObserveState extends ChiselEnum {
    val sIdle, sObserving, sDone = Value
  }
  
  // State registers
  val state = RegInit(ObserveState.sIdle)
  val observe_cycles = RegInit(0.U(64.W))
  
  // Communication methods
  def start(): Unit = {
    state := ObserveState.sObserving
    observe_cycles := 0.U
  }
  
  def running(): Bool = {
    state === ObserveState.sObserving
  }
  
  def done(): Bool = {
    state === ObserveState.sDone
  }
  
  // State machine logic
  def update(cycle_cnt: UInt): Unit = {
    switch(state) {
      is(ObserveState.sIdle) {
        // Wait for start() call
      }
      
      is(ObserveState.sObserving) {
        
        when(observe_cycles % 100.U === 0.U) {
          printf("ObserveSM: Execution cycle %d.\n",
                 observe_cycles)
        }
        
        observe_cycles := observe_cycles + 1.U
        when(top.io.ctrl2top.mrelease.valid) { // TODO: set the condition for finishing observation
          state := ObserveState.sDone
          val token_bits = top.io.ctrl2top.mrelease.bits.msyncRd.asUInt
          val token_id = OHToUInt(token_bits)
          printf("ObserveSM: Final observation completed at cycle %d, release token %b, %d\n", cycle_cnt, token_bits, token_id)
        }
      }
      
      is(ObserveState.sDone) {
        // Observation complete, maintain state
      }
    }
  }
}

class CuteTestHarness(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
    val req = Vec(8, Output(new DutMemReq))
    val resp = Vec(8, Input(new DutMemResp))
  })

  val top = Module(LazyModule(new XSCuteTestTop).module)

  top.io := DontCare
  top.tls.map(_(0)).foreach(_ := DontCare)
  
  val configSM = new ConfigStateMachine(top)
  val observeSM = new ObserveStateMachine(top)
  val mmuSMs = Seq.tabulate(8)(i =>
    new MMUMonitorStateMachine(
      channel      = i,
      tl           = top.tls(i)(0),
      edge         = top.edgeIns(i),
      memReq       = io.req(i),
      memResp      = io.resp(i),
      matrixDataIn = top.io.matrix_data_in(i)
    )
  )
  
  // Main control state machine definition
  object MainState extends ChiselEnum {
    val sIdle, sConfigPhase, sMonitorPhase, sSuccess = Value
  }

  val main_state = RegInit(MainState.sIdle)
  val cycle_cnt = RegInit(0.U(32.W))
  
  // TestHarness default assignments (before state machine logic)
  io.success := false.B
  // main_state maintains current value, no default assignment needed (initialized via RegInit)

  // Default assignments for multi-channel memory request interface (outputs only)
  for (i <- 0 until 8) {
    io.req(i).valid := false.B
    io.req(i).addr := 0.U
    io.req(i).write := false.B
    io.req(i).bytes := 0.U
    io.req(i).data := 0.U
  }
  // Note: io.resp inputs are not assigned here (they come from the test driver)
  
  // Apply default values for state machine signals
  StateMachineDefaults.applyDefaults(top)
  
  // Global cycle counter
  cycle_cnt := cycle_cnt + 1.U
  
  // Update all state machines
  configSM.update(cycle_cnt)
  mmuSMs.foreach(_.update(cycle_cnt))  // T7: 更新所有 8 个 MMUSM
  observeSM.update(cycle_cnt)

  // Main control state machine
  switch(main_state) {
    is(MainState.sIdle) {
      printf("TestHarness: Starting matrix multiplication test at cycle %d\n", cycle_cnt)
      main_state := MainState.sConfigPhase
      configSM.start()
    }

    is(MainState.sConfigPhase) {
      when(configSM.done()) {
        printf("TestHarness: Configuration completed, starting monitoring phases at cycle %d\n", cycle_cnt)
        main_state := MainState.sMonitorPhase
        mmuSMs.foreach(_.start())
        observeSM.start()
      }
    }

    is(MainState.sMonitorPhase) {
      when(observeSM.done()) {
        printf("TestHarness: All monitoring completed at cycle %d\n", cycle_cnt)
        main_state := MainState.sSuccess
      }
    }
    
    is(MainState.sSuccess) {
      io.success := true.B
      printf("TestHarness: Matrix multiplication test completed successfully at cycle %d\n", cycle_cnt)
    }
  }
}
