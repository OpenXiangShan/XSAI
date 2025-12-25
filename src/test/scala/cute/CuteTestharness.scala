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
import xiangshan.{XSCoreParamsKey, XSCoreParameters}
import system.{SoCParamsKey, SoCParameters}

import cute._

/**
  * CuteTestharness conducts MLA, MLB, MLC, MMA, MSC, and MacroIssue through
  * ConfigStateMachine, and uses MMUMonitorStateMachine to mimic ideal memory
  * responses. The ObserveStateMachine observes the final state of the execution
  * in parallel with the MMUMonitorStateMachine.
  */


// Provide default values for all signals assigned within state machines
object StateMachineDefaults {
  def applyDefaults(top: XSCuteTopImpl): Unit = {
    // ConfigStateMachine default assignments
    top.reset := false.B
    top.io.ctrl2top.amuCtrl.valid := false.B
    top.io.ctrl2top.amuCtrl.bits.data := 0.U
    top.io.ctrl2top.amuCtrl.bits.op := 0.U
    
    // MMUMonitorStateMachine default assignments
    top.tl.foreach { tl =>
      tl.a.ready := false.B
      tl.d.valid := false.B
      tl.d.bits.data := 0.U
      tl.d.bits.opcode := 0.U
      tl.d.bits.source := 0.U
    }
    
    // ObserveStateMachine has no direct assignment signals, only reads
  }
}

// Configuration state machine class
class ConfigStateMachine(top: XSCuteTopImpl)(implicit p: Parameters) extends CuteConsts {
  // State definitions
  object ConfigState extends ChiselEnum {
    val sIdle, sReset, sInit, sLoadATensor, sLoadBTensor, sLoadCTensor, sComputeMMA, sStoreDTensor, sConfig, sConfigDone = Value
  }
  
  // State registers
  val state = RegInit(ConfigState.sIdle)
  
  // Test parameters
  val ATensor_Base_Addr = 0x10000000L.U(64.W)
  val ATensor_M_Stride = 128.U(64.W)
  val BTensor_Base_Addr = 0x20000000L.U(64.W)
  val BTensor_M_Stride = 128.U(64.W)
  val CTensor_Base_Addr = 0x30000000L.U(64.W)
  val CTensor_M_Stride = 128.U(64.W)
  val DTensor_Base_Addr = 0x40000000L.U(64.W)
  val DTensor_M_Stride = 128.U(64.W)
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
        top.io.ctrl2top.amuCtrl.valid := true.B
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.baseAddr := ATensor_Base_Addr
        mls.stride   := ATensor_M_Stride
        mls.row      := M
        mls.column   := K
        mls.widths   := MSew.e8
        mls.ls       := false.B
        mls.isA      := true.B
        top.io.ctrl2top.amuCtrl.bits.data := mls.asUInt
        top.io.ctrl2top.amuCtrl.bits.op := AmuCtrlIO.mlsOp()
        state := ConfigState.sLoadBTensor
      }
      
      is(ConfigState.sLoadBTensor) {
        printf("ConfigSM: Load B Tensor at cycle %d\n", cycle_cnt)
        top.io.ctrl2top.amuCtrl.valid := true.B
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.baseAddr := BTensor_Base_Addr
        mls.stride   := BTensor_M_Stride
        mls.row      := N
        mls.column   := K
        mls.widths   := MSew.e8
        mls.ls       := false.B
        mls.isB      := true.B
        top.io.ctrl2top.amuCtrl.bits.data := mls.asUInt
        top.io.ctrl2top.amuCtrl.bits.op := AmuCtrlIO.mlsOp()
        state := ConfigState.sLoadCTensor
      }
      
      is(ConfigState.sLoadCTensor) {
        printf("ConfigSM: Load C Tensor at cycle %d\n", cycle_cnt)
        top.io.ctrl2top.amuCtrl.valid := true.B
        val mls = WireInit(0.U.asTypeOf(new AmuLsuIO))
        mls.baseAddr := CTensor_Base_Addr
        mls.stride   := CTensor_M_Stride
        mls.row      := M
        mls.column   := N
        mls.widths   := MSew.e32
        mls.ls       := false.B
        mls.isacc    := true.B
        top.io.ctrl2top.amuCtrl.bits.data := mls.asUInt
        top.io.ctrl2top.amuCtrl.bits.op := AmuCtrlIO.mlsOp()
        state := ConfigState.sComputeMMA
      }
      
      is(ConfigState.sComputeMMA) {
        printf("ConfigSM: Compute MMA at cycle %d\n", cycle_cnt)
        top.io.ctrl2top.amuCtrl.valid := true.B
        val mma = WireInit(0.U.asTypeOf(new AmuMmaIO))
        mma.md := 0.U
        mma.sat := false.B
        mma.ms1 := 0.U
        mma.ms2 := 0.U
        mma.mtilem := M
        mma.mtilen := N
        mma.mtilek := K
        mma.types := MSew.e8
        mma.typed := MSew.e32
        mma.isfp := false.B
        top.io.ctrl2top.amuCtrl.bits.data := mma.asUInt
        top.io.ctrl2top.amuCtrl.bits.op := AmuCtrlIO.mmaOp()
        state := ConfigState.sStoreDTensor
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
        top.io.ctrl2top.amuCtrl.valid := true.B
        top.io.ctrl2top.amuCtrl.bits.data := mls.asUInt
        top.io.ctrl2top.amuCtrl.bits.op := AmuCtrlIO.mlsOp()
        state := ConfigState.sConfigDone
      }
      
      is(ConfigState.sConfigDone) {
        // Configuration complete, maintain state
      }
    }
  }
}

// MMU monitoring state machine class
class MMUMonitorStateMachine(top: XSCuteTopImpl)(implicit p: Parameters) {
  val timer = RegInit(0.U(32.W))
  timer := timer + 1.U

  // 16-bit LFSR for pseudo-random number generation
  val randLFSR = RegInit(1.U(16.W))
  randLFSR := Cat(randLFSR(14,0), randLFSR(15) ^ randLFSR(13) ^ randLFSR(12) ^ randLFSR(10))

  // State definitions
  object MMUState extends ChiselEnum {
    val sIdle, sMonitoring, sDone = Value
  }

  class PendingResponse extends Bundle {
    val message = UInt(3.W)
    val sourceID = UInt(32.W)
    val dueTime = UInt(32.W)
    val size = UInt(4.W)           // echoed TL size field
    val beatCounter = UInt(4.W)
    val totalBeats = UInt(4.W)
  }
  
  // State registers
  val pendingSize = 128
  val pendingBits = log2Ceil(pendingSize)
  val state = RegInit(MMUState.sIdle)
  val pendingResponses = Reg(Vec(pendingSize, new PendingResponse))
  val pendingHead = RegInit(0.U(pendingBits.W))
  val pendingTail = RegInit(0.U(pendingBits.W))
  val pendingCount = RegInit(0.U((pendingBits + 1).W))
  
  val a_reg = Reg(top.tl(0).a.bits.cloneType)
  val a_reg_valid = RegInit(false.B)

  // Queue operation helper functions
  def enqueuePendingResponse(
    message: UInt,
    sourceID: UInt,
    dueTime: UInt,
    size: UInt,
    beatCounter: UInt,
    totalBeats: UInt,
    tailPtr: UInt
  ): Unit = {
    pendingResponses(tailPtr).message := message
    pendingResponses(tailPtr).sourceID := sourceID
    pendingResponses(tailPtr).dueTime := dueTime
    pendingResponses(tailPtr).size := size
    pendingResponses(tailPtr).beatCounter := beatCounter
    pendingResponses(tailPtr).totalBeats := totalBeats
  }

  def enqueueMultipleBeats(
    message: UInt,
    sourceID: UInt,
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
          dueTime = baseDelay + beatIdx.U,
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

  def printResponseLog(cycle_cnt: UInt, resp: PendingResponse, responseData: UInt): Unit = {
    when (resp.message === TLMessages.AccessAck) {
      printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Response sent:" +
          cf"  Response Message: ${resp.message} (Ack)" +
          cf"  Source ID: ${resp.sourceID}" +
          cf"  Size: ${resp.size}\n")
    }.otherwise {
      printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Response sent:" +
          cf"  Response Message: ${resp.message} (AckData)" +
          cf"  Beat: ${resp.beatCounter}/${resp.totalBeats}" +
          cf"  Response Data: 0x${responseData}%x" +
          cf"  Source ID: ${resp.sourceID}" +
          cf"  Size: ${resp.size}\n")
    }
  }

  def updateQueuePointers(new_head_ptr: UInt, new_tail_ptr: UInt, cnt_inc: UInt, cnt_dec: UInt): Unit = {
    val pendingCountNew = pendingCount + cnt_inc - cnt_dec
    pendingHead := new_head_ptr
    pendingTail := new_tail_ptr
    pendingCount := pendingCountNew
    when (pendingCount === 0.U) {
      printf(cf"MMUSM: [Cycle ${timer}] Pending Count: ${pendingCountNew}, Pending Head: ${new_head_ptr}, Pending Tail: ${new_tail_ptr}\n")
    }
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
        // MMU monitoring logic
        top.tl.foreach { tl =>
          tl.a.ready := (pendingCount < (pendingSize - 8).U)
          val last = top.edgeIn.last(tl.a)
          val countInc = WireInit(0.U(4.W))
          val countDec = WireInit(0.U(4.W))
          val newHeadPtr = WireInit(pendingHead)
          val newTailPtr = WireInit(pendingTail)

          // Check MMU requests
          when(tl.a.fire) {
            // Default response info for Put request.
            val respMsg = WireInit(TLMessages.AccessAck)
            val respBeats = WireInit(1.U(4.W))
            val randDelay = timer + randLFSR(7, 0)

            val physAddr = tl.a.bits.address
            val requestData = tl.a.bits.data
            val sourceID = tl.a.bits.source
            val size = tl.a.bits.size
            val beatBytes = tl.a.bits.data.getWidth / 8
            val opcode = tl.a.bits.opcode
            val param = tl.a.bits.param

            printf(cf"MMUSM: [Cycle ${cycle_cnt}] MMU Request detected:" +
                cf"  Physical Address: 0x${physAddr}%x" +
                cf"  opcode ${opcode} param ${param}" +
                cf"  Source ID: ${sourceID}" +
                cf"  Size: ${size}" +
                cf"  Beat Bytes: ${beatBytes}\n" +
                cf"  Data: 0x${requestData}%x\n")

            when (opcode === TLMessages.Get) {
              respMsg := TLMessages.AccessAckData
              val dummy_d = top.edgeIn.AccessAck(tl.a.bits, 0.U)
              respBeats := top.edgeIn.numBeats(dummy_d)
              assert(respBeats <= 8.U, "Unsupported number of beats > 8")
              assert(pendingCount + respBeats <= pendingSize.U, "Pending queue overflow")
            }

            when (last) {
              enqueueMultipleBeats(
                message = respMsg,
                sourceID = sourceID,
                size = size,
                baseDelay = randDelay,
                numBeats = respBeats,
                baseTailPtr = pendingTail
              )
              newTailPtr := (pendingTail + respBeats) % pendingSize.U
              countInc := respBeats
            }
          }

          // Handle pending requests
          when(pendingCount > 0.U) {
            val resp = dequeuePendingResponse()

            tl.d.valid := resp.dueTime <= timer
            tl.d.bits.size := resp.size
            tl.d.bits.opcode := resp.message
            tl.d.bits.source := resp.sourceID
            
            val responseData = generateResponseData(resp)
            tl.d.bits.data := responseData

            when (tl.d.fire) {
              newHeadPtr := (pendingHead + 1.U) % pendingSize.U
              countDec := 1.U
              printResponseLog(cycle_cnt, resp, responseData)
            }
          }

          updateQueuePointers(newHeadPtr, newTailPtr, countInc, countDec)
        }
      }
      
      is(MMUState.sDone) {
        // Monitoring complete, maintain state
      }
    }
  }
}

// Observation state machine class
class ObserveStateMachine(top: XSCuteTopImpl)(implicit p: Parameters) {
  // State definitions
  object ObserveState extends ChiselEnum {
    val sIdle, sObserving, sDone = Value
  }
  
  // State registers
  val state = RegInit(ObserveState.sIdle)
  val observe_cycles = RegInit(0.U(8.W))
  
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
        when(false.B) { // TODO: set the condition for finishing observation
          state := ObserveState.sDone
          printf("ObserveSM: Final observation completed at cycle %d\n", cycle_cnt)
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
  })

  val top = Module(LazyModule(new XSCuteTop).module)

  top.io := DontCare
  top.tl(0) := DontCare
  
  // Instantiate state machine objects
  val configSM = new ConfigStateMachine(top)
  val mmuSM = new MMUMonitorStateMachine(top)
  val observeSM = new ObserveStateMachine(top)
  
  // Main control state machine definition
  object MainState extends ChiselEnum {
    val sIdle, sConfigPhase, sMonitorPhase, sSuccess = Value
  }
  
  val main_state = RegInit(MainState.sIdle)
  val cycle_cnt = RegInit(0.U(32.W))
  
  // TestHarness default assignments (before state machine logic)
  io.success := false.B
  // main_state maintains current value, no default assignment needed (initialized via RegInit)
  
  // Apply default values for state machine signals
  StateMachineDefaults.applyDefaults(top)
  
  // Global cycle counter
  cycle_cnt := cycle_cnt + 1.U
  
  // Update all state machines
  configSM.update(cycle_cnt)
  mmuSM.update(cycle_cnt)
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
        mmuSM.start()
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