package xiangshan.backend.fu.NewCSR

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CSRs
import utility.GatedValidRegNext
import xiangshan.backend.fu.NewCSR.CSRDefines.{CSRROField => RO, CSRRWField => RW, CSRWARLField => WARL}
import xiangshan.backend.fu.NewCSR.CSRFunc._
import xiangshan.backend.fu.matrix.Bundles._
import xiangshan.backend.fu.vector.Bundles._
import xiangshan.backend.fu.NewCSR.CSRConfig._
import xiangshan.backend.fu.fpu.Bundles.{Fflags, Frm}
import xiangshan.backend.fu.NewCSR.CSREnumTypeImplicitCast._
import xiangshan.XSCoreParameters

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

  // Matrix CSR read-only fields: enum reset literals follow p(XSCoreParamsKey) (same as HasXSParameter.coreParams).
  private val matrixCp: XSCoreParameters = self.coreParams

  object TlenbField extends CSREnum with ROApply {
    val init = Value((matrixCp.TLEN / 8).U)
  }

  object TrlenbField extends CSREnum with ROApply {
    val init = Value((matrixCp.TRLEN / 8).U)
  }

  object AlenbField extends CSREnum with ROApply {
    val init = Value(
      (((matrixCp.TLEN / matrixCp.TRLEN) * (matrixCp.TLEN / matrixCp.TRLEN) * matrixCp.MELEN) / 8).U
    )
  }

  object MsyncField extends CSREnum with ROApply {
    val init = Value(matrixCp.MsyncRegs.U)
  }

  val fcsr = Module(new CSRModule("Fcsr", new CSRBundle {
    val NX = WARL(0, wNoFilter)
    val UF = WARL(1, wNoFilter)
    val OF = WARL(2, wNoFilter)
    val DZ = WARL(3, wNoFilter)
    val NV = WARL(4, wNoFilter)
    val FRM = WARL(7, 5, wNoFilter).withReset(0.U)
  }) with HasRobCommitBundle {
    val wAliasFflags = IO(Input(new CSRAddrWriteBundle(new CSRFFlagsBundle)))
    val wAliasFfm = IO(Input(new CSRAddrWriteBundle(new CSRFrmBundle)))
    val fflags = IO(Output(Fflags()))
    val frm = IO(Output(Frm()))
    val fflagsRdata = IO(Output(Fflags()))
    val frmRdata = IO(Output(Frm()))

    for (wAlias <- Seq(wAliasFflags, wAliasFfm)) {
      for ((name, field) <- wAlias.wdataFields.elements) {
        reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
          wAlias.wen && field.asInstanceOf[CSREnumType].isLegal,
          field.asInstanceOf[CSREnumType]
        )
      }
    }

    // write connection
    reconnectReg()

    when (robCommit.fflags.valid) {
      reg.NX := robCommit.fflags.bits(0) || reg.NX
      reg.UF := robCommit.fflags.bits(1) || reg.UF
      reg.OF := robCommit.fflags.bits(2) || reg.OF
      reg.DZ := robCommit.fflags.bits(3) || reg.DZ
      reg.NV := robCommit.fflags.bits(4) || reg.NV
    }

    // read connection
    fflags := reg.asUInt(4, 0)
    frm := reg.FRM.asUInt

    fflagsRdata := fflags.asUInt
    frmRdata := frm.asUInt
  }).setAddr(CSRs.fcsr)

  // vec
  val vstart = Module(new CSRModule("Vstart", new CSRBundle {
    // vstart is not a WARL CSR.
    // Since we need to judge whether flush pipe by vstart being not 0 in DecodeStage, vstart must be initialized to some value at reset.
    val vstart = RW(VlWidth - 2, 0).withReset(0.U) // hold [0, 128)
  }) with HasRobCommitBundle {
    // Todo make The use of vstart values greater than the largest element index for the current SEW setting is reserved.
    // Not trap
    when (wen) {
      reg.vstart := this.w.wdata(VlWidth - 2, 0)
    }.elsewhen (robCommit.vsDirty && !robCommit.vstart.valid) {
      reg.vstart := 0.U
    }.elsewhen (robCommit.vstart.valid) {
      reg.vstart := robCommit.vstart.bits
    }.otherwise {
      reg := reg
    }
  })
    .setAddr(CSRs.vstart)

  val vcsr = Module(new CSRModule("Vcsr", new CSRBundle {
    val VXSAT = RW(   0)
    val VXRM  = RW(2, 1)
  }) with HasRobCommitBundle {
    val wAliasVxsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXSAT = RW(0)
    })))
    val wAliasVxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val VXRM = RW(1, 0)
    })))
    val vxsat = IO(Output(Vxsat()))
    val vxrm  = IO(Output(Vxrm()))

    for (wAlias <- Seq(wAliasVxsat, wAliasVxrm)) {
      for ((name, field) <- wAlias.wdataFields.elements) {
        reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
          wAlias.wen && field.asInstanceOf[CSREnumType].isLegal,
          field.asInstanceOf[CSREnumType]
        )
      }
    }

    // write connection
    reconnectReg()

    when(robCommit.vxsat.valid) {
      reg.VXSAT := reg.VXSAT.asBool || robCommit.vxsat.bits.asBool
    }

    // read connection
    vxsat := reg.VXSAT.asUInt
    vxrm  := reg.VXRM.asUInt
  }).setAddr(CSRs.vcsr)

  val vl = Module(new CSRModule("Vl", new CSRBundle {
    val VL = RO(VlWidth - 1, 0).withReset(0.U)
  }))
    .setAddr(CSRs.vl)

  val vtype = Module(new CSRModule("Vtype", new CSRVTypeBundle) with HasRobCommitBundle {
    when(robCommit.vtype.valid) {
      reg := robCommit.vtype.bits
    }
  })
    .setAddr(CSRs.vtype)

  val vlenb = Module(new CSRModule("Vlenb", new CSRBundle {
    val VLENB = VlenbField(63, 0).withReset(VlenbField.init)
  }))
    .setAddr(CSRs.vlenb)

  val mcsr = Module(new CSRModule("Mcsr", new CSRBundle {
    val MXRM    = RW(1,  0).withReset(0.U)
    val MSAT    = RW(    2).withReset(0.U)
    val MFFLAGS = RW(7,  3).withReset(0.U)
    val MFRM    = RW(10, 8).withReset(0.U)
    val MSATEN  = RW(11).withReset(0.U)
  }) with HasRobCommitBundle {
    val wAliasMxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val MXRM = RW(1, 0)
    })))
    val wAliasMsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val MSAT = RW(0)
    })))
    val wAliasMflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val MFFLAGS = RW(4, 0)
    })))
    val wAliasMfrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val MFRM = RW(2, 0)
    })))
    val wAliasMsaten = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val MSATEN = RW(0)
    })))
    val mxrm = IO(Output(Mxrm()))
    val msat = IO(Output(Msat()))
    val mfflags = IO(Output(Mfflags()))
    val mfrm = IO(Output(Mfrm()))
    val msaten = IO(Output(Msaten()))

    for (wAlias <- Seq(wAliasMxrm, wAliasMsat, wAliasMflags, wAliasMfrm, wAliasMsaten)) {
      for ((name, field) <- wAlias.wdataFields.elements) {
        reg.elements(name).asInstanceOf[CSREnumType].addOtherUpdate(
          wAlias.wen && field.asInstanceOf[CSREnumType].isLegal,
          field.asInstanceOf[CSREnumType]
        )
      }
    }

    // write connection
    reconnectReg()

    // when(amu.xmsat.valid) {
    //   reg.XMSAT := reg.XMSAT.asBool || robCommit.xmsat.bits.asBool
    // }
    // when(amu.xmfflags.valid) {
    //   reg.XMFFLAGS := reg.XMFFLAGS.asUInt | amu.xmfflags.bits.asUInt
    // }
    // TODO: How to update xmsat and xmfflags from CUTE?

    // read connection
    mxrm := reg.MXRM.asUInt
    msat := reg.MSAT.asUInt
    mfflags := reg.MFFLAGS.asUInt
    mfrm := reg.MFRM.asUInt
    msaten := reg.MSATEN.asUInt
  }).setAddr(CSRs.mcsr)

  // Matrix tile size registers, read-only.
  // They can be updated only by msettilem/n/k instructions.
  val mtilem = Module(new CSRModule("Mtilem", new CSRBundle {
    val MTILEM = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilem)

  val mtilen = Module(new CSRModule("Mtilen", new CSRBundle {
    val MTILEN = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilen)

  val mtilek = Module(new CSRModule("Mtilek", new CSRBundle {
    val MTILEK = RO(63, 0).withReset(0.U)
  }))
    .setAddr(CSRs.mtilek)

  val tlenb = Module(new CSRModule("Tlenb", new CSRBundle {
    val TLENB = TlenbField(63, 0).withReset(TlenbField.init)
  }))
    .setAddr(CSRs.tlenb)

  val trlenb = Module(new CSRModule("Trlenb", new CSRBundle {
    val TRLENB = TrlenbField(63, 0).withReset(TrlenbField.init)
  }))
    .setAddr(CSRs.trlenb)

  val alenb = Module(new CSRModule("Alenb", new CSRBundle {
    val ALENB = AlenbField(63, 0).withReset(AlenbField.init)
  }))
    .setAddr(CSRs.alenb)

  val msync = Module(new CSRModule("Msync", new CSRBundle {
    val MSYNC = MsyncField(63, 0).withReset(MsyncField.init)
  }))
    .setAddr(CSRs.msync)

  val cycle = Module(new CSRModule("cycle", new CSRBundle {
    val cycle = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    when(unprivCountUpdate) {
      reg := mHPM.cycle
    }.otherwise{
      reg := reg
    }
    regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.cycle)
  })
    .setAddr(CSRs.cycle)

  val time = Module(new CSRModule("time", new CSRBundle {
    val time = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    val updated = IO(Output(Bool()))
    val stime  = IO(Output(UInt(64.W)))
    val vstime = IO(Output(UInt(64.W)))

    val stimeTmp  = mHPM.time.bits
    val vstimeTmp = mHPM.time.bits + htimedelta

    // Update when rtc clock tick and not dcsr.STOPTIME
    // or virtual mode changed
    // Note: we delay a cycle and use `v` for better timing
    val virtModeChanged = RegNext(nextV =/= v, false.B)
    when(mHPM.time.valid && !debugModeStopTime || virtModeChanged) {
      reg.time := Mux(v, vstimeTmp, stimeTmp)
    }.otherwise {
      reg := reg
    }

    updated := GatedValidRegNext(mHPM.time.valid && !debugModeStopTime)
    stime  := stimeTmp
    vstime := vstimeTmp
  })
    .setAddr(CSRs.time)

  val instret = Module(new CSRModule("instret", new CSRBundle {
    val instret = RO(63, 0)
  }) with HasMHPMSink with HasDebugStopBundle {
    when(unprivCountUpdate) {
      reg := mHPM.instret
    }.otherwise{
      reg := reg
    }
    regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.instret)
  })
    .setAddr(CSRs.instret)

  val hpmcounters: Seq[CSRModule[_]] = (3 to 0x1F).map(num =>
    Module(new CSRModule(s"Hpmcounter$num", new CSRBundle {
      val hpmcounter = RO(63, 0).withReset(0.U)
    }) with HasMHPMSink with HasDebugStopBundle {
      when(unprivCountUpdate) {
        reg := mHPM.hpmcounters(num - 3)
      }.otherwise{
        reg := reg
      }
      regOut := Mux(debugModeStopCount, reg.asUInt, mHPM.hpmcounters(num - 3))
    }).setAddr(CSRs.cycle + num)
  )

  val unprivilegedCSRMap: SeqMap[Int, (CSRAddrWriteBundle[_], UInt)] = SeqMap(
    CSRs.fflags   -> (fcsr.wAliasFflags -> fcsr.fflagsRdata),
    CSRs.frm      -> (fcsr.wAliasFfm    -> fcsr.frmRdata),
    CSRs.fcsr     -> (fcsr.w            -> fcsr.rdata),
    CSRs.vstart   -> (vstart.w          -> vstart.rdata),
    CSRs.vxsat    -> (vcsr.wAliasVxsat  -> vcsr.vxsat),
    CSRs.vxrm     -> (vcsr.wAliasVxrm   -> vcsr.vxrm),
    CSRs.vcsr     -> (vcsr.w            -> vcsr.rdata),
    CSRs.vl       -> (vl.w              -> vl.rdata),
    CSRs.vtype    -> (vtype.w           -> vtype.rdata),
    CSRs.vlenb    -> (vlenb.w           -> vlenb.rdata),
    CSRs.mcsr     -> (mcsr.w            -> mcsr.rdata),
    CSRs.mxrm     -> (mcsr.wAliasMxrm   -> mcsr.mxrm),
    CSRs.msat     -> (mcsr.wAliasMsat   -> mcsr.msat),
    CSRs.mfflags  -> (mcsr.wAliasMflags -> mcsr.mfflags),
    CSRs.mfrm     -> (mcsr.wAliasMfrm   -> mcsr.mfrm),
    CSRs.msaten   -> (mcsr.wAliasMsaten -> mcsr.msaten),
    CSRs.mtilem   -> (mtilem.w          -> mtilem.rdata),
    CSRs.mtilen   -> (mtilen.w          -> mtilen.rdata),
    CSRs.mtilek   -> (mtilek.w          -> mtilek.rdata),
    CSRs.tlenb    -> (tlenb.w           -> tlenb.rdata),
    CSRs.trlenb   -> (trlenb.w          -> trlenb.rdata),
    CSRs.alenb    -> (alenb.w           -> alenb.rdata),
    CSRs.msync    -> (msync.w           -> msync.rdata),
    CSRs.cycle    -> (cycle.w           -> cycle.rdata),
    CSRs.time     -> (time.w            -> time.rdata),
    CSRs.instret  -> (instret.w         -> instret.rdata),
  ) ++ hpmcounters.map(counter => (counter.addr -> (counter.w -> counter.rdata)))

  val unprivilegedCSRMods: Seq[CSRModule[_]] = Seq(
    fcsr,
    vcsr,
    vstart,
    vl,
    vtype,
    vlenb,
    mcsr,
    tlenb,
    trlenb,
    alenb,
    mtilem,
    mtilen,
    mtilek,
    msync,
    cycle,
    time,
    instret,
  ) ++ hpmcounters

  val unprivilegedCSROutMap: SeqMap[Int, UInt] = SeqMap(
    CSRs.fflags  -> fcsr.fflags.asUInt,
    CSRs.frm     -> fcsr.frm.asUInt,
    CSRs.fcsr    -> fcsr.rdata.asUInt,
    CSRs.vstart  -> vstart.rdata.asUInt,
    CSRs.vxsat   -> vcsr.vxsat.asUInt,
    CSRs.vxrm    -> vcsr.vxrm.asUInt,
    CSRs.vcsr    -> vcsr.rdata.asUInt,
    CSRs.vl      -> vl.rdata.asUInt,
    CSRs.vtype   -> vtype.rdata.asUInt,
    CSRs.vlenb   -> vlenb.rdata.asUInt,
    CSRs.mcsr    -> mcsr.rdata.asUInt,
    CSRs.mxrm    -> mcsr.mxrm.asUInt,
    CSRs.msat    -> mcsr.msat.asUInt,
    CSRs.mfflags -> mcsr.mfflags.asUInt,
    CSRs.mfrm    -> mcsr.mfrm.asUInt,
    CSRs.msaten  -> mcsr.msaten.asUInt,
    CSRs.tlenb   -> tlenb.rdata.asUInt,
    CSRs.trlenb  -> trlenb.rdata.asUInt,
    CSRs.alenb   -> alenb.rdata.asUInt,
    CSRs.mtilem  -> mtilem.rdata.asUInt,
    CSRs.mtilen  -> mtilen.rdata.asUInt,
    CSRs.mtilek  -> mtilek.rdata.asUInt,
    CSRs.msync   -> msync.rdata.asUInt,
    CSRs.cycle   -> cycle.rdata,
    CSRs.time    -> time.rdata,
    CSRs.instret -> instret.rdata,
  ) ++ hpmcounters.map(counter => (counter.addr -> counter.rdata))
}

class CSRVTypeBundle extends CSRBundle {
  // vtype's vill is initialized to 1, when executing vector instructions
  // which depend on vtype, will raise illegal instruction exception
  val VILL  = RO(  63).withReset(1.U)
  val VMA   = RO(   7).withReset(0.U)
  val VTA   = RO(   6).withReset(0.U)
  val VSEW  = RO(5, 3).withReset(0.U)
  val VLMUL = RO(2, 0).withReset(0.U)
}

class CSRFrmBundle extends CSRBundle {
  val FRM = WARL(2, 0, wNoFilter)
}

class CSRFFlagsBundle extends CSRBundle {
  val NX = WARL(0, wNoFilter)
  val UF = WARL(1, wNoFilter)
  val OF = WARL(2, wNoFilter)
  val DZ = WARL(3, wNoFilter)
  val NV = WARL(4, wNoFilter)
}

object VlenbField extends CSREnum with ROApply {
  val init = Value((VLEN / 8).U)
}

trait HasMHPMSink { self: CSRModule[_] =>
  val mHPM = IO(Input(new Bundle {
    val cycle   = UInt(64.W)
    // ValidIO is used to update time reg
    val time    = ValidIO(UInt(64.W))
    val instret = UInt(64.W)
    val hpmcounters = Vec(perfCntNum, UInt(XLEN.W))
  }))
  val v = IO(Input(Bool()))
  val nextV = IO(Input(Bool()))
  val htimedelta = IO(Input(UInt(64.W)))
}

trait HasDebugStopBundle { self: CSRModule[_] =>
  val debugModeStopCount = IO(Input(Bool()))
  val debugModeStopTime  = IO(Input(Bool()))
  val unprivCountUpdate  = IO(Input(Bool()))
}
