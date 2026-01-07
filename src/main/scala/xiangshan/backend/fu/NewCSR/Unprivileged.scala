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

import scala.collection.immutable.SeqMap

trait Unprivileged { self: NewCSR with MachineLevel with SupervisorLevel =>

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

  val xmcsr = Module(new CSRModule("Xmcsr", new CSRBundle {
    val XMXRM    = RW(1,  0)
    val XMSAT    = RW(    2)
    val XMFFLAGS = RW(7,  3)
    val XMFRM    = RW(10, 8)
    val XMSATEN  = RW(11)
    val VXSAT = RW(   0)
    val VXRM  = RW(2, 1)
  }) with HasRobCommitBundle {
    val wAliasXmxrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val XMXRM = RW(1, 0)
    })))
    val wAliasXmsat = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val XMSAT = RW(2)
    })))
    val wAliasXmflags = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val XMFFLAGS = RW(7, 3)
    })))
    val wAliasXmfrm = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val XMFRM = RW(10, 8)
    })))
    val wAliasXmsaten = IO(Input(new CSRAddrWriteBundle(new CSRBundle {
      val XMSATEN = RW(11)
    })))
    val xmxrm = IO(Output(Xmxrm()))
    val xmsat = IO(Output(Xmsat()))
    val xmfflags = IO(Output(Xmfflags()))
    val xmfrm = IO(Output(Xmfrm()))
    val xmsaten = IO(Output(Xmsaten()))

    for (wAlias <- Seq(wAliasXmxrm, wAliasXmsat, wAliasXmflags, wAliasXmfrm, wAliasXmsaten)) {
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
    xmxrm := reg.XMXRM.asUInt
    xmsat := reg.XMSAT.asUInt
    xmfflags := reg.XMFFLAGS.asUInt
    xmfrm := reg.XMFRM.asUInt
    xmsaten := reg.XMSATEN.asUInt
  }).setAddr(CSRs.xmcsr)

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

  val xmisa = Module(new CSRModule("Xmisa", new CSRBundle {
    val XMISA = XmisaField(63, 0).withReset(XmisaField.init)
  }))
    .setAddr(CSRs.xmisa)

  val xtlenb = Module(new CSRModule("Xtlenb", new CSRBundle {
    val XTLENB = XtlenbField(63, 0).withReset(XtlenbField.init)
  }))
    .setAddr(CSRs.xtlenb)

  val xtrlenb = Module(new CSRModule("Xtrlenb", new CSRBundle {
    val XTRLENB = XtrlenbField(63, 0).withReset(XtrlenbField.init)
  }))
    .setAddr(CSRs.xtrlenb)

  val xalenb = Module(new CSRModule("Xalenb", new CSRBundle {
    val XALENB = XalenbField(63, 0).withReset(XalenbField.init)
  }))
    .setAddr(CSRs.xalenb)

  val mtok = Module(new CSRModule("Mtok", new CSRBundle {
    val MTOK = MtokField(63, 0).withReset(MtokField.init)
  }))
    .setAddr(CSRs.mtok)

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
    CSRs.xmcsr    -> (xmcsr.w           -> xmcsr.rdata),
    CSRs.xmxrm    -> (xmcsr.wAliasXmxrm   -> xmcsr.xmxrm),
    CSRs.xmsat    -> (xmcsr.wAliasXmsat   -> xmcsr.xmsat),
    CSRs.xmfflags -> (xmcsr.wAliasXmflags -> xmcsr.xmfflags),
    CSRs.xmfrm    -> (xmcsr.wAliasXmfrm   -> xmcsr.xmfrm),
    CSRs.xmsaten  -> (xmcsr.wAliasXmsaten -> xmcsr.xmsaten),
    CSRs.mtilem   -> (mtilem.w          -> mtilem.rdata),
    CSRs.mtilen   -> (mtilen.w          -> mtilen.rdata),
    CSRs.mtilek   -> (mtilek.w          -> mtilek.rdata),
    CSRs.xmisa    -> (xmisa.w           -> xmisa.rdata),
    CSRs.xtlenb   -> (xtlenb.w          -> xtlenb.rdata),
    CSRs.xtrlenb  -> (xtrlenb.w         -> xtrlenb.rdata),
    CSRs.xalenb   -> (xalenb.w          -> xalenb.rdata),
    CSRs.mtok     -> (mtok.w            -> mtok.rdata),
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
    xmcsr,
    xmisa,
    xtlenb,
    xtrlenb,
    xalenb,
    mtok,
    mtilem,
    mtilen,
    mtilek,
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
    CSRs.xmcsr   -> xmcsr.rdata.asUInt,
    CSRs.xmxrm   -> xmcsr.xmxrm.asUInt,
    CSRs.xmsat   -> xmcsr.xmsat.asUInt,
    CSRs.xmfflags -> xmcsr.xmfflags.asUInt,
    CSRs.xmfrm   -> xmcsr.xmfrm.asUInt,
    CSRs.xmsaten -> xmcsr.xmsaten.asUInt,
    CSRs.xmisa   -> xmisa.rdata.asUInt,
    CSRs.xtlenb  -> xtlenb.rdata.asUInt,
    CSRs.xtrlenb -> xtrlenb.rdata.asUInt,
    CSRs.xalenb  -> xalenb.rdata.asUInt,
    CSRs.mtok    -> mtok.rdata.asUInt,
    CSRs.mtilem  -> mtilem.rdata.asUInt,
    CSRs.mtilen  -> mtilen.rdata.asUInt,
    CSRs.mtilek  -> mtilek.rdata.asUInt,
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

object XmisaField extends CSREnum with ROApply {
  val init = Value(0x2e6.U)
}

object XtlenbField extends CSREnum with ROApply {
  val init = Value((TLEN / 8).U)
}

object XtrlenbField extends CSREnum with ROApply {
  val init = Value((TRLEN / 8).U)
}

object XalenbField extends CSREnum with ROApply {
  val init = Value(((TLEN / TRLEN) * (TLEN / TRLEN) * MELEN / 8).U)
}

object MtokField extends CSREnum with ROApply {
  val init = Value(MTOK.U)
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