package xiangshan.backend.fu.matrix

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.XSBundle
import xiangshan.XSCoreParamsKey
import xiangshan.backend.decode.isa.bitfield.InstMType
import utility.ZeroExt
import _root_.utils.NamedUInt
import freechips.rocketchip.tile.XLen

object Bundles {
  object MSew extends NamedUInt(3) {
    def e8  : UInt = "b000".U(width.W)
    def e16 : UInt = "b001".U(width.W)
    def e32 : UInt = "b010".U(width.W)
    def e64 : UInt = "b011".U(width.W)
    def e4  : UInt = "b111".U(width.W)

    def reserved = Seq(BitPat("b100"), BitPat("b101"), BitPat("b110"))

    def isReserved(sew: UInt) : Bool = {
      require(sew.getWidth >= 2 && sew.getWidth <= 3)
      if (sew.getWidth == 3) {
        reserved.map(sew === _).reduce(_ || _)
      } else {
        false.B
      }
    }
  }

  object MSewOH extends NamedUInt(8) {
    def e8  : UInt = "b00000001".U(width.W)
    def e16 : UInt = "b00000010".U(width.W)
    def e32 : UInt = "b00000100".U(width.W)
    def e64 : UInt = "b00001000".U(width.W)
    def e4  : UInt = "b10000000".U(width.W)

    def convertFromMSew(msew: UInt): UInt = {
      require(msew.getWidth >= 2 && msew.getWidth <= 3)
      ZeroExt(UIntToOH(msew), this.width)
    }
  }

  object MtypeMSew extends NamedUInt(3)

  object Xmxrm extends NamedUInt(2)

  object Xmsat extends NamedUInt(1)

  object Xmfflags extends NamedUInt(5)

  object Xmfrm extends NamedUInt(3)

  object Xmsaten extends NamedUInt(1)

  object Mtilex {
    def apply()(implicit p: Parameters): UInt = UInt(width.W)

    // TODO: use a correct width
    // The mlwidth is just a placeholder
    def width(implicit p: Parameters) = p(XSCoreParamsKey).mlWidth
  }

  // class AmuMmaIO(implicit p: Parameters) extends XSBundle {
  //   val md       = UInt(4.W) // 3 : 0
  //   val sat      = Bool()    // 4
  //   val ms1      = UInt(4.W) // 8 : 5
  //   val ms2      = UInt(4.W) // 12 : 9
  //   val mtilem   = Mtilex()  // 21 : 13
  //   val mtilen   = Mtilex()  // 30 : 22
  //   val mtilek   = Mtilex()  // 39 : 31
  //   val types    = UInt(3.W) // 42 : 40
  //   val typed    = UInt(3.W) // 45 : 43
  //   val isfp     = Bool()    // 46
  //   val issigned = Bool()  // 47
  // }

  class AmuMmaIO(implicit p: Parameters) extends XSBundle {
    val rm       = UInt(3.W) // 52 : 50
    val md       = UInt(4.W) // 49 : 46
    val sat      = Bool()    // 45
    val ms1      = UInt(4.W) // 44 : 41
    val ms2      = UInt(4.W) // 40 : 37
    val mtilem   = Mtilex()  // 36 : 28
    val mtilen   = Mtilex()  // 27 : 19
    val mtilek   = Mtilex()  // 18 : 10
    val types2   = UInt(3.W) // 9 : 8
    val types1   = UInt(3.W) // 6 : 4
    val typed    = UInt(3.W) // 3 : 1
    val isfp     = Bool()    // 0
  }

  object AmuMmaIO {
    def apply()(implicit p: Parameters) : AmuMmaIO = {
      new AmuMmaIO()
    }
  }

  class AmuLsuIO(implicit p: Parameters) extends XSBundle {
    // src/dest matrix register
    val ms        = UInt(4.W)         // 125 : 122
    // load(0)/store(1)
    val ls        = Bool()            // 121
    // whether transposed
    val transpose = Bool()            // 120
    // whether accumulation register
    val isacc     = Bool()            // 119
    val isA       = Bool()            // 118
    val isB       = Bool()            // 117

    val baseAddr  = UInt(48.W)        // 116 : 69
    val stride    = UInt(48.W)        // 68 : 21
    
    val row       = Mtilex()          // 20 : 12
    val column    = Mtilex()          // 11 : 3
    val widths    = MtypeMSew()       // 2 : 0
  }

  object AmuLsuIO {
    def apply()(implicit p: Parameters) : AmuLsuIO = {
      new AmuLsuIO()
    }
  }

  class AmuArithIO(implicit p: Parameters) extends XSBundle {
    val md     = UInt(4.W) // 12 : 9
    val opType = UInt(9.W) // 8 : 0
  }

  object AmuArithIO {
    def apply()(implicit p: Parameters) : AmuArithIO = {
      new AmuArithIO()
    }
  }

  class AmuReleaseIO2CUTE(implicit p: Parameters) extends XSBundle {
    val tokenRd = UInt(p(XSCoreParamsKey).TokenRegs.W)
  }

  object AmuReleaseIO2CUTE {
    def apply()(implicit p: Parameters) : AmuReleaseIO2CUTE = {
      new AmuReleaseIO2CUTE()
    }
  }

  class AmuReleaseIO2XS(implicit p: Parameters) extends XSBundle {
    val tokenRd = Vec(p(XSCoreParamsKey).TokenRegs, Bool())
  }

  object AmuReleaseIO2XS {
    def apply()(implicit p: Parameters) : AmuReleaseIO2XS = {
      new AmuReleaseIO2XS()
    }
  }

  class AmuCtrlIO(implicit p: Parameters) extends XSBundle {
    // op: Determine the operation
    // 0: MMA
    // 1: Load/Store
    // 2: Release
    // 3: Arith
    val op = UInt(2.W)
    
    def isMma()     : Bool = op === AmuCtrlIO.mmaOp()
    def isMls()     : Bool = op === AmuCtrlIO.mlsOp()
    def isRelease() : Bool = op === AmuCtrlIO.releaseOp()
    def isArith()   : Bool = op === AmuCtrlIO.arithOp()
    // data: The ctrl signal for op
    val data = UInt(150.W)
  }

  object AmuCtrlIO {
    def apply()(implicit p: Parameters) : AmuCtrlIO = {
      new AmuCtrlIO()
    }

    def mmaOp()     : UInt = "b00".U
    def mlsOp()     : UInt = "b01".U
    def releaseOp() : UInt = "b10".U
    def arithOp()   : UInt = "b11".U
  }
}
