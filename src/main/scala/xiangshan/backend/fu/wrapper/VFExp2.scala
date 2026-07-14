/***************************************************************************************
 * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
 * Copyright (c) 2020-2021 Peng Cheng Laboratory
 *
 * XiangShan is licensed under Mulan PSL v2.
 * You can use this software according to the terms and conditions of the Mulan PSL v2.
 * You may obtain a copy of Mulan PSL v2 at:
 *          http://license.coscl.org.cn/MulanPSL2
 *
 * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
 * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
 * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
 *
 * See the Mulan PSL v2 for more details.
 ***************************************************************************************/

package xiangshan.backend.fu.wrapper

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode._
import utility.XSError
import xiangshan.ExceptionNO
import xiangshan.backend.fu.FuConfig
import xiangshan.backend.fu.vector.utils.VecDataSplitModule
import xiangshan.backend.fu.vector.{Mgu, VecPipedFuncUnit}
import yunsuan.VfexpType
import yunsuan.vector.{VfExp2 => YunSuanVfExp2}

class VFExp2(cfg: FuConfig)(implicit p: Parameters) extends VecPipedFuncUnit(cfg) {
  private val dataWidth = cfg.destDataBits
  private val dataWidthOfDataModule = 64
  private val numVecModule = dataWidth / dataWidthOfDataModule

  private val opcode = fuOpType(7, 0)
  private val isFpVfexp2 = opcode === VfexpType.vfexp2
  private val isBf16Vfexp2 = opcode === VfexpType.vfexp2bf16
  private val isVfexp2 = isFpVfexp2 || isBf16Vfexp2

  XSError(io.in.valid && !isVfexp2, "Vfexp2 OpType not supported")

  private val vfexp2s = Seq.fill(numVecModule)(Module(new YunSuanVfExp2(dataWidthOfDataModule)))
  private val vs2Split = Module(new VecDataSplitModule(dataWidth, dataWidthOfDataModule))
  private val mgu = Module(new Mgu(dataWidth))

  vs2Split.io.inVecData := vs2

  private val resultData = Wire(Vec(numVecModule, UInt(dataWidthOfDataModule.W)))
  private val fflagsData = Wire(Vec(numVecModule, UInt(20.W)))

  vfexp2s.zipWithIndex.foreach {
    case (mod, i) =>
      mod.io.fire := io.in.valid
      mod.io.src := vs2Split.io.outVec64b(i)
      mod.io.opType := opcode
      mod.io.sew := vsew
      mod.io.rm := rm
      resultData(i) := mod.io.result
      fflagsData(i) := mod.io.fflags
  }

  private val eNum1H = chisel3.util.experimental.decode.decoder(
    outVecCtrl.vsew,
    TruthTable(
      Seq(
        BitPat("b01") -> BitPat("b1000"),
        BitPat("b10") -> BitPat("b0100")
      ),
      BitPat.N(4)
    )
  )
  private val eNumMax1H =
    Mux(
      outVecCtrl.vlmul.head(1).asBool,
      eNum1H >> ((~outVecCtrl.vlmul.tail(1)).asUInt + 1.U),
      eNum1H << outVecCtrl.vlmul.tail(1)
    ).asUInt(6, 0)
  private val eNumMax = Mux1H(eNumMax1H, Seq(1, 2, 4, 8, 16, 32, 64).map(_.U))
  private val eNumEffectIdx = Mux(outVl > eNumMax, eNumMax, outVl)
  private val eNum = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(_.U))
  private val eStart = outVecCtrl.vuopIdx * eNum
  private val maskPart = outSrcMask >> eStart
  private val mask = Mux1H(eNum1H, Seq(1, 2, 4, 8).map(num => maskPart(num - 1, 0)))
  private val fflagsEn = Wire(Vec(4 * numVecModule, Bool()))
  fflagsEn := mask.asBools.zipWithIndex.map {
    case (maskBit, i) => maskBit && (eNumEffectIdx > eStart + i.U)
  }

  private val fflagsAll = Wire(Vec(4 * numVecModule, UInt(5.W)))
  fflagsAll := fflagsData.asUInt.asTypeOf(fflagsAll)
  private val outFFlags = fflagsEn.zip(fflagsAll).map {
    case (en, fflag) => Mux(en, fflag, 0.U(5.W))
  }.reduce(_ | _)
  io.out.bits.res.fflags.get := outFFlags

  private val maskToMgu = outSrcMask
  mgu.io.in.vd := resultData.asUInt
  mgu.io.in.oldVd := outOldVd
  mgu.io.in.mask := maskToMgu
  mgu.io.in.info.ta := outVecCtrl.vta
  mgu.io.in.info.ma := outVecCtrl.vma
  mgu.io.in.info.vl := outVl
  mgu.io.in.info.vlmul := outVecCtrl.vlmul
  mgu.io.in.info.valid := io.out.valid
  mgu.io.in.info.vstart := outVecCtrl.vstart
  mgu.io.in.info.eew := outVecCtrl.vsew
  mgu.io.in.info.vsew := outVecCtrl.vsew
  mgu.io.in.info.vdIdx := outVecCtrl.vuopIdx
  mgu.io.in.info.narrow := false.B
  mgu.io.in.info.dstMask := outVecCtrl.isDstMask
  mgu.io.in.isIndexedVls := false.B

  io.out.bits.res.data := mgu.io.out.vd
  io.out.bits.ctrl.exceptionVec.get(ExceptionNO.illegalInstr) := mgu.io.out.illegal
}
