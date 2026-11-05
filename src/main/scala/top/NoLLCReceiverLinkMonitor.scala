/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  * Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You can use this software according to the terms and conditions of the Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  *
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
  * EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
  * MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
  * See the Mulan PSL v2 for more details.
  * *************************************************************************************
  */

package top

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xscache.chi._

/** CHI receiver link for the NoLLC bridge.
  *
  * @param rxDatLCreditNum Number of link credits advertised for incoming RN-to-bridge DAT flits (1 to 15).
  *   Each credit reserves space for one flit; the configured count also sets the DAT receive FIFO depth.
  *   On the 256-bit CHI data channel, one flit carries a 32-byte beat; this count is independent of transaction slots.
  *   Credits must cover the pipelined credit/data round trip. With the upstream default of four credits,
  *   a continuous write stream can exhaust its credits before returned credits arrive, leaving AXI W bubbles.
  *   The NoLLC bridge selects eight credits to sustain one W beat per cycle, using four extra DAT FIFO entries.
  */
private[top] class NoLLCReceiverLinkMonitor(
  rxDatLCreditNum: Int = LCredit2Decoupled.defaultLCreditNum
)(implicit p: Parameters) extends Module {
  require(rxDatLCreditNum >= 1 && rxDatLCreditNum <= 15, "NoLLC DAT receive credits must be between 1 and 15")

  val io = IO(new Bundle {
    val in = Flipped(new PortIO)
    val out = new DecoupledPortIO
  })

  val txState = RegInit(LinkStates.STOP)
  val rxState = RegInit(LinkStates.STOP)

  Seq(txState, rxState).zip(MixedVecInit(Seq(io.in.tx, io.in.rx))).foreach { case (state, link) =>
    state := MuxLookup(Cat(link.linkactivereq, link.linkactiveack), LinkStates.STOP)(Seq(
      Cat(true.B, false.B) -> LinkStates.ACTIVATE,
      Cat(true.B, true.B) -> LinkStates.RUN,
      Cat(false.B, true.B) -> LinkStates.DEACTIVATE,
      Cat(false.B, false.B) -> LinkStates.STOP
    ))
  }

  val txreqDeact, txrspDeact, txdatDeact = Wire(Bool())
  val txDeact = txreqDeact && txrspDeact && txdatDeact
  LCredit2Decoupled(io.in.tx.req, io.out.tx.req, LinkState(txState), txreqDeact, Some("rxreq"))
  LCredit2Decoupled(io.in.tx.rsp, io.out.tx.rsp, LinkState(txState), txrspDeact, Some("rxrsp"))
  LCredit2Decoupled(
    io.in.tx.dat, io.out.tx.dat, LinkState(txState), txdatDeact, Some("rxdat"), lcreditNum = rxDatLCreditNum
  )
  Decoupled2LCredit(io.out.rx.snp, io.in.rx.snp, LinkState(rxState), Some("txsnp"))
  Decoupled2LCredit(io.out.rx.rsp, io.in.rx.rsp, LinkState(rxState), Some("txrsp"))
  Decoupled2LCredit(io.out.rx.dat, io.in.rx.dat, LinkState(rxState), Some("txdat"))

  io.in.rxsactive := true.B
  io.in.rx.linkactivereq := !reset.asBool
  io.in.tx.linkactiveack := RegNext(io.in.tx.linkactivereq) || !txDeact

  io.in.syscoack := RegNext(io.in.syscoreq)

  dontTouch(io.in)
}
