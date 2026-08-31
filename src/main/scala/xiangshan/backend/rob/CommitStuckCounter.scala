package xiangshan.backend.rob

import chisel3._
import chisel3.util.HasBlackBoxInline

class CommitStuckFinish(width: Int, timeout: Int) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val stuck = Input(Bool())
    val fired = Output(Bool())
  })

  require(width > 0, "commit-stuck watchdog width must be positive")
  require(timeout > 0, "commit-stuck watchdog timeout must be positive")

  setInline("CommitStuckFinish.sv",
    s"""
       |module CommitStuckFinish #(
       |  parameter integer TIMEOUT = $timeout
       |) (
       |  input  clock,
       |  input  reset,
       |  input  stuck,
       |  output fired
       |);
       |`ifdef PALLADIUM
       |  reg [${width - 1}:0] count;
       |  reg fired_reg;
       |
       |  assign fired = fired_reg;
       |
       |  always @(posedge clock) begin
       |    if (reset) begin
       |      count     <= '0;
       |      fired_reg <= 1'b0;
       |    end
       |    else if (!stuck) begin
       |      count     <= '0;
       |      fired_reg <= 1'b0;
       |    end
       |    else if (!fired_reg) begin
       |      if (count == TIMEOUT - 1) begin
       |        $$display("Commit stuck for %0d cycles; finishing simulation", TIMEOUT);
       |        $$finish;
       |        fired_reg <= 1'b1;
       |      end
       |      else begin
       |        count <= count + 1'b1;
       |      end
       |    end
       |  end
       |`else
       |  assign fired = 1'b0;
       |`endif // PALLADIUM
       |endmodule
       |""".stripMargin
  )
}


class CommitStuckCounter(width: Int, forceEnable: Bool) extends Module {
  val io = IO(new Bundle {
    val stuck = Input(Bool())
    val runtimeEnable = Input(Bool())
    val overflowEnabled = Input(Bool())
    val count = Output(UInt(width.W))
    val overflow = Output(Bool())
  })

  private val count = RegInit(0.U(width.W))
  private val effectiveEnable = io.runtimeEnable || forceEnable

  when (!effectiveEnable || !io.stuck) {
    count := 0.U
  }.otherwise {
    count := count + 1.U
  }

  io.count := count
  io.overflow := count.andR && io.overflowEnabled
}
