package gemmini

import chisel3._
import chisel3.util._

class SoftAggCommand extends Bundle {
  val funct = UInt(7.W)
  val rs1 = UInt(64.W)
  val rs2 = UInt(64.W)
}

/** Admission/descriptor state for CONFIG and RUN (STATUS uses the RoCC reply
  * queue). Faulted DMA must not hold these commands ahead of its recovery FLUSH.
  * Rejected submissions invalidate the descriptor and never start local IO.
  */
class SoftAggFrontend(enabled: Boolean) extends Module {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(new SoftAggCommand))
    val blocked = Input(Bool())
    val fault = Input(Bool())
    val start = Decoupled(new SoftAggDescriptor)
    val error = Output(Bool())
  })
  val descriptor = RegInit(0.U.asTypeOf(new SoftAggDescriptor))
  val error = RegInit(false.B)
  io.error := error
  val run = io.command.bits.funct === GemminiISA.SOFTAGG_RUN
  io.command.ready := io.fault || (!io.blocked && Mux(run && enabled.B, io.start.ready, true.B))
  io.start.valid := enabled.B && io.command.valid && run && !io.blocked && !io.fault
  io.start.bits := descriptor
  io.start.bits.s := io.command.bits.rs1(31, 0)
  io.start.bits.v := io.command.bits.rs1(63, 32)
  io.start.bits.out := io.command.bits.rs2(63, 32)
  io.start.bits.rows := io.command.bits.rs2(31, 16)
  io.start.bits.cols := io.command.bits.rs2(15, 8)
  io.start.bits.mode := io.command.bits.rs2(7, 0)
  when(io.command.fire) {
    if (enabled) {
      when(io.command.bits.funct === GemminiISA.SOFTAGG_CONFIG) {
        descriptor.biasS := io.command.bits.rs1(31, 0)
        descriptor.biasV := io.command.bits.rs1(63, 32)
        descriptor.strideS := io.command.bits.rs2(31, 0)
        descriptor.strideV := io.command.bits.rs2(63, 32)
        descriptor.configured := true.B
        error := false.B
      }
    } else { error := true.B }
  }
  when(io.fault) {
    descriptor.configured := false.B
    error := true.B
  }
}
