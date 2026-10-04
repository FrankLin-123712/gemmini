
package gemmini

import chisel3._
import chisel3.util._

object NormCmd extends ChiselEnum {
  val RESET, SUM, MEAN, VARIANCE, INV_STDDEV, MAX, SUM_EXP, INV_SUM_EXP = Value

  def writes_to_main_memory(cmd: Type): Bool = {
    cmd === RESET
  }

  // In the NHWC ABI, MEAN and INV_STDDEV export raw vector sums. The CPU
  // divides by the spatial count and computes sqrt(var + epsilon).
  def nhwc_writes_to_main_memory(cmd: Type): Bool = {
    cmd === RESET || cmd === MEAN || cmd === INV_STDDEV
  }

  def non_reset_version(cmd: Type): Type = {
    MuxCase(cmd, Seq(
      (cmd === MEAN) -> SUM,
      (cmd === MAX) -> MAX,
      (cmd === INV_STDDEV) -> VARIANCE,
      (cmd === INV_SUM_EXP) -> SUM_EXP
    ))
  }
}
