package gemmini

import chisel3._

object Activation {
  val NONE = 0.U
  val RELU = 1.U
  val LAYERNORM = 2.U
  val IGELU = 3.U
  val SOFTMAX = 4.U
  // FP32 NHWC normalization ABI v1; independent from integer LAYERNORM.
  val NHWC_NORM = 5.U
  val NHWC_NORM_RELU = 6.U

  def isNhwcNorm(act: UInt): Bool = act === NHWC_NORM || act === NHWC_NORM_RELU

  val bitwidth = 3
}
