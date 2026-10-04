package gemmini

import chisel3._
import chisel3.util._
import hardfloat._

/** FP32 NHWC normalization ABI v1.
  *
  * Each lane is one channel and each request is one spatial position. Four
  * contexts retain statistics across accumulator tiles for up to 64 channels.
  * CPU finishing computes mean, alpha = gamma / sqrt(variance + epsilon), and
  * shift = beta - mean * alpha. Commands load these vectors through normal
  * accumulator reads; the final pass computes x * alpha + shift.
  *
  * SUM / MEAN: accumulate / accumulate and export channel sums.
  * VARIANCE / INV_STDDEV: accumulate / export centered squared differences.
  * MAX / SUM_EXP / INV_SUM_EXP: load mean / alpha / shift vectors.
  * RESET: apply affine transform, optionally followed by ReLU.
  *
  * Capture the request before selecting operands. FP decoding, raw arithmetic,
  * rounding and writeback have separate register boundaries; each mathematical
  * operation still rounds to FP32 before the next operation begins. Requests
  * are serialized to preserve spatial summation order and avoid accumulation
  * feedback hazards. Ordinary activations use the same ordered output slot.
  */
class NhwcNormalizer[T <: Data: Arithmetic, U <: Data](
    maxLen: Int, fullDataType: Vec[Vec[T]], scaleType: U, numStats: Int = 4)
    extends Module {
  private val accType = fullDataType.head.head.cloneType
  private val lanes = fullDataType.flatten.size
  require(accType match { case Float(8, 24, false) => true; case _ => false })
  require(scaleType match { case Float(8, 24, false) => true; case _ => false })
  require(lanes == 16 && maxLen == lanes && numStats == 4)

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new NormalizedInput(maxLen, numStats, fullDataType, scaleType)))
    val out = Decoupled(new NormalizedOutput(fullDataType, scaleType))
    val busy = Output(Bool())
  })

  private def zero = 0.U.asTypeOf(accType)
  private def one = "h3f800000".U.asTypeOf(accType)
  private def vectors(value: => T) = VecInit(Seq.fill(numStats)(VecInit(Seq.fill(lanes)(value))))

  val sums = RegInit(vectors(zero))
  val means = RegInit(vectors(zero))
  val alphas = RegInit(vectors(one))
  val shifts = RegInit(vectors(zero))
  val counts = RegInit(VecInit(Seq.fill(numStats)(0.U(32.W))))

  val idle :: dispatch :: decode :: calculate :: round :: commit :: relu :: output :: Nil = Enum(8)
  val state = RegInit(idle)
  val sumOp :: subtractOp :: squareOp :: multiplyOp :: shiftOp :: Nil = Enum(5)
  val operation = Reg(UInt(3.W))
  val request = Reg(new NormalizedInput(maxLen, numStats, fullDataType, scaleType))
  val result = Reg(new NormalizedOutput(fullDataType, scaleType))

  // Keep the context mux and accumulator-response handshake out of the FP
  // paths. All arithmetic below consumes only these registered operands.
  val operandA = Reg(Vec(lanes, UInt(32.W)))
  val operandB = Reg(Vec(lanes, UInt(32.W)))
  val rawA = Reg(Vec(lanes, new RawFloat(8, 24)))
  val rawB = Reg(Vec(lanes, new RawFloat(8, 24)))
  val rawResult = Reg(Vec(lanes, new RawFloat(8, 26)))
  val invalidResult = Reg(Vec(lanes, Bool()))
  val roundedResult = Reg(Vec(lanes, UInt(33.W)))
  val encodedResult = Wire(Vec(lanes, UInt(32.W)))

  for (lane <- 0 until lanes) {
    val adder = Module(new AddRawFN(8, 24))
    adder.io.a := rawA(lane)
    adder.io.b := rawB(lane)
    adder.io.subOp := operation === subtractOp
    adder.io.roundingMode := consts.round_near_even

    val multiplier = Module(new MulRawFN(8, 24))
    multiplier.io.a := rawA(lane)
    multiplier.io.b := rawB(lane)
    val multiply = operation === squareOp || operation === multiplyOp

    val rounder = Module(new RoundRawFNToRecFN(8, 24, 0))
    rounder.io.in := rawResult(lane)
    rounder.io.invalidExc := invalidResult(lane)
    rounder.io.infiniteExc := false.B
    rounder.io.roundingMode := consts.round_near_even
    rounder.io.detectTininess := consts.tininess_afterRounding

    when (state === decode) {
      rawA(lane) := rawFloatFromFN(8, 24, operandA(lane))
      rawB(lane) := rawFloatFromFN(8, 24, operandB(lane))
    }
    when (state === calculate) {
      rawResult(lane) := Mux(multiply, multiplier.io.rawOut, adder.io.rawOut)
      invalidResult(lane) := Mux(multiply, multiplier.io.invalidExc, adder.io.invalidExc)
    }
    when (state === round) {
      roundedResult(lane) := rounder.io.out
    }
    encodedResult(lane) := fNFromRecFN(8, 24, roundedResult(lane))
  }

  io.in.ready := state === idle || (state === output && io.out.ready)
  io.out.valid := state === output
  io.out.bits := result
  io.busy := state =/= idle

  when (io.out.fire) { state := idle }

  val savedId = request.stats_id
  val exportSum = request.cmd === NormCmd.MEAN || request.cmd === NormCmd.INV_STDDEV
  when (state === decode) { state := calculate }
  when (state === calculate) { state := round }
  when (state === round) { state := commit }
  when (state === commit) {
    switch (operation) {
      is (sumOp) {
        assert(counts(savedId) =/= "hffffffff".U, "NHWC normalization spatial count overflow")
        for (lane <- 0 until lanes) {
          val updated = encodedResult(lane).asTypeOf(accType)
          val validLane = lane.U < request.len
          when (validLane) {
            sums(savedId)(lane) := Mux(exportSum, zero, updated)
          }
          when (exportSum) {
            // Clear padded lanes too, so later wider tails cannot inherit sums.
            sums(savedId)(lane) := zero
            result.acc_read_resp.data(lane / fullDataType.head.size)(lane % fullDataType.head.size) :=
              Mux(validLane, updated, zero)
          }
        }
        counts(savedId) := Mux(exportSum, 0.U, counts(savedId) + 1.U)
        state := Mux(exportSum, output, idle)
      }
      is (subtractOp) {
        // Square the rounded FP32 difference, preserving the original ABI.
        operandA := encodedResult
        operandB := encodedResult
        operation := squareOp
        state := decode
      }
      is (squareOp) {
        for (lane <- 0 until lanes) {
          operandA(lane) := sums(savedId)(lane).asUInt
          operandB(lane) := encodedResult(lane)
        }
        operation := sumOp
        state := decode
      }
      is (multiplyOp) {
        // Do not fuse x * alpha + shift: the product must round first.
        for (lane <- 0 until lanes) {
          operandA(lane) := encodedResult(lane)
          operandB(lane) := shifts(savedId)(lane).asUInt
        }
        operation := shiftOp
        state := decode
      }
      is (shiftOp) {
        for (lane <- 0 until lanes) {
          result.acc_read_resp.data(lane / fullDataType.head.size)(lane % fullDataType.head.size) :=
            Mux(lane.U < request.len, encodedResult(lane).asTypeOf(accType), zero)
        }
        state := relu
      }
    }
  }
  when (state === relu) {
    for (lane <- 0 until lanes) {
      val value = result.acc_read_resp.data(lane / fullDataType.head.size)(lane % fullDataType.head.size)
      val bits = value.asUInt
      val isNaN = bits(30, 23).andR && bits(22, 0).orR
      // Exactly FP32 value < +0: preserve -0 and NaNs, clip negative infinity.
      val negative = bits(31) && bits(30, 0).orR && !isNaN
      when (request.acc_read_resp.act === Activation.NHWC_NORM_RELU && negative) {
        value := zero
      }
    }
    state := output
  }

  when (state === dispatch) {
    val input = request
    val inputData = input.acc_read_resp.data.flatten
    val id = input.stats_id
    val isNhwc = Activation.isNhwcNorm(input.acc_read_resp.act)
    result.acc_read_resp := input.acc_read_resp
    result.mean := zero
    result.max := zero
    result.inv_stddev := 0.U.asTypeOf(scaleType)
    result.inv_sum_exp := 0.U.asTypeOf(scaleType)

    when (!isNhwc) {
      // Ordinary activations preserve all accumulator-response metadata.
      state := output
    }.otherwise {
      assert(input.len > 0.U && input.len <= lanes.U)
      // Affine and ReLU are already applied here. Force the existing output
      // scaler to identity, including for statistic-vector exports.
      result.acc_read_resp.act := Activation.NONE
      result.acc_read_resp.scale := "h3f800000".U.asTypeOf(scaleType)
      switch (input.cmd) {
        is (NormCmd.SUM, NormCmd.MEAN) {
          for (lane <- 0 until lanes) {
            operandA(lane) := sums(id)(lane).asUInt
            operandB(lane) := inputData(lane).asUInt
          }
          operation := sumOp
          state := decode
        }
        is (NormCmd.VARIANCE, NormCmd.INV_STDDEV) {
          for (lane <- 0 until lanes) {
            operandA(lane) := inputData(lane).asUInt
            operandB(lane) := means(id)(lane).asUInt
          }
          operation := subtractOp
          state := decode
        }
        is (NormCmd.MAX) {
          for (lane <- 0 until lanes) {
            means(id)(lane) := Mux(lane.U < input.len, inputData(lane), zero)
            sums(id)(lane) := zero
          }
          counts(id) := 0.U
          state := idle
        }
        is (NormCmd.SUM_EXP, NormCmd.INV_SUM_EXP) {
          for (lane <- 0 until lanes) {
            when (input.cmd === NormCmd.SUM_EXP) {
              alphas(id)(lane) := Mux(lane.U < input.len, inputData(lane), one)
            }.otherwise {
              shifts(id)(lane) := Mux(lane.U < input.len, inputData(lane), zero)
            }
          }
          state := idle
        }
        is (NormCmd.RESET) {
          for (lane <- 0 until lanes) {
            operandA(lane) := inputData(lane).asUInt
            operandB(lane) := alphas(id)(lane).asUInt
          }
          operation := multiplyOp
          state := decode
        }
      }
    }
  }

  // The handshake ends at this register, never at an FP arithmetic endpoint.
  // Keep this last so an output drain can accept the next request on that edge.
  when (io.in.fire) {
    request := io.in.bits
    state := dispatch
  }
}
