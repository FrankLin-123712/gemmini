package gemmini

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec

class NhwcNormalizerSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def bits(x: scala.Float): BigInt = BigInt(java.lang.Float.floatToRawIntBits(x).toLong & 0xffffffffL)

  it should "keep NHWC normalization opt-in and reserve its configuration subcommand" in {
    val baseline = GemminiCustomConfigs.dpvoFP32Dim16Config
    val accelerated = GemminiCustomConfigs.dpvoFP32Dim16NhwcNormConfig
    assert(!baseline.has_nhwc_norm)
    assert(accelerated.has_nhwc_norm && !accelerated.has_normalizations && !accelerated.has_im2col)
    assert(accelerated.has_loop_conv)
    val header = accelerated.generateHeader()
    assert(header.contains("#define HAS_NHWC_NORM\n"))
    assert(header.contains("#define NHWC_NORM_ABI_VERSION 1\n"))
    assert(header.contains("#define NHWC_NORM_STAT_IDS 4\n"))
    assert(!header.contains("#define HAS_NORMALIZATIONS\n"))
    assert(!header.contains("#define HAS_IM2COL\n"))
  }

  it should "enable both engines in the combined configuration without changing standalone targets" in {
    val corrOnly = GemminiCustomConfigs.dpvoFP32Dim16CorrGatherConfig
    val normOnly = GemminiCustomConfigs.dpvoFP32Dim16NhwcNormConfig
    val combined = GemminiCustomConfigs.dpvoFP32Dim16CorrGatherNhwcNormConfig
    assert(corrOnly.has_corr_gather && !corrOnly.has_nhwc_norm)
    assert(normOnly.has_nhwc_norm && !normOnly.has_corr_gather)
    assert(combined.has_corr_gather && combined.has_nhwc_norm)
    assert(!combined.has_normalizations && !combined.has_im2col && combined.has_loop_conv)
    assert(combined.sp_banks == 4 && combined.sp_bank_entries == 1024)
    assert(combined.acc_banks * combined.acc_bank_entries == 1024)
    assert(combined.headerFileName == "gemmini_params_dpvo_fp32_dim16_corr_gather_nhwc_norm.h")
    val header = combined.generateHeader()
    assert(header.contains("#define HAS_CORR_GATHER 1\n"))
    assert(header.contains("#define HAS_NHWC_NORM\n"))
    assert(header.contains("#define NHWC_NORM_ABI_VERSION 1\n"))
    assert(!header.contains("#define HAS_IM2COL\n"))
  }

  it should "keep independent channel contexts, export centered statistics and preserve ordered backpressure" in {
    test(new NhwcNormalizer(16, Vec(16, Vec(1, Float(8, 24))), Float(8, 24)))
      .withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      d.clock.setTimeout(20000)
      d.io.in.valid.poke(false.B)
      d.io.out.ready.poke(false.B)
      d.clock.step(2)
      val maxWait = 100
      def drive(cmd: NormCmd.Type, context: Int, values: Seq[scala.Float], act: Int = 5): Unit = {
        d.io.in.bits.len.poke(values.size.U)
        d.io.in.bits.stats_id.poke(context.U)
        d.io.in.bits.cmd.poke(cmd)
        d.io.in.bits.acc_read_resp.act.poke(act.U)
        d.io.in.bits.acc_read_resp.scale.bits.poke(bits(0.5f).U)
        d.io.in.bits.acc_read_resp.fromDMA.poke(true.B)
        d.io.in.bits.acc_read_resp.acc_bank_id.poke(1.U)
        d.io.in.bits.acc_read_resp.igelu_qb.bits.poke(0.U)
        d.io.in.bits.acc_read_resp.igelu_qc.bits.poke(0.U)
        d.io.in.bits.acc_read_resp.iexp_qln2.bits.poke(0.U)
        d.io.in.bits.acc_read_resp.iexp_qln2_inv.bits.poke(0.U)
        for (i <- 0 until 16) d.io.in.bits.acc_read_resp.data(i)(0).bits.poke(bits(values.lift(i).getOrElse(123456.0f)).U)
      }
      def send(cmd: NormCmd.Type, context: Int, values: Seq[scala.Float], act: Int = 5): Unit = {
        var waiting = 0
        while (!d.io.in.ready.peek().litToBoolean && waiting < maxWait) { d.clock.step(); waiting += 1 }
        assert(waiting < maxWait, "input did not become ready")
        drive(cmd, context, values, act)
        d.io.in.valid.poke(true.B)
        d.clock.step()
        d.io.in.valid.poke(false.B)
      }
      def waitOutput(): Int = {
        var waiting = 0
        while (!d.io.out.valid.peek().litToBoolean && waiting < maxWait) {
          d.io.busy.expect(true.B)
          d.clock.step()
          waiting += 1
        }
        assert(waiting < maxWait, "output did not become valid")
        waiting
      }
      def checkOutput(expected: Seq[scala.Float], activation: Int = 0, scale: scala.Float = 1.0f): Unit = {
        d.io.out.valid.expect(true.B)
        d.io.busy.expect(true.B)
        d.io.out.bits.acc_read_resp.act.expect(activation.U)
        d.io.out.bits.acc_read_resp.scale.bits.expect(bits(scale).U)
        d.io.out.bits.acc_read_resp.fromDMA.expect(true.B)
        d.io.out.bits.acc_read_resp.acc_bank_id.expect(1.U)
        for (i <- 0 until 16) {
          val value = expected.lift(i).getOrElse(0.0f)
          if (java.lang.Float.isNaN(value)) {
            // HardFloat may canonicalize NaNs; require IEEE NaN classification,
            // without depending on JVM payload/sign propagation.
            val actual = d.io.out.bits.acc_read_resp.data(i)(0).bits.peek().litValue
            assert((actual & BigInt("7f800000", 16)) == BigInt("7f800000", 16) &&
              (actual & BigInt("007fffff", 16)) != 0, s"lane $i expected NaN, got 0x${actual.toString(16)}")
          } else {
            d.io.out.bits.acc_read_resp.data(i)(0).bits.expect(bits(value).U)
          }
        }
      }
      def receive(expected: Seq[scala.Float], activation: Int = 0, scale: scala.Float = 1.0f,
                  stallCycles: Int = 3): Unit = {
        waitOutput()
        for (_ <- 0 until stallCycles) {
          checkOutput(expected, activation, scale)
          d.io.in.ready.expect(false.B)
          d.clock.step()
        }
        d.io.out.ready.poke(true.B)
        d.clock.step()
        d.io.out.ready.poke(false.B)
      }
      def waitIdle(): Unit = {
        var waiting = 0
        while (d.io.busy.peek().litToBoolean && waiting < maxWait) {
          d.io.out.valid.expect(false.B)
          d.clock.step()
          waiting += 1
        }
        assert(waiting < maxWait, "non-exporting command did not complete")
        d.io.out.valid.expect(false.B)
        d.io.in.ready.expect(true.B)
      }
      def resetUnit(): Unit = {
        d.io.in.valid.poke(false.B)
        d.io.out.ready.poke(false.B)
        d.reset.poke(true.B)
        d.clock.step(2)
        d.reset.poke(false.B)
        d.clock.step()
        d.io.busy.expect(false.B)
        d.io.out.valid.expect(false.B)
        d.io.in.ready.expect(true.B)
      }
      val vectors = (0 until 4).map(ctx => (0 until 16).map(i => (ctx * 16 + i + 1).toFloat))
      // Interleave all four contexts and export the final sample separately.
      for (ctx <- 0 until 4) send(NormCmd.SUM, ctx, vectors(ctx))
      for (ctx <- 0 until 4) {
        send(NormCmd.MEAN, ctx, vectors(ctx).map(_ + 2.0f))
        receive(vectors(ctx).map(x => x + x + 2.0f))
      }
      for (ctx <- 0 until 4) send(NormCmd.MAX, ctx, vectors(ctx).map(_ + 1.0f))
      for (ctx <- 0 until 4) send(NormCmd.VARIANCE, ctx, vectors(ctx))
      for (ctx <- 0 until 4) {
        send(NormCmd.INV_STDDEV, ctx, vectors(ctx).map(_ + 2.0f))
        receive(Seq.fill(16)(2.0f))
        send(NormCmd.SUM_EXP, ctx, Seq.fill(16)(-0.5f))
        send(NormCmd.INV_SUM_EXP, ctx, vectors(ctx).map(_ * 0.5f))
      }
      // Generic affine parameters include negative gamma and nonzero shift.
      send(NormCmd.RESET, 2, vectors(2).map(_ + 2.0f))
      receive(Seq.fill(16)(-1.0f))
      send(NormCmd.RESET, 3, vectors(3).map(_ + 2.0f), act = 6)
      receive(Seq.fill(16)(0.0f))
      // Export resets statistics; padded lanes must not contribute.
      send(NormCmd.MEAN, 0, Seq.fill(11)(3.0f))
      receive(Seq.fill(11)(3.0f))
      send(NormCmd.MEAN, 0, Seq.fill(16)(1.0f))
      receive(Seq.fill(16)(1.0f))
      // Ordinary activations preserve data and scaling for AccumulatorScale.
      send(NormCmd.RESET, 0, vectors.head, act = 1)
      receive(vectors.head, activation = 1, scale = 0.5f)
      d.io.busy.expect(false.B)

      // JVM Float operations round independently to binary32. Keep this oracle
      // separate from the DUT's HardFloat implementation, including underflow.
      def add(a: scala.Float, b: scala.Float): scala.Float = a + b
      def multiply(a: scala.Float, b: scala.Float): scala.Float = a * b
      def squareDifference(x: scala.Float, mean: scala.Float): scala.Float = {
        val delta: scala.Float = x - mean
        multiply(delta, delta)
      }
      def affine(x: scala.Float, alpha: scala.Float, shift: scala.Float, relu: Boolean): scala.Float = {
        val product = multiply(x, alpha)
        val value = add(product, shift)
        if (relu && value < 0.0f) 0.0f else value
      }
      def fromBits(x: Int): scala.Float = java.lang.Float.intBitsToFloat(x)
      val random = new scala.util.Random(0x4e485743L)
      def randomFinite(): scala.Float = {
        val sign = if (random.nextBoolean()) Int.MinValue else 0
        // Include subnormals and a broad finite range, keeping squared values
        // and accumulated results finite so raw-bit comparisons are unambiguous.
        val exponent = if (random.nextInt(4) == 0) 0 else 87 + random.nextInt(80)
        fromBits(sign | (exponent << 23) | (random.nextInt() & 0x7fffff))
      }
      val special = Seq(0.0f, -0.0f, fromBits(1), fromBits(0x80000001),
        fromBits(0x007fffff), fromBits(0x807fffff), fromBits(0x00800000),
        fromBits(0x80800000), fromBits(0x3f800001), fromBits(0xbf800001))

      resetUnit()
      val probeValues = (0 until 16).map(i => (i + 1) * 0.125f)
      send(NormCmd.MEAN, 0, probeValues)
      val sumLatency = waitOutput()
      receive(probeValues)
      send(NormCmd.INV_STDDEV, 0, probeValues)
      val varianceLatency = waitOutput()
      receive(probeValues.map(x => multiply(x, x)))
      send(NormCmd.RESET, 0, probeValues)
      val affineLatency = waitOutput()
      receive(probeValues)
      send(NormCmd.RESET, 0, probeValues, act = 1)
      val bypassLatency = waitOutput()
      receive(probeValues, activation = 1, scale = 0.5f)
      println(s"NHWC latency (accepted input edge=0; output valid; no output stall): sum=$sumLatency variance=$varianceLatency affine=$affineLatency bypass=$bypassLatency")

      // Four contexts interleave long enough to expose RAW hazards. Lane zero
      // is order-sensitive: (2^24 + 1) + (-2^24) rounds to zero, not one.
      val sumRows = (0 until 4).map { ctx =>
        (0 until 7).map { row =>
          (0 until 16).map { lane =>
            if (lane == 0) Seq(16777216.0f, 1.0f, -16777216.0f, 0.0f, 0.0f, 0.0f, 0.0f)(row)
            else if (lane % 3 == 0) special((ctx + row + lane) % special.size)
            else randomFinite()
          }
        }
      }
      val sumExpected = Array.fill(4)(Seq.fill(16)(0.0f))
      val tails = Seq(1, 7, 11, 16)
      for (row <- 0 until 6; ctx <- 0 until 4) {
        send(NormCmd.SUM, ctx, sumRows(ctx)(row))
        sumExpected(ctx) = sumExpected(ctx).zip(sumRows(ctx)(row)).map { case (a, b) => add(a, b) }
        // Changing an invalid input after acceptance must not corrupt captured
        // operands, context, command, or the completion metadata.
        drive(NormCmd.MAX, (ctx + 1) % 4, Seq.fill(16)(-9876.5f))
      }
      waitIdle()
      for (ctx <- 0 until 4) {
        val tail = tails(ctx)
        send(NormCmd.MEAN, ctx, sumRows(ctx).last.take(tail))
        val expected = sumExpected(ctx).zip(sumRows(ctx).last).map { case (a, b) => add(a, b) }
        receive(expected.take(tail), stallCycles = ctx + 1)
        // Export clears even masked lanes before a subsequent wider invocation.
        send(NormCmd.MEAN, ctx, Seq.fill(16)(1.0f))
        receive(Seq.fill(16)(1.0f))
      }

      val means = (0 until 4).map(ctx => (0 until 16).map(lane =>
        if (lane == 0) 4096.0f + ctx * 16.0f else randomFinite()))
      val varianceRows = (0 until 4).map { ctx =>
        (0 until 5).map { row =>
          (0 until 16).map { lane =>
            if (lane == 0) means(ctx)(lane) + (row - 2) * 0.001f
            else if (lane % 3 == 0) special((ctx + row + lane) % special.size)
            else randomFinite()
          }
        }
      }
      val varianceExpected = Array.fill(4)(Seq.fill(16)(0.0f))
      for (ctx <- 0 until 4) send(NormCmd.MAX, ctx, means(ctx))
      for (row <- 0 until 4; ctx <- 0 until 4) {
        send(NormCmd.VARIANCE, ctx, varianceRows(ctx)(row))
        varianceExpected(ctx) = (0 until 16).map(lane =>
          add(varianceExpected(ctx)(lane), squareDifference(varianceRows(ctx)(row)(lane), means(ctx)(lane))))
        drive(NormCmd.SUM_EXP, (ctx + 1) % 4, Seq.fill(16)(0.0f))
      }
      waitIdle()
      for (ctx <- 0 until 4) {
        send(NormCmd.INV_STDDEV, ctx, varianceRows(ctx).last)
        receive((0 until 16).map(lane =>
          add(varianceExpected(ctx)(lane), squareDifference(varianceRows(ctx).last(lane), means(ctx)(lane)))),
          stallCycles = 5 - ctx)
      }

      // Explicitly distinguish separate multiply/add from FMA. These exactly
      // representable operands give 0 after two roundings, but 1 with FMA.
      assert(bits(affine(4097.0f, 4097.0f, -16785408.0f, false)) == bits(0.0f))
      assert(bits(java.lang.Math.fma(4097.0f, 4097.0f, -16785408.0f)) == bits(1.0f))
      val affineInputs = (0 until 4).map { _ =>
        Seq(4097.0f, -0.0f, 0.0f, fromBits(1), fromBits(0x80000001),
          fromBits(0x007fffff), fromBits(0x00800000)) ++ Seq.fill(9)(randomFinite())
      }
      val alphas = (0 until 4).map { _ =>
        Seq(4097.0f, 1.0f, -1.0f, 1.0f, 1.0f, 1.0f, 1.0f) ++ Seq.fill(9)(randomFinite())
      }
      val shifts = (0 until 4).map { _ =>
        Seq(-16785408.0f, -0.0f, -0.0f, 0.0f, -0.0f, -fromBits(1), -fromBits(0x007fffff)) ++
          Seq.fill(9)(randomFinite())
      }
      def affineExpected(ctx: Int, relu: Boolean): Seq[scala.Float] =
        (0 until 16).map(lane => affine(affineInputs(ctx)(lane), alphas(ctx)(lane), shifts(ctx)(lane), relu))
      for (ctx <- 0 until 4) {
        send(NormCmd.SUM_EXP, ctx, alphas(ctx))
        send(NormCmd.INV_SUM_EXP, ctx, shifts(ctx))
      }
      for (ctx <- 0 until 4; relu <- Seq(false, true)) {
        send(NormCmd.RESET, ctx, affineInputs(ctx), act = if (relu) 6 else 5)
        receive(affineExpected(ctx, relu), stallCycles = 1 + random.nextInt(7))
      }

      // Ordered ReLU preserves NaN classification and negative zero, clips
      // negative infinity, and masks the short output's inactive lanes to +0.
      val exceptional = Seq(scala.Float.PositiveInfinity, scala.Float.NegativeInfinity,
        fromBits(0x7fc12345), fromBits(0xffc12345), -0.0f, 0.0f, fromBits(1), fromBits(0x80000001))
      send(NormCmd.SUM_EXP, 2, Seq.fill(exceptional.size)(1.0f))
      send(NormCmd.INV_SUM_EXP, 2, Seq.fill(exceptional.size)(-0.0f))
      for (relu <- Seq(false, true)) {
        send(NormCmd.RESET, 2, exceptional, act = if (relu) 6 else 5)
        receive(exceptional.map(x => affine(x, 1.0f, -0.0f, relu)))
      }
      send(NormCmd.SUM_EXP, 2, Seq(0.0f, -0.0f))
      send(NormCmd.RESET, 2, exceptional.take(2), act = 6)
      receive(Seq(scala.Float.NaN, scala.Float.NaN))

      // Keep the next input asserted while the previous output is blocked, then
      // drain the old output and accept its replacement on the very same edge.
      send(NormCmd.RESET, 0, affineInputs(0))
      drive(NormCmd.RESET, 1, affineInputs(1), act = 6)
      d.io.in.valid.poke(true.B)
      var blockedCycles = 0
      while (!d.io.out.valid.peek().litToBoolean && blockedCycles < maxWait) {
        d.io.in.ready.expect(false.B)
        d.io.busy.expect(true.B)
        d.clock.step()
        blockedCycles += 1
      }
      assert(blockedCycles < maxWait)
      for (_ <- 0 until 5) {
        checkOutput(affineExpected(0, false))
        d.io.in.ready.expect(false.B)
        d.clock.step()
      }
      d.io.out.ready.poke(true.B)
      d.io.in.ready.expect(true.B)
      d.clock.step()
      d.io.in.valid.poke(false.B)
      d.io.out.ready.poke(false.B)
      receive(affineExpected(1, true))

      // A short parameter import resets its padded lanes to affine identity.
      send(NormCmd.SUM_EXP, 3, Seq.fill(5)(-2.0f))
      send(NormCmd.INV_SUM_EXP, 3, Seq.fill(5)(0.75f))
      send(NormCmd.RESET, 3, probeValues)
      receive(probeValues.zipWithIndex.map { case (x, lane) =>
        if (lane < 5) affine(x, -2.0f, 0.75f, false) else x
      })

      // Reset on each observed arithmetic stage and while an exported result is
      // blocked. Never infer a fixed implementation latency from the test.
      for (delay <- 0 to varianceLatency) {
        resetUnit()
        send(NormCmd.MAX, 2, Seq.fill(16)(1.25f))
        send(NormCmd.SUM_EXP, 2, Seq.fill(16)(-3.0f))
        send(NormCmd.INV_SUM_EXP, 2, Seq.fill(16)(7.0f))
        send(NormCmd.VARIANCE, 2, probeValues)
        send(NormCmd.INV_STDDEV, 2, probeValues)
        if (delay > 0) d.clock.step(delay)
        resetUnit()
        for (_ <- 0 until varianceLatency + 3) {
          d.io.out.valid.expect(false.B)
          d.io.busy.expect(false.B)
          d.clock.step()
        }
        send(NormCmd.RESET, 2, probeValues)
        receive(probeValues)
        send(NormCmd.MEAN, 2, Seq.fill(16)(1.0f))
        receive(Seq.fill(16)(1.0f))
      }
      d.io.busy.expect(false.B)
    }
  }
}
