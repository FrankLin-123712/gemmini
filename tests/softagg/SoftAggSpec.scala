package gemmini

import chisel3._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable

class SoftAggFrontendHarness(enabled: Boolean) extends Module {
  val io = IO(new Bundle {
    val command = Flipped(chisel3.util.Decoupled(new SoftAggCommand))
    val blocked = Input(Bool())
    val fault = Input(Bool())
    val flushes = Output(UInt(8.W))
    val starts = Output(UInt(8.W))
    val configured = Output(Bool())
    val error = Output(Bool())
  })
  val queue = Module(new chisel3.util.Queue(new SoftAggCommand, 4))
  queue.io.enq <> io.command
  val frontend = Module(new SoftAggFrontend(enabled))
  val isSoftagg = queue.io.deq.bits.funct === GemminiISA.SOFTAGG_CONFIG || queue.io.deq.bits.funct === GemminiISA.SOFTAGG_RUN
  frontend.io.command.valid := queue.io.deq.valid && isSoftagg
  frontend.io.command.bits := queue.io.deq.bits
  frontend.io.blocked := io.blocked
  frontend.io.fault := io.fault
  frontend.io.start.ready := true.B
  queue.io.deq.ready := Mux(isSoftagg, frontend.io.command.ready, true.B)
  val flushes = RegInit(0.U(8.W)); val starts = RegInit(0.U(8.W)); val configured = RegInit(false.B)
  when(queue.io.deq.fire && queue.io.deq.bits.funct === GemminiISA.FLUSH_CMD) { flushes := flushes + 1.U }
  when(frontend.io.start.fire) { starts := starts + 1.U; configured := frontend.io.start.bits.configured }
  io.flushes := flushes; io.starts := starts; io.configured := configured; io.error := frontend.io.error
}

class SoftAggSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def bits(x: scala.Float): BigInt = BigInt(java.lang.Float.floatToRawIntBits(x).toLong & 0xffffffffL)
  private def value(x: BigInt): scala.Float = java.lang.Float.intBitsToFloat(x.toInt)
  private def fromBits(x: Int): scala.Float = java.lang.Float.intBitsToFloat(x)
  private def equal(got: BigInt, expected: scala.Float): Boolean =
    if (expected.isNaN) value(got).isNaN else got == bits(expected)
  private def fma(a: scala.Float, b: scala.Float, c: scala.Float): scala.Float = java.lang.Math.fma(a, b, c)
  private def exp(input: scala.Float): scala.Float = {
    var x = input
    if (x < -103.9720840454f) x = -103.9720840454f
    if (x > 88.7762626647950f) x = 88.7762626647950f
    val biased = fma(x, 1.44269504088896341f, 12582912.0f)
    val m = biased - 12582912.0f
    x = fma(m, -6.93145752e-1f, x)
    x = fma(m, -1.42860677e-6f, x)
    val e = java.lang.Float.floatToRawIntBits(biased) << 23
    val n = math.max(0xc1000000, math.min(0x3f800000, e))
    val overflow = fromBits(e - n + 0x3f800000)
    var p = fma(fromBits(0x3ab4a000), x, fromBits(0x3c092f6e))
    for (coefficient <- Seq(0x3d2aadad, 0x3e2aaa28, 0x3efffffb, 0x3f800000))
      p = fma(p, x, fromBits(coefficient))
    x = x * overflow
    p = fma(p, x, overflow)
    p * fromBits(n + 0x3f800000)
  }

  for (enabled <- Seq(true, false)) {
    it should s"release CONFIG/RUN ahead of a fault recovery FLUSH and invalidate old configuration (enabled=$enabled)" in {
      test(new SoftAggFrontendHarness(enabled)).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
        d.io.command.valid.poke(false.B); d.io.blocked.poke(false.B); d.io.fault.poke(false.B)
        d.clock.step(2)
        def send(funct: Int): Unit = {
          while(!d.io.command.ready.peek().litToBoolean) d.clock.step()
          d.io.command.bits.funct.poke(funct.U)
          d.io.command.bits.rs1.poke(BigInt("00000c0100000c00", 16).U)
          d.io.command.bits.rs2.poke(BigInt("0000000100000001", 16).U)
          d.io.command.valid.poke(true.B); d.clock.step(); d.io.command.valid.poke(false.B)
        }
        send(45); d.clock.step(3)
        d.io.blocked.poke(true.B)
        send(45); send(46); send(7) // FLUSH is funct 7 in this Gemmini ISA.
        d.clock.step(3)
        d.io.flushes.expect(0.U); d.io.starts.expect(0.U)
        d.io.fault.poke(true.B); d.clock.step(6)
        d.io.flushes.expect(1.U); d.io.starts.expect(0.U); d.io.error.expect(true.B)
        d.io.fault.poke(false.B); d.io.blocked.poke(false.B)
        send(46); d.clock.step(4)
        d.io.starts.expect((if(enabled) 1 else 0).U)
        d.io.configured.expect(false.B)
        send(45); send(46); d.clock.step(4)
        d.io.starts.expect((if(enabled) 2 else 0).U)
        d.io.configured.expect(enabled.B)
        d.io.error.expect((!enabled).B)
      }
    }
  }

  it should "round each arithmetic operation and retain independently completed divide lanes under output backpressure" in {
    test(new SoftAggArithmetic).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      d.clock.setTimeout(100000)
      d.io.in.valid.poke(false.B); d.io.out.ready.poke(false.B); d.clock.step(2)
      val vectors = Seq(
        (Seq(1.0f, -0.0f, 3.0f, scala.Float.PositiveInfinity), Seq(3.0f, 2.0f, 0.0f, scala.Float.PositiveInfinity), Seq(-1.0f, -0.0f, 2.0f, 1.0f)),
        (Seq(fromBits(1), scala.Float.NaN, -2.0f, fromBits(0x3f800001)), Seq(2.0f, 1.0f, -0.0f, fromBits(0x3f7fffff)), Seq(0.0f, 0.0f, -0.0f, -1.0f)))
      for ((a, b, c) <- vectors; op <- 0 to 4) {
        while(!d.io.in.ready.peek().litToBoolean) d.clock.step()
        d.io.in.bits.op.poke(op.U)
        for (i <- 0 until 4) { d.io.in.bits.a(i).poke(bits(a(i)).U); d.io.in.bits.b(i).poke(bits(b(i)).U); d.io.in.bits.c(i).poke(bits(c(i)).U) }
        d.io.in.valid.poke(true.B); d.clock.step(); d.io.in.valid.poke(false.B)
        var cycles = 0
        while(!d.io.out.valid.peek().litToBoolean && cycles < 100) { d.clock.step(); cycles += 1 }
        assert(cycles < 100)
        val expected = (0 until 4).map(i => op match {
          case 0 => a(i) + b(i); case 1 => a(i) - b(i); case 2 => a(i) * b(i)
          case 3 => fma(a(i), b(i), c(i)); case 4 => a(i) / b(i)
        })
        val captured = (0 until 4).map(i => d.io.out.bits(i).peek().litValue)
        for (i <- 0 until 4) assert(equal(captured(i), expected(i)), s"op=$op lane=$i got=${captured(i).toString(16)} expected=${bits(expected(i)).toString(16)}")
        d.clock.step(7)
        assert(d.io.out.valid.peek().litToBoolean)
        for (i <- 0 until 4) assert(d.io.out.bits(i).peek().litValue == captured(i))
        d.io.out.ready.poke(true.B); d.clock.step(); d.io.out.ready.poke(false.B)
        println(s"SoftAggArithmetic op=$op accept_to_valid_cycles=$cycles")
      }
    }
  }

  it should "match ordered MLAS SoftAgg with tails, strided ACC rows, delayed reads and write stalls, and reject invalid descriptors before IO" in {
    test(new SoftAgg(4096, 1024)).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      d.clock.setTimeout(1000000)
      d.io.start.valid.poke(false.B)
      d.io.memory.read.ready.poke(false.B); d.io.memory.response.valid.poke(false.B)
      d.io.memory.response.bits.poke(0.U); d.io.memory.write.ready.poke(false.B)
      d.clock.step(2)
      val memory = mutable.Map.empty[BigInt, Vector[BigInt]]
      var cycle = 0
      var pending: Option[(BigInt, Int)] = None
      var requests = 0; var writes = 0
      def tick(stall: Boolean = true): Unit = {
        val ready = !stall || cycle % 3 != 0
        d.io.memory.read.ready.poke(ready.B)
        d.io.memory.write.ready.poke((!stall || cycle % 4 == 0).B)
        val responding = pending.exists(_._2 <= cycle)
        d.io.memory.response.valid.poke(responding.B)
        d.io.memory.response.bits.poke(pending.map { case (addr, _) => memory(addr).zipWithIndex.map { case (v, i) => v << (32 * i) }.reduce(_ | _) }.getOrElse(BigInt(0)).U)
        val readFire = ready && d.io.memory.read.valid.peek().litToBoolean
        val responseFire = responding && d.io.memory.response.ready.peek().litToBoolean
        val writeFire = d.io.memory.write.ready.peek().litToBoolean && d.io.memory.write.valid.peek().litToBoolean
        if (responseFire) pending = None
        if (readFire) {
          assert(pending.isEmpty, "more than one outstanding local read")
          val addr = d.io.memory.read.bits.peek().litValue
          assert(memory.contains(addr), s"read uninitialized address ${addr.toString(16)}")
          pending = Some((addr, cycle + (if(stall) 2 + cycle % 5 else 1)))
          requests += 1
        }
        if (writeFire) {
          val addr = d.io.memory.write.bits.addr.peek().litValue
          val data = d.io.memory.write.bits.data.peek().litValue
          memory(addr) = Vector.tabulate(16)(i => (data >> (32 * i)) & BigInt("ffffffff", 16))
          writes += 1
        }
        d.clock.step(); cycle += 1
      }
      def start(s: BigInt, v: BigInt, rows: Int, cols: Int, mode: Int = 0, strideS: Int = 1, strideV: Int = 1,
          out: Int = 3072, configured: Boolean = true): Unit = {
        assert(d.io.start.ready.peek().litToBoolean)
        d.io.start.bits.s.poke(s.U); d.io.start.bits.v.poke(v.U)
        d.io.start.bits.biasS.poke(3000.U); d.io.start.bits.biasV.poke(3001.U)
        d.io.start.bits.strideS.poke(strideS.U); d.io.start.bits.strideV.poke(strideV.U)
        d.io.start.bits.out.poke(out.U); d.io.start.bits.rows.poke(rows.U); d.io.start.bits.cols.poke(cols.U)
        d.io.start.bits.mode.poke(mode.U); d.io.start.bits.configured.poke(configured.B)
        d.io.start.valid.poke(true.B); tick(); d.io.start.valid.poke(false.B)
      }
      def finish(stall: Boolean = true): Int = {
        val begin = cycle
        while(d.io.busy.peek().litToBoolean && cycle - begin < 200000) tick(stall)
        assert(cycle - begin < 200000, "engine failed to drain")
        assert(pending.isEmpty)
        cycle - begin
      }
      val biasS = Vector.tabulate(16)(c => (c - 7) * 0.015625f)
      val biasV = Vector.tabulate(16)(c => (c - 4) * -0.0625f)
      memory(3000) = biasS.map(bits); memory(3001) = biasV.map(bits)
      val sBase = BigInt("80000000", 16); val vBase = BigInt("80000200", 16)
      for ((rows, cols, stride, stall) <- Seq((1, 16, 1, false), (3, 5, 2, true), (17, 16, 1, true), (2, 16, 1, false))) {
        val scores = Vector.tabulate(rows, 16)((e, c) => if(c == 0 && e > 0) -110.0f else ((e * 13 + c * 7) % 37 - 18) * 0.0625f)
        val values = Vector.tabulate(rows, 16)((e, c) => ((e * 29 + c * 17) % 53 - 26) * 0.03125f)
        for (e <- 0 until rows) { memory(sBase + e * stride) = scores(e).map(bits); memory(vBase + e * stride) = values(e).map(bits) }
        val expected = Vector.tabulate(16) { c =>
          if(c >= cols) 0.0f else {
            var m = scala.Float.NegativeInfinity
            for(e <- 0 until rows) { val x = scores(e)(c) + biasS(c); if(x > m) m = x }
            val u = Vector.tabulate(rows)(e => exp((scores(e)(c) + biasS(c)) - m))
            var denominator = 0.0f; for(e <- 0 until rows) denominator = denominator + u(e)
            var y = 0.0f; for(e <- 0 until rows) { val a = u(e) / denominator; val v = values(e)(c) + biasV(c); val p = v * a; y = y + p }
            y
          }
        }
        val previousReads = requests; val previousWrites = writes
        start(sBase, vBase, rows, cols, strideS = stride, strideV = stride)
        val cycles = finish(stall)
        assert(!d.io.error.peek().litToBoolean)
        assert(requests - previousReads == 2 + 4 * rows)
        assert(writes - previousWrites == rows + 1)
        for(c <- 0 until 16) assert(equal(memory(3072)(c), expected(c)), s"rows=$rows c=$c got=${memory(3072)(c).toString(16)} expected=${bits(expected(c)).toString(16)}")
        println(s"SoftAgg rows=$rows cols=$cols stride=$stride stalled=$stall command_cycles=$cycles reads=${requests-previousReads} writes=${writes-previousWrites}")
      }
      // Residual: H is broadcast from ACC; X rows remain in SP. Tails are zero.
      val h = Vector.tabulate(16)(c => c * 0.5f)
      memory(sBase) = h.map(bits)
      for(e <- 0 until 3) memory(100 + e) = Vector.tabulate(16)(c => bits((e - c) * 0.25f))
      start(sBase, 100, 3, 7, mode = 1, strideS = 0)
      finish()
      for(e <- 0 until 3; c <- 0 until 16) {
        val expected = if(c < 7) (e - c) * 0.25f + (h(c) + biasS(c)) else 0.0f
        assert(equal(memory(3072 + e)(c), expected))
      }
      val previousReads = requests; val previousWrites = writes
      for(invalid <- 0 until 5) {
        invalid match {
          case 0 => start(sBase, vBase, 0, 16)
          case 1 => start(sBase + 1023, vBase, 2, 16)
          case 2 => start(sBase, sBase, 1, 16)
          case 3 => start(sBase, vBase, 1, 17)
          case 4 => start(sBase, vBase, 1, 16, configured = false)
        }
        finish(); assert(d.io.error.peek().litToBoolean)
        assert(requests == previousReads && writes == previousWrites)
      }
      // Error is the last RUN result, not a permanently latched failure.
      start(sBase, 100, 1, 16, mode = 1, strideS = 0)
      finish(); assert(!d.io.error.peek().litToBoolean)
    }
  }
}
