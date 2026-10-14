package gemmini

import chisel3._
import chisel3.util._
import hardfloat._

class SoftAggDescriptor extends Bundle {
  val s = UInt(32.W)
  val v = UInt(32.W)
  val biasS = UInt(32.W)
  val biasV = UInt(32.W)
  val strideS = UInt(32.W)
  val strideV = UInt(32.W)
  val out = UInt(32.W)
  val rows = UInt(16.W)
  val cols = UInt(8.W)
  val mode = UInt(8.W)
  val configured = Bool()
}

class SoftAggWrite extends Bundle {
  val addr = UInt(32.W)
  val data = UInt(512.W)
}

/** Exclusive local-memory client. The controller drains normal commands before
  * granting ownership; exactly one read is outstanding. bit31 selects ACC.
  */
class SoftAggMemoryIO extends Bundle {
  val active = Output(Bool())
  val read = Decoupled(UInt(32.W))
  val response = Flipped(Decoupled(UInt(512.W)))
  val write = Decoupled(new SoftAggWrite)
}

/** Four registered FP32 lanes. Each instruction rounds once; the exp polynomial
  * uses FMA, while SoftAgg normalization uses separate divide, multiply and add.
  * Divider results are captured independently because special inputs can finish
  * at different cycles. An output remains stable until accepted.
  */
class SoftAggArithmetic(lanes: Int = 4) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new Bundle {
      val op = UInt(3.W) // 0 add, 1 sub, 2 mul, 3 fma, 4 div
      val a = Vec(lanes, UInt(32.W))
      val b = Vec(lanes, UInt(32.W))
      val c = Vec(lanes, UInt(32.W))
    }))
    val out = Decoupled(Vec(lanes, UInt(32.W)))
  })
  val idle :: execute :: divide :: output :: Nil = Enum(4)
  val state = RegInit(idle)
  val req = Reg(chiselTypeOf(io.in.bits))
  val result = Reg(Vec(lanes, UInt(32.W)))
  val done = RegInit(VecInit(Seq.fill(lanes)(false.B)))
  io.in.ready := state === idle
  io.out.valid := state === output
  io.out.bits := result
  when(io.in.fire) { req := io.in.bits; done.foreach(_ := false.B); state := execute }
  when(io.out.fire) { state := idle }
  val divReady = Wire(Vec(lanes, Bool()))
  for (i <- 0 until lanes) {
    val fma = Module(new MulAddRecFN(8, 24))
    val isAdd = req.op === 0.U || req.op === 1.U
    val a = req.a(i)
    val b = Mux(isAdd, "h3f800000".U, req.b(i))
    // Multiplication's zero addend has product sign, preserving signed zero.
    val signedZero = Cat(req.a(i)(31) ^ req.b(i)(31), 0.U(31.W))
    val c = Mux(isAdd, req.b(i), Mux(req.op === 2.U, signedZero, req.c(i)))
    fma.io.a := recFNFromFN(8, 24, a)
    fma.io.b := recFNFromFN(8, 24, b)
    fma.io.c := recFNFromFN(8, 24, c)
    fma.io.op := Mux(req.op === 1.U, 1.U, 0.U)
    fma.io.roundingMode := consts.round_near_even
    fma.io.detectTininess := consts.tininess_afterRounding
    val div = Module(new DivSqrtRecFN_small(8, 24, 0))
    divReady(i) := div.io.inReady
    div.io.inValid := state === execute && req.op === 4.U && divReady.asUInt.andR
    div.io.sqrtOp := false.B
    div.io.a := recFNFromFN(8, 24, req.a(i))
    div.io.b := recFNFromFN(8, 24, req.b(i))
    div.io.roundingMode := consts.round_near_even
    div.io.detectTininess := consts.tininess_afterRounding
    when(state === execute && req.op =/= 4.U) { result(i) := fNFromRecFN(8, 24, fma.io.out) }
    when(div.io.outValid_div) { result(i) := fNFromRecFN(8, 24, div.io.out); done(i) := true.B }
  }
  when(state === execute) {
    when(req.op =/= 4.U) { state := output }
      .elsewhen(divReady.asUInt.andR) { state := divide }
  }
  when(state === divide && done.asUInt.andR) { state := output }
}

/** SoftAgg v1: three complete, ordered scans of S/V. S is overwritten by u;
  * m/d/y and one row are registers, with no additional bulk SRAM. Sources may
  * be SP or ACC; final outputs are SP. No DMA or CPU arithmetic is hidden here.
  * Rows/channels retain their original order; invalid channel tails write zero.
  */
class SoftAgg(spRows: Int, accRows: Int, drainCycles: Int = 6) extends Module {
  val io = IO(new Bundle {
    val start = Flipped(Decoupled(new SoftAggDescriptor))
    val memory = new SoftAggMemoryIO
    val busy = Output(Bool())
    val done = Valid(Bool()) // error; emitted only after the existing write drain
    val error = Output(Bool())
    val commands = Output(UInt(64.W))
    val readRows = Output(UInt(64.W))
    val writeRows = Output(UInt(64.W))
  })
  val idle :: request :: response :: prepare :: arithmetic :: finishChunk :: write :: drain :: Nil = Enum(8)
  val state = RegInit(idle)
  val desc = Reg(new SoftAggDescriptor)
  val error = RegInit(false.B)
  val done = RegInit(false.B)
  val counters = RegInit(VecInit(Seq.fill(3)(0.U(64.W))))
  io.commands := counters(0); io.readRows := counters(1); io.writeRows := counters(2)
  io.error := error
  io.done.valid := done
  io.done.bits := error
  done := false.B
  io.busy := state =/= idle
  io.start.ready := state === idle
  io.memory.active := io.busy

  val biasS = Reg(Vec(16, UInt(32.W)))
  val biasV = Reg(Vec(16, UInt(32.W)))
  val maximum = Reg(Vec(16, UInt(32.W)))
  val denominator = Reg(Vec(16, UInt(32.W)))
  val y = Reg(Vec(16, UInt(32.W)))
  val rowS = Reg(Vec(16, UInt(32.W)))
  val rowV = Reg(Vec(16, UInt(32.W)))
  val writeRow = Reg(Vec(16, UInt(32.W)))
  val edge = RegInit(0.U(16.W))
  val chunk = RegInit(0.U(2.W))
  val pass = RegInit(0.U(2.W)) // 0 max, 1 exp/sum, 2 weighted sum, 3 add
  val readKind = RegInit(0.U(2.W)) // 0 biasS, 1 biasV, 2 S, 3 V
  val pc = RegInit(0.U(5.W))
  val saved = Reg(Vec(4, Vec(8, UInt(32.W))))
  val drainCount = RegInit(0.U(log2Ceil(drainCycles + 1).W))
  val fpu = Module(new SoftAggArithmetic)
  private def fp(x: scala.Float): UInt = (java.lang.Float.floatToRawIntBits(x).toLong & 0xffffffffL).U(32.W)
  private def greater(a: UInt, b: UInt): Bool = {
    val nanA = a(30, 23).andR && a(22, 0).orR
    val nanB = b(30, 23).andR && b(22, 0).orR
    val zeros = !a(30, 0).orR && !b(30, 0).orR
    !nanA && !nanB && !zeros && Mux(a(31) =/= b(31), !a(31), Mux(a(31), a < b, a > b))
  }
  val sAddr = desc.s + edge * desc.strideS
  val vAddr = desc.v + edge * desc.strideV
  io.memory.read.valid := state === request
  io.memory.read.bits := MuxLookup(readKind, sAddr)(Seq(0.U -> desc.biasS, 1.U -> desc.biasV, 3.U -> vAddr))
  io.memory.response.ready := state === response
  io.memory.write.valid := state === write
  io.memory.write.bits.addr := Mux(pass === 1.U, sAddr, desc.out + Mux(pass === 3.U, edge, 0.U))
  io.memory.write.bits.data := writeRow.asUInt
  fpu.io.in.valid := state === prepare
  fpu.io.in.bits.op := 0.U
  fpu.io.in.bits.a := 0.U.asTypeOf(fpu.io.in.bits.a)
  fpu.io.in.bits.b := 0.U.asTypeOf(fpu.io.in.bits.b)
  fpu.io.in.bits.c := 0.U.asTypeOf(fpu.io.in.bits.c)
  fpu.io.out.ready := state === arithmetic

  // Address checks use wide multiplication and check every live interval before
  // any memory request. Conservative intervals deliberately reject strided aliases.
  val d = io.start.bits
  def end(base: UInt, count: UInt, stride: UInt): UInt = base(30, 0).pad(64) + (count - 1.U).pad(64) * stride
  def valid(base: UInt, count: UInt, stride: UInt): Bool = end(base, count, stride) < Mux(base(31), accRows.U, spRows.U)
  def alias(a: UInt, an: UInt, astride: UInt, b: UInt, bn: UInt, bstride: UInt): Bool =
    a(31) === b(31) && a(30, 0) <= end(b, bn, bstride) && b(30, 0) <= end(a, an, astride)
  val outputRows = Mux(d.mode === 0.U, 1.U, d.rows)
  val legal = d.configured && d.mode <= 1.U && d.rows =/= 0.U && d.cols =/= 0.U && d.cols <= 16.U &&
    valid(d.s, d.rows, d.strideS) && valid(d.v, d.rows, d.strideV) &&
    valid(d.biasS, 1.U, 0.U) && valid(d.biasV, 1.U, 0.U) && !d.out(31) && valid(d.out, outputRows, 1.U) &&
    !alias(d.out, outputRows, 1.U, d.s, d.rows, d.strideS) &&
    !alias(d.out, outputRows, 1.U, d.v, d.rows, d.strideV) &&
    !alias(d.out, outputRows, 1.U, d.biasS, 1.U, 0.U) && !alias(d.out, outputRows, 1.U, d.biasV, 1.U, 0.U) &&
    (d.mode === 1.U || (d.strideS =/= 0.U && !alias(d.s, d.rows, d.strideS, d.v, d.rows, d.strideV) &&
      !alias(d.s, d.rows, d.strideS, d.biasS, 1.U, 0.U) && !alias(d.s, d.rows, d.strideS, d.biasV, 1.U, 0.U)))
  when(io.start.fire) {
    done := !legal
    error := !legal
    when(legal) {
      desc := d; edge := 0.U; chunk := 0.U; pc := 0.U; readKind := 0.U
      pass := Mux(d.mode === 1.U, 3.U, 0.U); state := request
      counters(0) := counters(0) + 1.U
      for (i <- 0 until 16) { maximum(i) := "hff800000".U; denominator(i) := 0.U; y(i) := 0.U; writeRow(i) := 0.U }
    }
  }
  when(io.memory.read.fire) { counters(1) := counters(1) + 1.U; state := response }
  when(io.memory.response.fire) {
    val data = io.memory.response.bits.asTypeOf(Vec(16, UInt(32.W)))
    switch(readKind) {
      is(0.U) { biasS := data; readKind := 1.U; state := request }
      is(1.U) { biasV := data; readKind := 2.U; state := request }
      is(2.U) {
        rowS := data
        when(pass === 2.U || pass === 3.U) { readKind := 3.U; state := request }
          .otherwise { chunk := 0.U; pc := 0.U; state := prepare }
      }
      is(3.U) { rowV := data; chunk := 0.U; pc := 0.U; state := prepare }
    }
  }

  // Microprogram register names: saved[0]=x/u, [1]=m, [2]=biased,
  // [3]=polynomial/normalized weight, [4]=overflow, [5]=normal, [6]=value.
  for (l <- 0 until 4) {
    val c = Cat(chunk, l.U(2.W))
    val r = saved(l)
    val a = WireDefault(rowS(c)); val b = WireDefault(biasS(c)); val z = WireDefault(0.U(32.W))
    val op = WireDefault(0.U(3.W))
    when(pass === 1.U) {
      switch(pc) {
        is(1.U) { a := r(0); b := maximum(c); op := 1.U }
        is(2.U) { a := Mux(greater(fp(-103.9720840454f), r(0)), fp(-103.9720840454f), Mux(greater(r(0), fp(88.7762626647950f)), fp(88.7762626647950f), r(0))); b := fp(1.44269504088896341f); z := fp(12582912.0f); op := 3.U }
        is(3.U) { a := r(2); b := fp(12582912.0f); op := 1.U }
        is(4.U) { a := r(1); b := fp(-6.93145752e-1f); z := r(0); op := 3.U }
        is(5.U) { a := r(1); b := fp(-1.42860677e-6f); z := r(0); op := 3.U }
        is(6.U) { a := fp(java.lang.Float.intBitsToFloat(0x3ab4a000)); b := r(0); z := fp(java.lang.Float.intBitsToFloat(0x3c092f6e)); op := 3.U }
        is(7.U) { a := r(3); b := r(0); z := fp(java.lang.Float.intBitsToFloat(0x3d2aadad)); op := 3.U }
        is(8.U) { a := r(3); b := r(0); z := fp(java.lang.Float.intBitsToFloat(0x3e2aaa28)); op := 3.U }
        is(9.U) { a := r(3); b := r(0); z := fp(java.lang.Float.intBitsToFloat(0x3efffffb)); op := 3.U }
        is(10.U) { a := r(3); b := r(0); z := fp(1.0f); op := 3.U }
        is(11.U) { a := r(0); b := r(4); op := 2.U }
        is(12.U) { a := r(3); b := r(0); z := r(4); op := 3.U }
        is(13.U) { a := r(3); b := r(5); op := 2.U }
        is(14.U) { a := denominator(c); b := r(0) }
      }
    }
    when(pass === 2.U) {
      switch(pc) {
        is(0.U) { a := rowS(c); b := denominator(c); op := 4.U }
        is(1.U) { a := rowV(c); b := biasV(c) }
        is(2.U) { a := r(6); b := r(3); op := 2.U }
        is(3.U) { a := y(c); b := r(0) }
      }
    }
    when(pass === 3.U && pc === 1.U) { a := rowV(c); b := r(0) }
    fpu.io.in.bits.a(l) := a; fpu.io.in.bits.b(l) := b; fpu.io.in.bits.c(l) := z
    if(l == 0) { fpu.io.in.bits.op := op }
    when(fpu.io.out.fire) {
      val value = fpu.io.out.bits(l)
      when(pass === 0.U) { when(c < desc.cols && greater(value, maximum(c))) { maximum(c) := value } }
      when(pass === 1.U) {
        switch(pc) {
          is(0.U) { r(0) := value }
          is(1.U) { r(0) := Mux(greater(fp(-103.9720840454f), value), fp(-103.9720840454f), Mux(greater(value, fp(88.7762626647950f)), fp(88.7762626647950f), value)) }
          is(2.U) {
            r(2) := value
            val exponent = (value << 23)(31, 0).asSInt
            val normal = Mux(exponent < "hc1000000".U(32.W).asSInt, "hc1000000".U, Mux(exponent > "h3f800000".U(32.W).asSInt, "h3f800000".U, exponent.asUInt))
            r(4) := exponent.asUInt - normal + "h3f800000".U
            r(5) := normal + "h3f800000".U
          }
          is(3.U) { r(1) := value }
          is(4.U) { r(0) := value }
          is(5.U) { r(0) := value }
          is(6.U) { r(3) := value }
          is(7.U) { r(3) := value }
          is(8.U) { r(3) := value }
          is(9.U) { r(3) := value }
          is(10.U) { r(3) := value }
          is(11.U) { r(0) := value }
          is(12.U) { r(3) := value }
          is(13.U) { r(0) := value; writeRow(c) := Mux(c < desc.cols, value, 0.U) }
          is(14.U) { when(c < desc.cols) { denominator(c) := value } }
        }
      }
      when(pass === 2.U) {
        switch(pc) {
          is(0.U) { r(3) := value }
          is(1.U) { r(6) := value }
          is(2.U) { r(0) := value }
          is(3.U) { when(c < desc.cols) { y(c) := value }; writeRow(c) := Mux(c < desc.cols, value, 0.U) }
        }
      }
      when(pass === 3.U) {
        when(pc === 0.U) { r(0) := value }.otherwise { writeRow(c) := Mux(c < desc.cols, value, 0.U) }
      }
    }
  }
  when(fpu.io.in.fire) { state := arithmetic }
  when(fpu.io.out.fire) {
    val last = pass === 0.U || (pass === 1.U && pc === 14.U) ||
      (pass === 2.U && pc === 3.U) || (pass === 3.U && pc === 1.U)
    pc := pc + 1.U
    state := Mux(last, finishChunk, prepare)
  }
  def nextRow(): Unit = {
    edge := edge + 1.U; readKind := 2.U; state := request
    when(edge + 1.U === desc.rows) {
      edge := 0.U
      when(pass === 0.U) { pass := 1.U }
      when(pass === 1.U) { pass := 2.U }
    }
  }
  when(state === finishChunk) {
    pc := 0.U; chunk := chunk + 1.U
    when(chunk =/= 3.U) { state := prepare }
      .elsewhen(pass === 1.U || pass === 3.U || (pass === 2.U && edge + 1.U === desc.rows)) { state := write }
      .otherwise { nextRow() }
  }
  when(io.memory.write.fire) {
    counters(2) := counters(2) + 1.U
    when(pass === 2.U || (pass === 3.U && edge + 1.U === desc.rows)) {
      state := drain; drainCount := drainCycles.U
    }.otherwise { nextRow() }
  }
  when(state === drain) {
    drainCount := drainCount - 1.U
    when(drainCount === 1.U) { state := idle; done := true.B }
  }
}
