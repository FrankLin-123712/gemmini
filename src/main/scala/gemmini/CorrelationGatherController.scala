package gemmini

import chisel3._
import chisel3.util._

class CorrelationGatherDescriptor(val addrBits: Int, val spAddrBits: Int) extends Bundle {
  val bases = Vec(2, UInt(addrBits.W))
  val heights = Vec(2, UInt(32.W))
  val widths = Vec(2, UInt(32.W))
  val originX = Vec(2, SInt(32.W))
  val originY = Vec(2, SInt(32.W))
  val bBase = UInt(spAddrBits.W)
}

class CorrelationGatherReadRequest(val addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val token = UInt(5.W)
  val bytes = UInt(6.W)
}

class CorrelationGatherStats extends Bundle {
  val cycles = UInt(64.W)
  val usefulBytes = UInt(64.W)
  val strips = UInt(64.W)
  val rows = UInt(64.W)
  val readStall = UInt(64.W)
  val writeStall = UInt(64.W)
}

/** Fixed first-stage FP32 gather: C=128, two 8x8 windows, DIM=16.
  * Address order is channel -> level -> window row. Destination rows use the
  * existing LoopMatmul tile layout, not linear CHW or B row-major storage.
  * The enclosing controller owns serialization, VA translation and SRAM ports.
  */
class CorrelationGatherController(addrBits: Int, spAddrBits: Int, entries: Int = 16) extends Module {
  require(addrBits >= 32 && addrBits <= 64)
  require(spAddrBits >= 10)
  require(entries > 0 && entries <= 16 && isPow2(entries))
  val io = IO(new Bundle {
    val start = Flipped(Decoupled(new CorrelationGatherDescriptor(addrBits, spAddrBits)))
    val readReq = Decoupled(new CorrelationGatherReadRequest(addrBits))
    val readResp = Flipped(Decoupled(new CorrelationGatherReadResponse))
    val write = Decoupled(new CorrelationGatherWrite(spAddrBits))
    val abort = Input(Bool())
    // Must include DMA transactions, BeatMerger responses and response queues.
    val memoryBusy = Input(Bool())
    val busy = Output(Bool())
    val done = Valid(Bool()) // bits=true: failed/aborted, never publish B ready.
    val error = Output(Bool())
    val stats = Output(new CorrelationGatherStats)
  })

  val context = Reg(new CorrelationGatherDescriptor(addrBits, spAddrBits))
  val active = RegInit(false.B)
  val aborting = RegInit(false.B)
  val error = RegInit(false.B)
  val done = RegInit(false.B)
  val stats = RegInit(0.U.asTypeOf(new CorrelationGatherStats))
  val allocate :: firstStrip :: secondStrip :: drain :: Nil = Enum(4)
  val state = RegInit(allocate)
  val channel = RegInit(0.U(7.W))
  val level = RegInit(0.U(1.W))
  val pair = RegInit(0.U(2.W))
  val token = Reg(UInt(5.W))
  val requestAddresses = Reg(Vec(2, UInt(addrBits.W)))
  val requestBytes = Reg(Vec(2, UInt(6.W)))
  // Precomputed strides avoid a channel*H*W multiply on every request.
  val rowStride = Reg(Vec(2, UInt(18.W)))
  val channelStride = Reg(Vec(2, UInt(34.W)))
  val channelBase = Reg(Vec(2, UInt(addrBits.W)))

  val packer = Module(new CorrelationRowPacker(spAddrBits, entries))
  val stopping = aborting || io.abort || packer.io.fault
  io.start.ready := !active && !io.memoryBusy && !io.abort
  io.busy := active
  io.done.valid := done
  io.done.bits := error
  io.error := error
  io.stats := stats
  done := false.B

  // No token is re-used after abort until the entire reader has drained.
  packer.io.clear := io.abort || aborting || !active
  packer.io.response.valid := io.readResp.valid && active && !aborting && !io.abort
  packer.io.response.bits := io.readResp.bits
  io.readResp.ready := true.B
  io.write <> packer.io.write

  // Extend signed origins *before* adding 8, avoiding INT32_MAX wraparound.
  val bx = context.originX(level).pad(34)
  val by = context.originY(level).pad(34)
  val width = context.widths(level).zext.pad(34)
  val height = context.heights(level).zext.pad(34)
  val xBegin = Mux(bx < 0.S, 0.S(34.W), bx)
  val xEnd = Mux(bx + 8.S < width, bx + 8.S, width)
  val xValid = xEnd > xBegin
  val count = Mux(xValid, (xEnd - xBegin).asUInt, 0.U)(3, 0)
  val leftPad = Mux(xValid, (xBegin - bx).asUInt, 0.U)(3, 0) << 2
  val bytes = count << 2
  val stripBytes = Wire(Vec(2, UInt(6.W)))
  val stripAddresses = Wire(Vec(2, UInt(addrBits.W)))
  val stripMasks = Wire(Vec(2, UInt(32.W)))
  for (half <- 0 until 2) {
    val y = by + (Cat(pair, half.U(1.W))).zext
    val legal = xValid && y >= 0.S && y < height
    stripBytes(half) := Mux(legal, bytes, 0.U)
    // Only valid y/x participate in address arithmetic. Base range is checked
    // once at submission; every accepted request lies inside that tensor.
    val safeY = Mux(legal, y.asUInt, 0.U)(15, 0)
    val safeX = Mux(legal, xBegin.asUInt, 0.U)(15, 0)
    stripAddresses(half) := channelBase(level) + safeY * rowStride(level) + (safeX << 2)
    val lowMask = ((1.U(33.W) << stripBytes(half)) - 1.U)(31, 0)
    stripMasks(half) := (lowMask << leftPad)(31, 0)
  }
  // ((c/16)*8 + level*4 + gy/2)*16 + c%16, for even gy.
  val nativeRow = Cat(channel(6, 4), level, pair, channel(3, 0))
  packer.io.allocate.valid := active && !stopping && state === allocate
  packer.io.allocate.bits.addr := context.bBase + nativeRow
  packer.io.allocate.bits.expected := Cat(stripMasks(1), stripMasks(0))
  packer.io.allocate.bits.leftPad := VecInit(Seq.fill(2)(leftPad))

  when(packer.io.allocate.fire) {
    token := packer.io.allocationId << 1
    requestAddresses := stripAddresses
    requestBytes := stripBytes
    state := firstStrip
  }
  val half = state === secondStrip
  io.readReq.valid := active && !stopping && (state === firstStrip || state === secondStrip) && requestBytes(half) =/= 0.U
  io.readReq.bits.addr := requestAddresses(half)
  io.readReq.bits.bytes := requestBytes(half)
  io.readReq.bits.token := token | half.asUInt
  val stripAdvanced = active && !stopping && (state === firstStrip || state === secondStrip) &&
    (requestBytes(half) === 0.U || io.readReq.fire)
  when(stripAdvanced) {
    when(state === firstStrip) {
      state := secondStrip
    }.otherwise {
      when(channel === 127.U && level === 1.U && pair === 3.U) {
        state := drain
      }.otherwise {
        state := allocate
        pair := pair + 1.U
        when(pair === 3.U) {
          level := level + 1.U
          when(level === 1.U) {
            channel := channel + 1.U
            for (l <- 0 until 2) { channelBase(l) := channelBase(l) + channelStride(l) }
          }
        }
      }
    }
  }

  when(active) {
    stats.cycles := stats.cycles + 1.U
    when(io.readReq.fire) {
      stats.strips := stats.strips + 1.U
      stats.usefulBytes := stats.usefulBytes + io.readReq.bits.bytes
    }
    when(io.write.fire) { stats.rows := stats.rows + 1.U }
    when(io.readReq.valid && !io.readReq.ready) { stats.readStall := stats.readStall + 1.U }
    when(io.write.valid && !io.write.ready) { stats.writeStall := stats.writeStall + 1.U }
    when(io.abort || packer.io.fault) {
      aborting := true.B
      error := true.B
    }
    when(aborting && !io.memoryBusy && !io.readResp.valid) {
      active := false.B
      aborting := false.B
      done := true.B
    }
    when(!stopping && state === drain && packer.io.empty && !io.memoryBusy && !io.readResp.valid) {
      assert(stats.rows === 1024.U, "correlation gather completed before all SPAD writes")
      active := false.B
      done := true.B
    }
  }

  // Validate the complete tensor address ranges using one extra carry bit.
  val invalidShape = (0 until 2).map(l => io.start.bits.heights(l) === 0.U ||
    io.start.bits.widths(l) === 0.U || io.start.bits.heights(l) > 65535.U || io.start.bits.widths(l) > 65535.U).reduce(_ || _)
  val invalidAddress = (0 until 2).map { l =>
    val tensorBytes = io.start.bits.heights(l) * io.start.bits.widths(l) * 512.U
    val end = io.start.bits.bases(l) +& tensorBytes
    io.start.bits.bases(l)(1, 0).orR || (end >> addrBits).orR
  }.reduce(_ || _)
  val spadEnd = io.start.bits.bBase +& 1023.U
  val invalidBase = io.start.bits.bBase(9, 0).orR || spadEnd(spAddrBits)
  when(io.start.fire) {
    context := io.start.bits
    stats := 0.U.asTypeOf(new CorrelationGatherStats)
    channel := 0.U
    level := 0.U
    pair := 0.U
    state := allocate
    aborting := false.B
    error := invalidShape || invalidAddress || invalidBase
    active := !(invalidShape || invalidAddress || invalidBase)
    done := invalidShape || invalidAddress || invalidBase
    for (l <- 0 until 2) {
      rowStride(l) := io.start.bits.widths(l) << 2
      channelStride(l) := (io.start.bits.heights(l) * io.start.bits.widths(l)) << 2
      channelBase(l) := io.start.bits.bases(l)
    }
  }
}
