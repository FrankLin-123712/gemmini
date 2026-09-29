package gemmini

import chisel3._
import chisel3.util._

class CorrelationGatherReadResponse extends Bundle {
  // A token identifies one of two strips belonging to an assembly entry.
  val token = UInt(5.W)
  // DMA preserves the offset *within the requested strip*, including split Gets.
  val data = UInt(512.W)
  val mask = UInt(64.W)
  val bytesRead = UInt(8.W)
}

class CorrelationGatherWrite(val spAddrBits: Int) extends Bundle {
  val addr = UInt(spAddrBits.W)
  val data = UInt(512.W)
}

class CorrelationRowAllocation(val spAddrBits: Int) extends Bundle {
  val addr = UInt(spAddrBits.W)
  // Set bits denote bytes which need a DMA response; all other bytes are zero.
  val expected = UInt(64.W)
  val leftPad = Vec(2, UInt(6.W))
}

/** Sixteen 64-byte assembly entries, separate from the DMA transaction credits.
  * Entries are released only when their complete row is accepted by the SPAD.
  * readResp masks can describe split and out-of-order fragments. No FP arithmetic
  * or numeric conversion is performed here.
  */
class CorrelationRowPacker(spAddrBits: Int, entries: Int = 16) extends Module {
  require(entries > 0 && entries <= 16 && isPow2(entries))
  private val indexBits = log2Ceil(entries) max 1
  val io = IO(new Bundle {
    val allocate = Flipped(Decoupled(new CorrelationRowAllocation(spAddrBits)))
    val allocationId = Output(UInt(indexBits.W))
    val response = Flipped(Decoupled(new CorrelationGatherReadResponse))
    val write = Decoupled(new CorrelationGatherWrite(spAddrBits))
    val clear = Input(Bool())
    val empty = Output(Bool())
    val fault = Output(Bool())
  })

  val live = RegInit(VecInit(Seq.fill(entries)(false.B)))
  val rowAddress = Reg(Vec(entries, UInt(spAddrBits.W)))
  val data = Reg(Vec(entries, UInt(512.W)))
  val missing = Reg(Vec(entries, UInt(64.W)))
  val leftPad = Reg(Vec(entries, Vec(2, UInt(6.W))))

  val available = ~live.asUInt
  val freeId = PriorityEncoder(available)
  io.allocate.ready := available.orR && !io.clear
  io.allocationId := freeId
  io.empty := !live.asUInt.orR

  // Lock the selected row when the SRAM writer stalls. Another entry becoming
  // ready must not change the address/data of the pending write.
  val locked = RegInit(false.B)
  val lockedId = Reg(UInt(indexBits.W))
  val complete = VecInit((0 until entries).map(i => live(i) && !missing(i).orR)).asUInt
  val writeId = Mux(locked, lockedId, PriorityEncoder(complete))
  io.write.valid := complete.orR && !io.clear
  io.write.bits.addr := rowAddress(writeId)
  io.write.bits.data := data(writeId)
  when(io.write.valid && !io.write.ready) {
    locked := true.B
    lockedId := writeId
  }
  when(io.write.fire) {
    live(writeId) := false.B
    locked := false.B
  }

  val responseIndex = io.response.bits.token >> 1
  val responseId = responseIndex(indexBits - 1, 0)
  val half = io.response.bits.token(0)
  val offset = Cat(half, 0.U(5.W)) +& leftPad(responseId)(half)
  val placedMask = (io.response.bits.mask << offset)(63, 0)
  val placedData = (io.response.bits.data << (offset << 3))(511, 0)
  // A malformed or duplicate fragment aborts the command rather than publishing
  // a partially valid B. The normal reader never emits duplicate byte masks.
  val fragmentValid = responseIndex < entries.U && live(responseId) &&
    io.response.bits.mask.orR && !io.response.bits.mask(63, 32).orR &&
    PopCount(placedMask) === PopCount(io.response.bits.mask) &&
    placedMask.orR && (placedMask & ~missing(responseId)) === 0.U
  io.response.ready := true.B // The entry storage itself accepts one beat/cycle.
  io.fault := io.response.fire && !io.clear && !fragmentValid

  when(io.response.fire && !io.clear && fragmentValid) {
    val bytes = Wire(Vec(64, UInt(8.W)))
    for (b <- 0 until 64) {
      bytes(b) := Mux(placedMask(b), placedData(8 * b + 7, 8 * b), data(responseId)(8 * b + 7, 8 * b))
    }
    data(responseId) := bytes.asUInt
    missing(responseId) := missing(responseId) & ~placedMask
  }

  when(io.allocate.fire) {
    live(freeId) := true.B
    rowAddress(freeId) := io.allocate.bits.addr
    data(freeId) := 0.U
    missing(freeId) := io.allocate.bits.expected
    leftPad(freeId) := io.allocate.bits.leftPad
  }
  when(io.clear) {
    live.foreach(_ := false.B)
    locked := false.B
  }
}
