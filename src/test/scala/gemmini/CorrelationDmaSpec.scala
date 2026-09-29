package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tile.{RocketTileParams, TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._

class CorrelationDmaFragment extends Bundle {
  val token = UInt(5.W)
  val sourceShift = UInt(6.W)
  val stripOffset = UInt(6.W)
  val bytes = UInt(6.W)
  val data = UInt(512.W)
}

/** Actual BeatMerger -> RowPacker adapter, using the same byte masks and token
  * mapping as Scratchpad. The harness supplies transaction metadata that the
  * StreamReader normally reserves in XactTracker, not pre-merged strip data.
  */
class CorrelationDmaHarness extends Module {
  val io = IO(new Bundle {
    val allocate = Flipped(Decoupled(new CorrelationRowAllocation(12)))
    val allocationId = Output(UInt(4.W))
    val fragment = Flipped(Decoupled(new CorrelationDmaFragment))
    val write = Decoupled(new CorrelationGatherWrite(12))
    val clear = Input(Bool())
    val readerBusy = Output(Bool())
    val empty = Output(Bool())
    val fault = Output(Bool())
  })
  val merger = Module(new BeatMerger[UInt](512, 64, 512, 512, 4096, 1024, 64, 1, 16, 32, 2))
  val packer = Module(new CorrelationRowPacker(12))
  packer.io.allocate <> io.allocate
  io.allocationId := packer.io.allocationId
  packer.io.clear := io.clear
  io.write <> packer.io.write
  io.empty := packer.io.empty
  io.fault := packer.io.fault
  io.readerBusy := merger.io.busy

  merger.io.req.valid := io.fragment.valid
  merger.io.req.bits := 0.U.asTypeOf(merger.io.req.bits)
  merger.io.req.bits.addr := io.fragment.bits.token
  merger.io.req.bits.shift := io.fragment.bits.sourceShift
  merger.io.req.bits.spad_row_offset := io.fragment.bits.stripOffset
  merger.io.req.bits.bytes_to_read := io.fragment.bits.bytes
  merger.io.req.bits.lg_len_req := 6.U // One 64-byte TileLink beat.
  merger.io.in.valid := io.fragment.valid
  merger.io.in.bits := io.fragment.bits.data
  io.fragment.ready := merger.io.in.ready
  packer.io.response.valid := merger.io.out.valid
  merger.io.out.ready := packer.io.response.ready
  packer.io.response.bits.token := merger.io.out.bits.addr
  packer.io.response.bits.data := merger.io.out.bits.data
  packer.io.response.bits.mask := merger.io.out.bits.mask.asUInt
  packer.io.response.bits.bytesRead := RegEnable(io.fragment.bits.bytes, merger.io.req.fire)
}

class CorrelationDmaSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Correlation DMA adaptation"
  it should "preserve token and split-Get byte offsets through the actual BeatMerger" in {
    test(new CorrelationDmaHarness) { d =>
      d.io.allocate.valid.poke(false.B)
      d.io.fragment.valid.poke(false.B)
      d.io.write.ready.poke(false.B)
      d.io.clear.poke(false.B)
      d.clock.step()
      // First strip clips two FP32 values on the left; its 24 remaining bytes
      // cross a 64-byte Get boundary after 12 bytes. Second strip has 32 bytes.
      val mask0 = ((BigInt(1) << 24) - 1) << 8
      val mask1 = ((BigInt(1) << 32) - 1) << 32
      d.io.allocate.bits.addr.poke(42.U)
      d.io.allocate.bits.expected.poke((mask0 | mask1).U)
      d.io.allocate.bits.leftPad(0).poke(8.U)
      d.io.allocate.bits.leftPad(1).poke(0.U)
      d.io.allocate.valid.poke(true.B)
      d.io.allocate.ready.expect(true.B)
      val id = d.io.allocationId.peek().litValue.toInt
      d.clock.step()
      d.io.allocate.valid.poke(false.B)

      def fragment(half: Int, shift: Int, offset: Int, bytes: Int): Unit = {
        val base = if (half == 0) 8 else 32
        val raw = (0 until 64).map { i =>
          val b = if (i >= shift && i < shift + bytes) base + offset + i - shift + 1 else 0xa5
          BigInt(b) << (8 * i)
        }.foldLeft(BigInt(0))(_ | _)
        d.io.fragment.bits.token.poke((2 * id + half).U)
        d.io.fragment.bits.sourceShift.poke(shift.U)
        d.io.fragment.bits.stripOffset.poke(offset.U)
        d.io.fragment.bits.bytes.poke(bytes.U)
        d.io.fragment.bits.data.poke(raw.U)
        d.io.fragment.valid.poke(true.B)
        var timeout = 0
        while (!d.io.fragment.ready.peek().litToBoolean && timeout < 20) {
          d.io.fault.expect(false.B)
          d.clock.step()
          timeout += 1
        }
        assert(timeout < 20)
        d.clock.step()
        d.io.fragment.valid.poke(false.B)
        // Receiving the memory beat does not yet make the merger idle: its
        // pending output still has to be consumed by the packer.
        d.io.readerBusy.expect(true.B)
      }
      fragment(0, 0, 12, 12)  // Second Get returns before the first.
      fragment(1, 0, 16, 16)
      fragment(0, 52, 0, 12)
      d.io.write.valid.expect(false.B)
      fragment(1, 48, 0, 16)
      var timeout = 0
      while (!d.io.write.valid.peek().litToBoolean && timeout < 20) {
        d.io.fault.expect(false.B)
        d.clock.step()
        timeout += 1
      }
      assert(timeout < 20)
      val expected = (8 until 64).map(i => BigInt(i + 1) << (8 * i)).foldLeft(BigInt(0))(_ | _)
      for (_ <- 0 until 5) {
        d.io.write.valid.expect(true.B)
        d.io.write.bits.addr.expect(42.U)
        d.io.write.bits.data.expect(expected.U)
        d.io.empty.expect(false.B)
        d.io.fault.expect(false.B)
        d.clock.step()
      }
      d.io.readerBusy.expect(false.B)
      d.io.write.ready.poke(true.B)
      d.clock.step()
      d.io.empty.expect(true.B)
      d.io.write.valid.expect(false.B)
    }
  }
}


/** The real reader, including XactTracker, translation queues and BeatMerger.
  * Translation results and the external TileLink memory are driven by the test.
  */
class CorrelationStreamReaderHarness(implicit baseParameters: Parameters) extends LazyModule {
  private val visibility = TLEphemeralNode()
  private val readerParameters = baseParameters.alterPartial {
    case TileKey => RocketTileParams()
    case TileVisibilityNodeKey => visibility
  }
  val reader = LazyModule(new StreamReader(
    GemminiCustomConfigs.dpvoFP32Dim16CorrGatherConfig.copy(use_firesim_simulation_counters = false),
    16, 512, 64, 512, 512, 1, 4096, 1024, 16, false, false)(readerParameters))
  val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0, (BigInt(1) << 32) - 1)),
      supportsGet = TransferSizes(64, 64))), beatBytes = 64)))
  manager := visibility := reader.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (tl, edge) = manager.in(0)
    val io = IO(new Bundle {
      val request = Flipped(Decoupled(new CorrelationGatherReadRequest(40)))
      val response = Decoupled(new CorrelationGatherReadResponse)
      val a = Decoupled(chiselTypeOf(tl.a.bits))
      val d = Flipped(Decoupled(chiselTypeOf(tl.d.bits)))
      val cancel = Input(Bool())
      val miss = Input(Bool())
      val pageFault = Input(Bool())
      val busy = Output(Bool())
      val fault = Output(Bool())
    })
    io.a <> tl.a
    tl.d <> io.d
    tl.b.valid := false.B
    tl.b.bits := DontCare
    tl.c.ready := true.B
    tl.e.ready := true.B
    val r = reader.module.io
    r.req.valid := io.request.valid
    io.request.ready := r.req.ready
    r.req.bits := 0.U.asTypeOf(r.req.bits)
    r.req.bits.vaddr := io.request.bits.addr
    r.req.bits.spaddr := io.request.bits.token
    r.req.bits.len := io.request.bits.bytes >> 2
    r.req.bits.pixel_repeats := 1.U
    io.response.valid := r.resp.valid
    r.resp.ready := io.response.ready
    io.response.bits.token := r.resp.bits.addr
    io.response.bits.data := r.resp.bits.data
    io.response.bits.mask := r.resp.bits.mask.asUInt
    io.response.bits.bytesRead := r.resp.bits.bytes_read
    r.gather := true.B
    r.cancel := io.cancel
    r.flush := false.B
    r.tlb.resp := 0.U.asTypeOf(r.tlb.resp)
    r.tlb.resp.miss := io.miss
    r.tlb.resp.pf.ld := io.pageFault
    r.tlb.resp.paddr := RegNext(r.tlb.req.bits.tlb_req.vaddr)
    r.counter.external_reset := false.B
    io.busy := r.busy
    io.fault := r.fault
  }
}

class CorrelationStreamReaderSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Correlation StreamReader cancellation"
  it should "retire unpublished reservations and drain real responses without losing busy" in {
    implicit val p: Parameters = Parameters.empty
    test(LazyModule(new CorrelationStreamReaderHarness).module) { d =>
      d.io.request.valid.poke(false.B)
      d.io.request.bits.addr.poke(0x1034.U)
      d.io.request.bits.bytes.poke(32.U)
      d.io.request.bits.token.poke(3.U)
      d.io.response.ready.poke(false.B)
      d.io.a.ready.poke(false.B)
      d.io.d.valid.poke(false.B)
      d.io.d.bits.opcode.poke(1.U) // AccessAckData
      d.io.d.bits.param.poke(0.U)
      d.io.d.bits.size.poke(6.U)
      d.io.d.bits.source.poke(0.U)
      d.io.d.bits.sink.poke(0.U)
      d.io.d.bits.denied.poke(false.B)
      d.io.d.bits.data.poke(0.U)
      d.io.d.bits.corrupt.poke(false.B)
      d.io.cancel.poke(false.B)
      d.io.miss.poke(false.B)
      d.io.pageFault.poke(false.B)
      d.clock.step()

      // One strip crosses a line, reserving two transactions. Neither may be
      // sent while A.ready is low. Keep merger output blocked during cancel.
      d.io.request.ready.expect(true.B)
      d.io.request.valid.poke(true.B)
      d.clock.step()
      d.io.request.valid.poke(false.B)
      d.clock.step(8)
      d.io.busy.expect(true.B)
      d.io.cancel.poke(true.B)
      for (_ <- 0 until 8) {
        d.io.a.valid.expect(false.B)
        d.io.request.ready.expect(false.B)
        d.io.busy.expect(true.B)
        d.clock.step()
      }
      d.io.response.ready.poke(true.B)
      var cycles = 0
      while (d.io.busy.peek().litToBoolean && cycles < 50) {
        d.io.a.valid.expect(false.B)
        d.clock.step()
        cycles += 1
      }
      assert(cycles < 50, "canceled tracker reservation did not drain")
      d.io.cancel.poke(false.B)
      d.clock.step()
      d.io.request.ready.expect(true.B)

      // New job: let both Gets publish, then cancel. Their real responses must
      // still be consumed; no synthetic response may prematurely free the IDs.
      d.io.a.ready.poke(true.B)
      d.io.request.valid.poke(true.B)
      d.clock.step()
      d.io.request.valid.poke(false.B)
      val ids = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      cycles = 0
      while (ids.size < 2 && cycles < 30) {
        if (d.io.a.valid.peek().litToBoolean) ids += d.io.a.bits.source.peek().litValue
        d.clock.step()
        cycles += 1
      }
      assert(ids.size == 2 && ids.distinct.size == 2)
      d.io.cancel.poke(true.B)
      d.clock.step(4)
      d.io.busy.expect(true.B)
      for (id <- ids.reverse) {
        d.io.d.bits.source.poke(id.U)
        d.io.d.valid.poke(true.B)
        cycles = 0
        while (!d.io.d.ready.peek().litToBoolean && cycles < 20) {
          d.clock.step()
          cycles += 1
        }
        assert(cycles < 20)
        d.clock.step()
        d.io.d.valid.poke(false.B)
        d.io.busy.expect(true.B) // Last beat is still queued in the merger.
      }
      cycles = 0
      while (d.io.busy.peek().litToBoolean && cycles < 30) {
        d.clock.step()
        cycles += 1
      }
      assert(cycles < 30, "published transactions did not drain")
      d.io.cancel.poke(false.B)
      d.clock.step()

      // A translation fault must never publish an A request; cancel must also
      // drain the retry queues in this case rather than wedging on the fault.
      d.io.pageFault.poke(true.B)
      d.io.request.valid.poke(true.B)
      d.clock.step()
      d.io.request.valid.poke(false.B)
      cycles = 0
      while (!d.io.fault.peek().litToBoolean && cycles < 20) {
        d.io.a.valid.expect(false.B)
        d.clock.step()
        cycles += 1
      }
      assert(cycles < 20)
      d.io.a.valid.expect(false.B)
      d.io.cancel.poke(true.B)
      cycles = 0
      while (d.io.busy.peek().litToBoolean && cycles < 50) {
        d.io.a.valid.expect(false.B)
        d.clock.step()
        cycles += 1
      }
      assert(cycles < 50, "faulted translation reservations did not drain")
    }
  }
}


class CorrelationLoopCommand extends Bundle {
  val funct = UInt(7.W)
  val rs1 = UInt(64.W)
  val rs2 = UInt(64.W)
}

/** Completes each emitted command one cycle after acceptance. This models the
  * reservation-station credit protocol while checking the real unroller's
  * generated addresses; it deliberately does not model arithmetic or SRAM.
  */
class CorrelationLoopHarness(implicit p: Parameters) extends Module {
  import GemminiISA._
  val config = GemminiCustomConfigs.dpvoFP32Dim16CorrGatherConfig
  import config._
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new CorrelationLoopCommand))
    val out = Decoupled(new CorrelationLoopCommand)
    val completed = Output(Vec(2, Bool()))
  })
  val loop = Module(new LoopMatmul(16, 40, 28, 8, 16, 4, 4096, 1024, 32, 32, 64,
    new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutSpadRs1(32, local_addr_t), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)))
  loop.io.in.valid := io.in.valid
  loop.io.in.bits := 0.U.asTypeOf(loop.io.in.bits)
  loop.io.in.bits.cmd.inst.funct := io.in.bits.funct
  loop.io.in.bits.cmd.rs1 := io.in.bits.rs1
  loop.io.in.bits.cmd.rs2 := io.in.bits.rs2
  io.in.ready := loop.io.in.ready
  io.out.valid := loop.io.out.valid
  loop.io.out.ready := io.out.ready
  io.out.bits.funct := loop.io.out.bits.cmd.inst.funct
  io.out.bits.rs1 := loop.io.out.bits.cmd.rs1
  io.out.bits.rs2 := loop.io.out.bits.cmd.rs2
  val f = io.out.bits.funct
  val fire = io.out.fire
  loop.io.ld_completed := RegNext(fire && (f === LOAD_CMD || f === LOAD2_CMD || f === LOAD3_CMD), false.B)
  loop.io.st_completed := RegNext(fire && (f === STORE_CMD || f === STORE_SPAD_CMD), false.B)
  loop.io.ex_completed := RegNext(fire && (f === PRELOAD_CMD || f === COMPUTE_AND_FLIP_CMD || f === COMPUTE_AND_STAY_CMD), false.B)
  io.completed := loop.io.completed
}

class CorrelationLoopRestoreSpec extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "Correlation one-shot LoopMatmul addresses"
  it should "restore both loop slots for normal kernels while retaining legacy persistent addresses" in {
    implicit val p: Parameters = Parameters.empty.alterPartial { case TileKey => RocketTileParams() }
    test(new CorrelationLoopHarness) { d =>
      d.io.in.valid.poke(false.B)
      d.io.out.ready.poke(true.B)
      d.clock.step()
      val random = new scala.util.Random(17)
      def run(custom: Option[(Int, Int)], restore: Boolean, skipLoads: Boolean,
              expectCustom: Option[(Int, Int)]): Unit = {
        val commands = scala.collection.mutable.ArrayBuffer.empty[(Int, BigInt, BigInt)]
        var completed: Option[Int] = None
        def tick(): Unit = {
          val ready = random.nextInt(5) != 0
          d.io.out.ready.poke(ready.B)
          if (ready && d.io.out.valid.peek().litToBoolean) commands += ((
            d.io.out.bits.funct.peek().litValue.toInt,
            d.io.out.bits.rs1.peek().litValue,
            d.io.out.bits.rs2.peek().litValue))
          (0 until 2).foreach(i => if (d.io.completed(i).peek().litToBoolean) completed = Some(i))
          d.clock.step()
        }
        def command(funct: Int, rs1: BigInt, rs2: BigInt): Unit = {
          d.io.in.bits.funct.poke(funct.U)
          d.io.in.bits.rs1.poke(rs1.U)
          d.io.in.bits.rs2.poke(rs2.U)
          d.io.in.valid.poke(true.B)
          var cycles = 0
          while (!d.io.in.ready.peek().litToBoolean && cycles < 200) { tick(); cycles += 1 }
          assert(cycles < 200)
          tick()
          d.io.in.valid.poke(false.B)
        }
        // M/N/K=1/32/32 => I/J/K=1/2/2; keep ldD bookkeeping with D=0.
        command(9, 15, (BigInt(2) << 32) | (BigInt(2) << 16) | 1)
        command(10, 0x10000, 0x20000)
        command(11, 0, 0x30000)
        command(12, 32, 32)
        command(13, 32, 32)
        custom.foreach { case (a, bEnd) => command(24, a, bEnd) }
        command(8, 0, (if (restore) 1 << 10 else 0) | (if (skipLoads) (1 << 3) | (1 << 4) else 0))
        var cycles = 0
        while (completed.isEmpty && cycles < 500) { tick(); cycles += 1 }
        assert(completed.nonEmpty, "loop did not complete")
        val (a, bEnd) = expectCustom.getOrElse((completed.get * 2048, (completed.get + 1) * 2048))
        val b = bEnd - 64
        def addresses(funct: Set[Int], rs: Int): Seq[Int] = commands.collect {
          case (f, r1, r2) if funct.contains(f) => ((if (rs == 1) r1 else r2) & 4095).toInt
        }.toSeq
        assert(addresses(Set(6), 1) == Seq(b, b + 16, b + 32, b + 48), s"B preload addresses: $commands")
        assert(addresses(Set(4, 5), 1) == Seq(a, a, a + 16, a + 16), s"A execute addresses: $commands")
        assert(addresses(Set(2), 2) == (if (skipLoads) Seq.empty else Seq(a, a + 16)))
        assert(addresses(Set(1), 2) == (if (skipLoads) Seq.empty else Seq(b, b + 16, b + 32, b + 48)))
        // Drain the one-cycle mock completions before configuring another loop.
        d.io.in.valid.poke(false.B)
        d.clock.step(2)
      }
      // Both physical loop slots use custom addresses, then normal calls must
      // return to the default half-SPAD layouts regardless of initial head ID.
      run(Some((2048, 1024)), true, true, Some((2048, 1024)))
      run(Some((2048, 1024)), true, true, Some((2048, 1024)))
      run(None, false, false, None)
      run(None, false, false, None)
      // Bit10=0 preserves the pre-existing funct24 persistent-address contract.
      run(Some((2560, 1536)), false, true, Some((2560, 1536)))
      run(Some((2560, 1536)), false, true, Some((2560, 1536)))
      run(None, false, false, Some((2560, 1536)))
      run(None, false, false, Some((2560, 1536)))
    }
  }
}
