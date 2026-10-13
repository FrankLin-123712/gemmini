package gemmini

import chisel3._
import chisel3.util._
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tile.{RocketTileParams, TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._

/** Exercise the actual Scratchpad muxes, physical SP/ACC banks and accumulator
  * write/read hazard handling. Ordinary execute write ports initialize memory;
  * a debug local read shares the exclusive client only while the engine is idle.
  * Any TileLink request is a failure: this datapath must remain wholly local.
  */
class SoftAggScratchpadHarness(implicit baseParameters: Parameters) extends LazyModule {
  private val visibility = TLEphemeralNode()
  private val parameters = baseParameters.alterPartial {
    case TileKey => RocketTileParams()
    case TileVisibilityNodeKey => visibility
  }
  private val config = GemminiCustomConfigs.dpvoFP32Dim16SoftAggConfig.copy(use_firesim_simulation_counters = false)
  val scratchpad = LazyModule(new Scratchpad(config)(parameters, implicitly[Arithmetic[Float]]))
  val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0, (BigInt(1) << 32) - 1)),
      supportsGet = TransferSizes(64, 64), supportsPutFull = TransferSizes(64, 64),
      supportsPutPartial = TransferSizes(64, 64))), beatBytes = 64)))
  manager := visibility := scratchpad.id_node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val initialize = Flipped(Decoupled(new SoftAggWrite))
      val start = Flipped(Decoupled(new SoftAggDescriptor))
      val inspect = Flipped(Decoupled(UInt(32.W)))
      val inspected = Decoupled(UInt(512.W))
      val busy = Output(Bool())
      val error = Output(Bool())
    })
    val engine = Module(new SoftAgg(4096, 1024, config.acc_latency + 3))
    engine.io.start <> io.start
    io.busy := engine.io.busy
    io.error := engine.io.error
    val s = scratchpad.module.io
    s.flush := false.B
    s.counter.external_reset := false.B
    s.dma.read.req.valid := false.B; s.dma.read.req.bits := 0.U.asTypeOf(s.dma.read.req.bits)
    s.dma.write.req.valid := false.B; s.dma.write.req.bits := 0.U.asTypeOf(s.dma.write.req.bits)
    s.tlb.foreach(_.resp := 0.U.asTypeOf(s.tlb.head.resp))
    s.corr.foreach { c =>
      c.active := false.B; c.cancel := false.B; c.status := 0.U.asTypeOf(c.status)
      c.readReq.valid := false.B; c.readReq.bits := 0.U.asTypeOf(c.readReq.bits)
      c.readResp.ready := true.B; c.write.valid := false.B; c.write.bits := 0.U.asTypeOf(c.write.bits)
    }
    val inspecting = RegInit(false.B)
    val client = s.softagg.get
    client.active := engine.io.busy || inspecting || io.inspect.valid
    client.read.valid := Mux(engine.io.busy, engine.io.memory.read.valid, io.inspect.valid && !inspecting)
    client.read.bits := Mux(engine.io.busy, engine.io.memory.read.bits, io.inspect.bits)
    engine.io.memory.read.ready := engine.io.busy && client.read.ready
    io.inspect.ready := !engine.io.busy && !inspecting && client.read.ready
    client.response.ready := Mux(engine.io.busy, engine.io.memory.response.ready, io.inspected.ready)
    engine.io.memory.response.valid := engine.io.busy && client.response.valid
    engine.io.memory.response.bits := client.response.bits
    io.inspected.valid := !engine.io.busy && client.response.valid
    io.inspected.bits := client.response.bits
    client.write <> engine.io.memory.write
    when(io.inspect.fire) { inspecting := true.B }
    when(io.inspected.fire) { inspecting := false.B }
    io.initialize.ready := false.B
    val initAllowed = !client.active
    for(i <- 0 until config.sp_banks) {
      val w = s.srams.write(i)
      w.valid := io.initialize.valid && initAllowed && !io.initialize.bits.addr(31) &&
        io.initialize.bits.addr(30, 0) / config.sp_bank_entries.U === i.U
      w.addr := io.initialize.bits.addr(30, 0) % config.sp_bank_entries.U
      w.data := io.initialize.bits.data; w.mask.foreach(_ := true.B)
      when(w.valid) { io.initialize.ready := w.ready }
      s.srams.read(i).req.valid := false.B; s.srams.read(i).req.bits := 0.U.asTypeOf(s.srams.read(i).req.bits)
      s.srams.read(i).resp.ready := true.B
    }
    for(i <- 0 until config.acc_banks) {
      val w = s.acc.write(i)
      w.valid := io.initialize.valid && initAllowed && io.initialize.bits.addr(31) &&
        io.initialize.bits.addr(30, 0) / config.acc_bank_entries.U === i.U
      w.bits.addr := io.initialize.bits.addr(30, 0) % config.acc_bank_entries.U
      w.bits.data := io.initialize.bits.data.asTypeOf(w.bits.data)
      w.bits.acc := false.B; w.bits.mask.foreach(_ := true.B)
      when(w.valid) { io.initialize.ready := w.ready }
      s.acc.read_req(i).valid := false.B; s.acc.read_req(i).bits := 0.U.asTypeOf(s.acc.read_req(i).bits)
      s.acc.read_resp(i).ready := true.B
    }
    val (tl, _) = manager.in(0)
    tl.a.ready := true.B; tl.d.valid := false.B; tl.d.bits := 0.U.asTypeOf(tl.d.bits)
    tl.b.valid := false.B; tl.b.bits := 0.U.asTypeOf(tl.b.bits); tl.c.ready := true.B; tl.e.ready := true.B
    assert(!tl.a.valid, "SoftAgg local-memory operation unexpectedly issued external DMA")
  }
}

class SoftAggScratchpadSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def bits(x: scala.Float): BigInt = BigInt(java.lang.Float.floatToRawIntBits(x).toLong & 0xffffffffL)
  private def row(xs: Seq[scala.Float]): BigInt = xs.zipWithIndex.map { case (x, i) => bits(x) << (32 * i) }.reduce(_ | _)
  it should "read raw ACC results, overwrite S safely, and produce SP output without DMA or stale normal-path responses" in {
    implicit val p: Parameters = Parameters.empty
    test(LazyModule(new SoftAggScratchpadHarness).module).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      d.clock.setTimeout(100000)
      d.io.initialize.valid.poke(false.B); d.io.start.valid.poke(false.B)
      d.io.inspect.valid.poke(false.B); d.io.inspected.ready.poke(false.B)
      d.clock.step(5)
      def initialize(addr: BigInt, xs: Seq[scala.Float]): Unit = {
        d.io.initialize.bits.addr.poke(addr.U); d.io.initialize.bits.data.poke(row(xs).U)
        d.io.initialize.valid.poke(true.B)
        var wait = 0
        while(!d.io.initialize.ready.peek().litToBoolean && wait < 30) { d.clock.step(); wait += 1 }
        assert(wait < 30); d.clock.step(); d.io.initialize.valid.poke(false.B)
      }
      def inspect(addr: BigInt): BigInt = {
        d.io.inspect.bits.poke(addr.U); d.io.inspect.valid.poke(true.B)
        var wait = 0
        while(!d.io.inspect.ready.peek().litToBoolean && wait < 30) { d.clock.step(); wait += 1 }
        assert(wait < 30); d.clock.step(); d.io.inspect.valid.poke(false.B)
        while(!d.io.inspected.valid.peek().litToBoolean && wait < 60) { d.clock.step(); wait += 1 }
        assert(wait < 60)
        val data = d.io.inspected.bits.peek().litValue
        d.clock.step(7); d.io.inspected.valid.expect(true.B); d.io.inspected.bits.expect(data.U)
        d.io.inspected.ready.poke(true.B); d.clock.step(); d.io.inspected.ready.poke(false.B)
        data
      }
      val sBase = BigInt("80000000", 16); val vBase = BigInt("80000200", 16)
      initialize(2048, Seq.fill(16)(0.0f)); initialize(2049, Seq.fill(16)(0.25f))
      initialize(sBase, Seq.fill(16)(0.0f)); initialize(sBase + 1, Seq.fill(16)(0.0f))
      initialize(vBase, (0 until 16).map(_.toFloat)); initialize(vBase + 1, (0 until 16).map(c => (c + 2).toFloat))
      d.clock.step(5)
      d.io.start.bits.s.poke(sBase.U); d.io.start.bits.v.poke(vBase.U)
      d.io.start.bits.biasS.poke(2048.U); d.io.start.bits.biasV.poke(2049.U)
      d.io.start.bits.strideS.poke(1.U); d.io.start.bits.strideV.poke(1.U)
      d.io.start.bits.out.poke(3072.U); d.io.start.bits.rows.poke(2.U)
      d.io.start.bits.cols.poke(5.U); d.io.start.bits.mode.poke(0.U); d.io.start.bits.configured.poke(true.B)
      d.io.start.valid.poke(true.B); d.clock.step(); d.io.start.valid.poke(false.B)
      var cycles = 0
      while(d.io.busy.peek().litToBoolean && cycles < 20000) { d.clock.step(); cycles += 1 }
      assert(cycles < 20000); d.io.error.expect(false.B)
      val expected = (0 until 16).map(c => if(c < 5) c.toFloat + 1.25f else 0.0f)
      assert(inspect(3072) == row(expected))
      assert(inspect(sBase) == row((0 until 16).map(c => if(c < 5) 1.0f else 0.0f)))
      assert(inspect(sBase + 1) == row((0 until 16).map(c => if(c < 5) 1.0f else 0.0f)))
      assert(inspect(vBase) == row((0 until 16).map(_.toFloat)))
      // Releasing exclusive ownership must leave no duplicate SP response queued.
      initialize(3073, Seq.fill(16)(-2.0f))
      assert(inspect(3073) == row(Seq.fill(16)(-2.0f)))
      println(s"SoftAgg actual Scratchpad command_cycles=$cycles")
    }
  }
}
