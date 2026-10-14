package gemmini

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import chiseltest._
import chiseltest.simulator.VerilatorBackendAnnotation
import org.scalatest.flatspec.AnyFlatSpec
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{AddressSet, LazyModule, LazyModuleImp, TransferSizes}
import freechips.rocketchip.tile.{RocketTileParams, TileKey, TileVisibilityNodeKey}
import freechips.rocketchip.tilelink._

/** Real GemminiModule + unrollers + RS + controllers + Scratchpad + engines.
  * Only the external memory responder and CPU command source are test devices.
  */
class BranchControllerHarness(implicit baseParameters: Parameters) extends LazyModule {
  private val visibility = TLEphemeralNode()
  private val parameters: Parameters = baseParameters.alterPartial {
    case TileKey => RocketTileParams()
    case TileVisibilityNodeKey => visibility
  }
  private val baseConfig = GemminiCustomConfigs.dpvoFP32Dim16SoftAggConfig
  // Gemmini emits a C header during elaboration. Redirect only this test's
  // generated header into a temporary directory, never the checked-out ABI.
  private val header = java.nio.file.Files.createTempDirectory("gemmini-dispatch-test-").resolve("gemmini_params.h")
  private val headerDirectory = java.nio.file.Paths.get(baseConfig.headerFilePath).toAbsolutePath.getParent
  private val config = baseConfig.copy(use_firesim_simulation_counters = false,
    headerFileName = headerDirectory.relativize(header).toString)
  val accelerator = LazyModule(new Gemmini(config)(implicitly[Arithmetic[Float]], parameters))
  val manager = TLManagerNode(Seq(TLSlavePortParameters.v1(
    Seq(TLSlaveParameters.v1(address = Seq(AddressSet(0, (BigInt(1) << 32) - 1)),
      supportsGet = TransferSizes(1, 64), supportsPutFull = TransferSizes(1, 64),
      supportsPutPartial = TransferSizes(1, 64))), beatBytes = 64)))
  manager := visibility := accelerator.node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val io = IO(new Bundle {
      val command = Flipped(Decoupled(new SoftAggCommand))
      val response = Decoupled(UInt(64.W))
      val busy = Output(Bool())
      val interrupt = Output(Bool())
      val allowMemoryResponse = Input(Bool())
      val memoryRequests = Output(UInt(32.W))
      val writeData = Valid(UInt(512.W))
      val owner = Output(UInt(2.W))
      val completed = Valid(new BranchCompletion)
    })
    val r = accelerator.module.io
    r.mem <> DontCare
    r.fpu_req.ready := true.B
    r.fpu_resp.valid := false.B; r.fpu_resp.bits := 0.U.asTypeOf(r.fpu_resp.bits)
    r.exception := false.B
    for (ptw <- r.ptw) {
      ptw.req.ready := true.B
      ptw.resp.valid := false.B; ptw.resp.bits := 0.U.asTypeOf(ptw.resp.bits)
      ptw.ptbr := 0.U.asTypeOf(ptw.ptbr); ptw.vsatp := 0.U.asTypeOf(ptw.vsatp)
      ptw.hgatp := 0.U.asTypeOf(ptw.hgatp); ptw.status := 0.U.asTypeOf(ptw.status)
      ptw.hstatus := 0.U.asTypeOf(ptw.hstatus); ptw.gstatus := 0.U.asTypeOf(ptw.gstatus)
      ptw.pmp := 0.U.asTypeOf(ptw.pmp)
      // Make test memory accessible at every privilege level. OS/PMP policy
      // is outside this controller/ownership integration test.
      if (ptw.pmp.nonEmpty) {
        ptw.pmp(0).cfg.a := 3.U
        ptw.pmp(0).cfg.r := true.B; ptw.pmp(0).cfg.w := true.B; ptw.pmp(0).cfg.x := true.B
        ptw.pmp(0).addr := Fill(ptw.pmp(0).addr.getWidth, 1.U(1.W))
        ptw.pmp(0).mask := Fill(ptw.pmp(0).mask.getWidth, 1.U(1.W))
      }
      ptw.customCSRs <> DontCare
    }
    r.cmd.valid := io.command.valid
    io.command.ready := r.cmd.ready
    r.cmd.bits := 0.U.asTypeOf(r.cmd.bits)
    r.cmd.bits.inst.funct := io.command.bits.funct
    r.cmd.bits.inst.opcode := "h7b".U
    r.cmd.bits.inst.rd := 1.U
    r.cmd.bits.inst.xd := io.command.bits.funct === GemminiISA.CORR_STATUS ||
      io.command.bits.funct === GemminiISA.SOFTAGG_STATUS
    r.cmd.bits.rs1 := io.command.bits.rs1; r.cmd.bits.rs2 := io.command.bits.rs2
    r.cmd.bits.status.prv := 3.U; r.cmd.bits.status.dprv := 3.U
    io.response.valid := r.resp.valid; io.response.bits := r.resp.bits.data
    r.resp.ready := io.response.ready
    io.busy := r.busy; io.interrupt := r.interrupt
    io.owner := DontCare
    io.completed := DontCare
    BoringUtils.bore(accelerator.module.dispatcher.io.owner, Seq(io.owner))
    BoringUtils.bore(accelerator.module.dispatcher.io.completed, Seq(io.completed))

    val (tl, _) = manager.in(0)
    val responses = Module(new Queue(chiselTypeOf(tl.d.bits), 16))
    responses.io.enq.valid := tl.a.valid
    tl.a.ready := responses.io.enq.ready
    responses.io.enq.bits := 0.U.asTypeOf(tl.d.bits)
    responses.io.enq.bits.opcode := Mux(tl.a.bits.opcode === TLMessages.Get,
      TLMessages.AccessAckData, TLMessages.AccessAck)
    responses.io.enq.bits.source := tl.a.bits.source
    responses.io.enq.bits.size := tl.a.bits.size
    // Deterministic external input: every FP32 element is 1.0.
    responses.io.enq.bits.data := Fill(16, "h3f800000".U(32.W))
    tl.d.valid := responses.io.deq.valid && io.allowMemoryResponse
    tl.d.bits := responses.io.deq.bits
    responses.io.deq.ready := tl.d.ready && io.allowMemoryResponse
    tl.b.valid := false.B; tl.b.bits := DontCare; tl.c.ready := true.B; tl.e.ready := true.B
    val requests = RegInit(0.U(32.W))
    when(tl.a.fire) { requests := requests + 1.U }
    io.memoryRequests := requests
    io.writeData.valid := tl.a.fire && tl.a.bits.opcode =/= TLMessages.Get
    io.writeData.bits := tl.a.bits.data
  }
}

class BranchControllerSpec extends AnyFlatSpec with ChiselScalatestTester {
  it should "serialize real normal DMA, CorrGather and SoftAgg while preserving status replies and write drain" in {
    implicit val p: Parameters = Parameters.empty
    test(LazyModule(new BranchControllerHarness).module).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      d.clock.setTimeout(200000)
      d.io.command.valid.poke(false.B); d.io.response.ready.poke(true.B)
      d.io.allowMemoryResponse.poke(true.B); d.clock.step(5)
      val completions = scala.collection.mutable.ArrayBuffer.empty[(Int, Boolean)]
      val stores = scala.collection.mutable.ArrayBuffer.empty[BigInt]
      def step(n: Int = 1): Unit = for (_ <- 0 until n) {
        if (d.io.completed.valid.peek().litToBoolean)
          completions += ((d.io.completed.bits.engine.peek().litValue.toInt, d.io.completed.bits.error.peek().litToBoolean))
        if (d.io.writeData.valid.peek().litToBoolean) stores += d.io.writeData.bits.peek().litValue
        d.io.interrupt.expect(false.B)
        d.clock.step()
      }
      def command(funct: Int, rs1: BigInt = 0, rs2: BigInt = 0): Unit = {
        d.io.command.bits.funct.poke(funct.U); d.io.command.bits.rs1.poke(rs1.U)
        d.io.command.bits.rs2.poke(rs2.U); d.io.command.valid.poke(true.B)
        var waited = 0
        while (!d.io.command.ready.peek().litToBoolean && waited < 20000) { step(); waited += 1 }
        assert(waited < 20000, s"command $funct stuck")
        step(); d.io.command.valid.poke(false.B)
      }
      def idle(): Unit = {
        var waited = 0
        while (d.io.busy.peek().litToBoolean && waited < 20000) { step(); waited += 1 }
        assert(waited < 20000, "controller failed to drain"); step(2)
      }
      def status(funct: Int): BigInt = {
        d.io.response.ready.poke(false.B); command(funct)
        var waited = 0
        while (!d.io.response.valid.peek().litToBoolean && waited < 100) { step(); waited += 1 }
        assert(waited < 100)
        val value = d.io.response.bits.peek().litValue
        step(5); d.io.response.valid.expect(true.B); d.io.response.bits.expect(value.U)
        d.io.response.ready.poke(true.B); step(); value
      }

      // A normal mvin stays outstanding while its response is held. A following
      // CORR configuration must not steal the shared reader or SP bank ports.
      command(0, (BigInt(0x3f800000L) << 32) | (BigInt(1) << 8) | (BigInt(16) << 16) | 1, 64)
      d.io.allowMemoryResponse.poke(false.B)
      command(2, 0x1000, (BigInt(1) << 48) | (BigInt(16) << 32) | 2048)
      command(40, (BigInt(1) << 32) | 1, (BigInt(1) << 32) | 1)
      step(40); d.io.owner.expect(0.U)
      assert(d.io.memoryRequests.peek().litValue > 0)
      d.io.allowMemoryResponse.poke(true.B)
      // Entire 8x8 windows are outside a 1x1 image: real gather writes 1024
      // zero rows with no external reads, still exercising packer and SP ports.
      command(41, 0x2000, 0x3000)
      val negativeOrigin = BigInt("fffffff0fffffff0", 16)
      command(42, negativeOrigin, negativeOrigin)
      command(43)
      command(45, (BigInt(3) << 32) | 2, (BigInt(1) << 32) | 1)
      command(46, BigInt(1) << 32, (BigInt(2049) << 32) | (BigInt(1) << 16) | (BigInt(16) << 8) | 1)
      idle()
      assert(completions.contains((1, false)), completions.toString)
      assert(completions.contains((2, false)), completions.toString)
      assert((status(44) & 7) == 4)
      assert((status(47) & 7) == 4)

      // Store retirement in the RS precedes the external write acknowledgement.
      // A later branch must wait for the real writer, not just the RS entry.
      command(0, 2, (BigInt(0x3f800000L) << 32) | 64)
      d.io.allowMemoryResponse.poke(false.B)
      command(3, 0x8000, (BigInt(1) << 48) | (BigInt(16) << 32) | 2049)
      command(45, (BigInt(3) << 32) | 2, (BigInt(1) << 32) | 1)
      step(80); d.io.owner.expect(0.U)
      assert(stores.nonEmpty && stores.last == 0, stores.toString)
      d.io.allowMemoryResponse.poke(true.B)
      command(46, BigInt(1) << 32, (BigInt(2049) << 32) | (BigInt(1) << 16) | (BigInt(16) << 8) | 1)
      idle()

      // Preserve the Linux interrupt/fence exception between PRELOAD and
      // COMPUTE. Strict branch admission must not become the RoCC busy signal.
      command(0, (BigInt(0x3f800000L) << 32) | (BigInt(1) << 16) | 4, BigInt(1) << 48)
      command(6, (BigInt(16) << 48) | (BigInt(16) << 32),
        (BigInt(1) << 48) | (BigInt(16) << 32) | BigInt(0x80000000L))
      step(80); d.io.busy.expect(false.B)
      command(4, (BigInt(1) << 48) | (BigInt(16) << 32) | 2048,
        (BigInt(16) << 48) | (BigInt(16) << 32) | BigInt(0xffffffffL))
      idle()

      command(43)
      assert((status(44) & 2) == 2)
      command(7) // recovery FLUSH bypasses the active Corr branch
      idle(); assert((status(44) & 1) == 1)

      // Invalid submissions complete with a tagged error even if busy never
      // becomes high, and must not strand the dispatcher owner.
      command(43, 1) // misaligned B
      idle(); assert((status(44) & 1) == 1)
      command(46, 0, 0) // rows=0
      idle(); assert((status(47) & 1) == 1)
      assert(completions.contains((1, true)), completions.toString)
      assert(completions.contains((2, true)), completions.toString)
    }
  }
}
