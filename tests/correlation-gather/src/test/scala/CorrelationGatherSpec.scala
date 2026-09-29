package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable
import scala.util.Random

/** The memory model is independent of the RTL's bit-concatenated tile addressing:
  * requests are checked against a CHW loop, writes against logical B[c,n]. It
  * splits 64-byte crossings and can return any pending fragment out of order.
  */
class CorrelationGatherSpec extends AnyFlatSpec with ChiselScalatestTester {
  case class Geometry(h: Int, w: Int, x: Int, y: Int, base: BigInt)
  case class Request(addr: BigInt, bytes: Int)
  case class Fragment(token: Int, data: BigInt, mask: BigInt, bytes: Int)
  private val full = (BigInt(1) << 64) - 1
  private def feature(level: Int, c: Int, y: Int, x: Int): BigInt =
    BigInt(0x3f000000L ^ (level.toLong << 22) ^ (c.toLong << 14) ^ (y.toLong << 7) ^ x.toLong) & 0xffffffffL

  private def defaults(d: CorrelationGatherController): Unit = {
    d.io.start.valid.poke(false.B)
    d.io.readReq.ready.poke(false.B)
    d.io.readResp.valid.poke(false.B)
    d.io.readResp.bits.token.poke(0.U)
    d.io.readResp.bits.data.poke(0.U)
    d.io.readResp.bits.mask.poke(0.U)
    d.io.readResp.bits.bytesRead.poke(0.U)
    d.io.write.ready.poke(false.B)
    d.io.abort.poke(false.B)
    d.io.memoryBusy.poke(false.B)
  }
  private def start(d: CorrelationGatherController, g: Seq[Geometry], bBase: Int): Unit = {
    for (l <- 0 until 2) {
      d.io.start.bits.bases(l).poke(g(l).base.U)
      d.io.start.bits.heights(l).poke(g(l).h.U)
      d.io.start.bits.widths(l).poke(g(l).w.U)
      d.io.start.bits.originX(l).poke(g(l).x.S)
      d.io.start.bits.originY(l).poke(g(l).y.S)
    }
    d.io.start.bits.bBase.poke(bBase.U)
    d.io.start.ready.expect(true.B)
    d.io.start.valid.poke(true.B)
    d.clock.step()
    d.io.start.valid.poke(false.B)
  }
  private def requests(g: Seq[Geometry]): Seq[Request] = for {
    c <- 0 until 128; l <- 0 until 2; gy <- 0 until 8
    y = g(l).y.toLong + gy
    x0 = math.max(g(l).x.toLong, 0L)
    x1 = math.min(g(l).x.toLong + 8L, g(l).w.toLong)
    if y >= 0 && y < g(l).h && x1 > x0
  } yield Request(g(l).base + 4 * (c.toLong * g(l).h * g(l).w + y * g(l).w + x0), ((x1 - x0) * 4).toInt)

  private def readByte(g: Seq[Geometry], addr: BigInt): BigInt = {
    val l = g.indexWhere(v => addr >= v.base && addr < v.base + BigInt(128) * v.h * v.w * 4)
    assert(l >= 0, s"read outside tensor: $addr")
    val offset = addr - g(l).base
    val element = (offset / 4).toInt
    val c = element / (g(l).h * g(l).w)
    val y = element / g(l).w % g(l).h
    val x = element % g(l).w
    (feature(l, c, y, x) >> (8 * (offset.toInt % 4))) & 255
  }
  private def expectedRow(g: Seq[Geometry], row: Int): BigInt = {
    val tile = row / 16
    val c = tile / 8 * 16 + row % 16
    val n0 = tile % 8 * 16
    (0 until 16).map { lane =>
      val n = n0 + lane
      val l = n / 64
      val y = g(l).y.toLong + (n % 64) / 8
      val x = g(l).x.toLong + n % 8
      val bits = if (y >= 0 && y < g(l).h && x >= 0 && x < g(l).w) feature(l, c, y.toInt, x.toInt) else BigInt(0)
      bits << (lane * 32)
    }.foldLeft(BigInt(0))(_ | _)
  }

  private def run(d: CorrelationGatherController, g: Seq[Geometry], bBase: Int, seed: Int): Unit = {
    val random = new Random(seed)
    val expectedRequests = requests(g)
    val pending = mutable.ArrayBuffer.empty[Fragment]
    val rows = mutable.Set.empty[Int]
    var requestIndex = 0
    var cycles = 0
    var previousWrite: Option[(BigInt, BigInt)] = None
    var previousRead: Option[(BigInt, BigInt, BigInt)] = None
    var crossed = false
    var reordered = false
    start(d, g, bBase)
    while (!d.io.done.valid.peek().litToBoolean && cycles < 50000) {
      val reqReady = random.nextInt(5) != 0
      val writeReady = random.nextInt(4) != 0
      d.io.readReq.ready.poke(reqReady.B)
      d.io.write.ready.poke(writeReady.B)
      d.io.memoryBusy.poke(pending.nonEmpty.B)
      val chosen = if (pending.nonEmpty && random.nextInt(3) != 0) Some(random.nextInt(pending.size)) else None
      d.io.readResp.valid.poke(chosen.nonEmpty.B)
      chosen.foreach { index =>
        if (index != 0) reordered = true
        val f = pending(index)
        d.io.readResp.bits.token.poke(f.token.U)
        d.io.readResp.bits.data.poke(f.data.U)
        d.io.readResp.bits.mask.poke(f.mask.U)
        d.io.readResp.bits.bytesRead.poke(f.bytes.U)
      }
      previousRead.foreach { case (addr, bytes, token) =>
        d.io.readReq.valid.expect(true.B)
        d.io.readReq.bits.addr.expect(addr.U)
        d.io.readReq.bits.bytes.expect(bytes.U)
        d.io.readReq.bits.token.expect(token.U)
      }
      previousWrite.foreach { case (addr, data) =>
        d.io.write.valid.expect(true.B)
        d.io.write.bits.addr.expect(addr.U)
        d.io.write.bits.data.expect(data.U)
      }
      previousRead = None
      previousWrite = None
      if (d.io.write.valid.peek().litToBoolean) {
        val addr = d.io.write.bits.addr.peek().litValue
        val data = d.io.write.bits.data.peek().litValue
        if (writeReady) {
          val row = addr.toInt - bBase
          assert(row >= 0 && row < 1024, s"bad destination row: $row")
          assert(rows.add(row), s"duplicate row: $row")
          assert(data == expectedRow(g, row), s"row $row mismatch\nactual=$data\nexpected=${expectedRow(g, row)}")
        } else previousWrite = Some((addr, data))
      }
      val acceptedRequest = if (d.io.readReq.valid.peek().litToBoolean) {
        val addr = d.io.readReq.bits.addr.peek().litValue
        val bytes = d.io.readReq.bits.bytes.peek().litValue
        val token = d.io.readReq.bits.token.peek().litValue
        if (reqReady) Some((addr, bytes.toInt, token.toInt))
        else { previousRead = Some((addr, bytes, token)); None }
      } else None
      // Responses visible in this cycle predate any newly accepted request.
      chosen.foreach(i => pending.remove(i))
      acceptedRequest.foreach { case (addr, bytes, token) =>
        assert(requestIndex < expectedRequests.size)
        assert(Request(addr, bytes) == expectedRequests(requestIndex), s"request $requestIndex mismatch")
        requestIndex += 1
        var offset = 0
        while (offset < bytes) {
          val size = math.min(bytes - offset, 64 - ((addr + offset) % 64).toInt)
          if (size != bytes) crossed = true
          val data = (0 until size).map(i => readByte(g, addr + offset + i) << ((offset + i) * 8)).foldLeft(BigInt(0))(_ | _)
          pending += Fragment(token, data, ((BigInt(1) << size) - 1) << offset, size)
          offset += size
        }
      }
      d.clock.step()
      cycles += 1
    }
    assert(cycles < 50000, "gather hung")
    d.io.done.bits.expect(false.B)
    d.io.error.expect(false.B)
    d.io.busy.expect(false.B)
    assert(rows.size == 1024 && pending.isEmpty && requestIndex == expectedRequests.size)
    d.io.stats.rows.expect(1024.U)
    d.io.stats.strips.expect(expectedRequests.size.U)
    d.io.stats.usefulBytes.expect(expectedRequests.map(_.bytes.toLong).sum.U)
    if (seed == 1) assert(crossed && reordered, "test must exercise split and out-of-order returns")
    defaults(d)
    d.clock.step()
  }

  behavior of "CorrelationGatherController"
  it should "pack all native B tiles across clipping, split returns, reordering and stalls" in {
    test(new CorrelationGatherController(40, 12)).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      defaults(d)
      d.clock.step()
      run(d, Seq(Geometry(120, 188, 17, 7, 0x100004), Geometry(30, 47, 2, 1, 0x2000040)), 0, 1)
      run(d, Seq(Geometry(9, 11, -2, -1, 0x100004), Geometry(5, 7, 3, 2, 0x2000040)), 1024, 2)
      run(d, Seq(Geometry(1, 1, -7, -7, 0x100004), Geometry(1, 1, 0, 0, 0x2000040)), 2048, 3)
      run(d, Seq(Geometry(9, 11, Int.MinValue, Int.MaxValue, 0x100004), Geometry(5, 7, Int.MaxValue, Int.MinValue, 0x2000040)), 3072, 4)
      // New command must not inherit data, zero masks, counters or error state.
      run(d, Seq(Geometry(8, 8, 0, 0, 0x100004), Geometry(8, 8, 0, 0, 0x2000040)), 0, 5)
    }
  }

  it should "reject bad descriptors, drain aborts, and recover without reusing live tokens" in {
    test(new CorrelationGatherController(40, 12)).withAnnotations(Seq(VerilatorBackendAnnotation)) { d =>
      defaults(d)
      d.clock.step()
      val good = Seq(Geometry(8, 8, 0, 0, 0x100004), Geometry(8, 8, 0, 0, 0x2000040))
      val invalid = Seq(
        (good.updated(0, good(0).copy(h = 0)), 0),
        (good.updated(1, good(1).copy(w = 65536)), 0),
        (good.updated(0, good(0).copy(base = 3)), 0),
        (good.updated(0, good(0).copy(base = (BigInt(1) << 40) - 4)), 0),
        (good, 1))
      invalid.foreach { case (g, base) =>
        start(d, g, base)
        d.io.done.valid.expect(true.B)
        d.io.done.bits.expect(true.B)
        d.io.busy.expect(false.B)
        d.io.readReq.valid.expect(false.B)
        d.io.write.valid.expect(false.B)
        d.clock.step()
      }
      start(d, good, 0)
      d.io.readReq.ready.poke(true.B)
      var issued = 0
      while (issued < 8) {
        if (d.io.readReq.valid.peek().litToBoolean) issued += 1
        d.io.memoryBusy.poke((issued > 0).B)
        d.clock.step()
      }
      d.io.abort.poke(true.B)
      d.io.readReq.valid.expect(false.B)
      d.io.write.valid.expect(false.B)
      d.clock.step()
      d.io.abort.poke(false.B)
      // Late responses are dropped; they may not resurrect a cleared entry.
      d.io.readResp.valid.poke(true.B)
      d.io.readResp.bits.mask.poke(full.U)
      for (_ <- 0 until 8) {
        d.io.start.ready.expect(false.B)
        d.io.done.valid.expect(false.B)
        d.io.write.valid.expect(false.B)
        d.clock.step()
      }
      d.io.readResp.valid.poke(false.B)
      d.io.memoryBusy.poke(false.B)
      // A flush may remain asserted until completion is observed.
      d.io.abort.poke(true.B)
      d.clock.step()
      d.io.done.valid.expect(true.B)
      d.io.done.bits.expect(true.B)
      d.io.start.ready.expect(false.B)
      defaults(d)
      d.clock.step()
      run(d, good, 0, 9)

      // An unallocated response token is an error, not successful completion.
      start(d, good, 0)
      d.io.readResp.valid.poke(true.B)
      d.io.readResp.bits.token.poke(31.U)
      d.io.readResp.bits.mask.poke(15.U)
      d.io.memoryBusy.poke(true.B)
      d.clock.step()
      d.io.error.expect(true.B)
      d.io.readResp.valid.poke(false.B)
      d.io.memoryBusy.poke(false.B)
      d.clock.step()
      d.io.done.valid.expect(true.B)
      d.io.done.bits.expect(true.B)
      defaults(d)
      d.clock.step()
      run(d, good, 0, 10)
    }
  }
}
