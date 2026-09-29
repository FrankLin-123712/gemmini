# Correlation gather RTL regression

This isolated sbt project compiles the actual `CorrelationGatherController.scala`
and `CorrelationRowPacker.scala` sources with the enclosing Chipyard's Chisel
6.7.0 / Scala 2.13.16 versions. It avoids the older unrelated Gemmini iotesters
suite and requires sbt, Java, Verilator, make and a C++ compiler on `PATH`.

```sh
cd chipyard/generators/gemmini/tests/correlation-gather
sbt test
```

The independent software scoreboard builds logical `B[c,n]` from CHW features,
checks every source request and all 1,024 destination rows bit for bit. Cases
cover full windows, all four clipped edges, a 1x1 feature map, extreme signed
origins, different B banks, consecutive jobs, random read/write stalls, split
64-byte boundary responses and responses returned out of order. It also checks
invalid descriptors, abort with in-flight reads and late responses, held flush,
malformed responses, error recovery, counters and stable backpressured requests.

The isolated memory model accepts the same strip-relative byte masks as the
adapter from the real StreamReader/BeatMerger. It does **not** instantiate that
reader, TLB, TileLink or the SRAM arbiter. The additional integration tests below
cover the real reader and loop unroller. Neither suite alone establishes FireSim
parity, integrated SPAD ownership, end-to-end arithmetic parity or speedup.

`memoryBusy` must include all outstanding reader transactions and queued
responses. On abort the test keeps it asserted while late responses drain; a new
descriptor must not be accepted until draining and completion have finished.

## Reader and loop integration regressions

From this directory, run:

```sh
./run-integration.sh
```

This uses the enclosing Chipyard build and actual Gemmini dependencies. It adds
ChiselTest and selects `src/test/scala/gemmini/CorrelationDmaSpec.scala` only for
that sbt invocation, without editing the shared build or compiling legacy tests.
These short integration tests use ChiselTest's interpreter backend.

- `CorrelationDmaSpec`: actual BeatMerger -> RowPacker, including two independently
  split strips, reversed Get responses, left padding, stable blocked writes and
  reader busy until the final merged response is consumed.
- `CorrelationStreamReaderSpec`: actual StreamReaderCore, translation/retry
  queues, XactTracker, BeatMerger and a TileLink manager. It checks cancellation
  of reserved but unpublished reads while the response consumer stalls; drains
  already-published Gets with real responses returned in reverse order; and
  aborts a faulted translation without publishing a TileLink A request. Memory
  and TLB response inputs are driven by the test, not a real PTW or DRAM.
- `CorrelationLoopRestoreSpec`: drives the actual LoopMatmul command stream,
  acknowledges reservation-station credits and checks all generated A/B load,
  preload and compute addresses. Both loop slots execute custom preloaded-SPAD
  calls with the new one-shot restore bit, then ordinary calls must use their
  original half-SPAD layouts. With the restore bit clear, existing persistent
  funct24 address semantics must remain intact. Output backpressure is random.

The remaining system checks are full-target RTL generation, full-SPAD packed-B
parity, preloaded-dot numerical parity and interleaving with other kernels,
followed by the replay cases and first16 FireSim workload on a newly built target.
