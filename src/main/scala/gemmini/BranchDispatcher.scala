package gemmini

import chisel3._
import chisel3.util._

/** Internal job identity, independent of the RS's instruction/ROB identifiers.
  * NORMAL jobs are drained epochs; CORR and SOFTAGG jobs are individual RUNs.
  * No new software instruction or completion-token ABI is introduced here.
  */
class BranchJob extends Bundle {
  val id = UInt(32.W)
  val engine = UInt(2.W) // 0 NORMAL, 1 CORR, 2 SOFTAGG
}

class BranchCompletion extends BranchJob {
  val error = Bool()
  val localCommitted = Bool()
  val externalCommitted = Bool()
  val errorDrained = Bool()
}

/** Conservative central admission and ownership. The RS retains all scheduling
  * inside a normal epoch. A special branch acquires the whole local-memory/DMA
  * resource set only after normalDrained, and retains it through completion.
  * Descriptors, local FSMs, numerical operations and the legacy ISA stay in the
  * branch adapters. STATUS and a running CORR's recovery FLUSH bypass admission.
  */
class BranchDispatcher extends Module {
  val io = IO(new Bundle {
    val command = Flipped(Decoupled(UInt(7.W)))
    val normal = Decoupled(UInt(7.W))
    val branches = Vec(2, Decoupled(UInt(7.W)))
    val status = Decoupled(UInt(7.W))
    val abortCorr = Decoupled(UInt(7.W))
    // Includes unrollers, RS, controllers and local/external memory pipelines;
    // excludes the frontend command waiting for admission and branch engines.
    val normalBusy = Input(Bool())
    val fault = Input(Bool())
    val branchBusy = Input(Vec(2, Bool()))
    // A branch may report completion only after every old request has drained.
    val branchCompleted = Flipped(Vec(2, Valid(Bool()))) // bits = error
    val owner = Output(UInt(2.W))
    val busy = Output(Bool())
    val accepted = Valid(new BranchJob)
    val completed = Valid(new BranchCompletion)
    val lastCompletion = Output(new BranchCompletion)
    val acceptedCount = Output(UInt(32.W))
    val completedCount = Output(UInt(32.W))
    val admissionWaitCycles = Output(UInt(32.W))
    val activeCycles = Output(UInt(32.W))
    val rejectedCommands = Output(UInt(32.W))
  })
  val owner = RegInit(0.U(2.W))
  val nextId = RegInit(0.U(32.W))
  val activeId = Reg(UInt(32.W))
  val normalPending = RegInit(false.B)
  val activeError = RegInit(false.B)
  val completedCount = RegInit(0.U(32.W))
  val waitCycles = RegInit(0.U(32.W))
  val activeCycles = RegInit(0.U(32.W))
  val rejected = RegInit(0.U(32.W))
  val lastCompletion = RegInit(0.U.asTypeOf(new BranchCompletion))

  val funct = io.command.bits
  val corr = funct >= GemminiISA.CORR_CONFIG_GEOM && funct <= GemminiISA.CORR_GATHER
  val softagg = funct === GemminiISA.SOFTAGG_CONFIG || funct === GemminiISA.SOFTAGG_RUN
  val status = funct === GemminiISA.CORR_STATUS || funct === GemminiISA.SOFTAGG_STATUS
  val abort = owner === 1.U && funct === GemminiISA.FLUSH_CMD
  val branchCommand = corr || softagg
  val admit = owner === 0.U && !io.normalBusy
  io.normal.valid := io.command.valid && !branchCommand && !status && !abort && owner === 0.U
  io.normal.bits := funct
  for (i <- 0 until 2) {
    io.branches(i).valid := io.command.valid && (if (i == 0) corr else softagg) && (admit || io.fault)
    io.branches(i).bits := funct
  }
  io.status.valid := io.command.valid && status
  io.status.bits := funct
  io.abortCorr.valid := io.command.valid && abort
  io.abortCorr.bits := funct
  io.command.ready := Mux(status, io.status.ready, Mux(abort, io.abortCorr.ready,
    Mux(corr, (admit || io.fault) && io.branches(0).ready,
      Mux(softagg, (admit || io.fault) && io.branches(1).ready,
        owner === 0.U && io.normal.ready))))

  val normalStart = io.normal.fire && !normalPending
  val corrStart = io.branches(0).fire && funct === GemminiISA.CORR_GATHER && !io.fault
  val softaggStart = io.branches(1).fire && funct === GemminiISA.SOFTAGG_RUN && !io.fault
  val branchStart = corrStart || softaggStart
  io.accepted.valid := normalStart || branchStart
  io.accepted.bits.id := nextId
  io.accepted.bits.engine := Mux(corrStart, 1.U, Mux(softaggStart, 2.U, 0.U))

  // A branch command waiting at the head is not normal work. Conversely a
  // normal command presented to an unroller must not be mistaken for a drain.
  val normalDone = normalPending && !io.normalBusy && !io.normal.valid
  val branchDone = (owner === 1.U && io.branchCompleted(0).valid) ||
    (owner === 2.U && io.branchCompleted(1).valid)
  val branchError = Mux(owner === 1.U, io.branchCompleted(0).bits, io.branchCompleted(1).bits)
  val completionError = activeError || io.fault || (branchDone && branchError)
  io.completed.valid := normalDone || branchDone
  io.completed.bits.id := activeId
  io.completed.bits.engine := owner
  io.completed.bits.error := completionError
  io.completed.bits.localCommitted := !completionError
  io.completed.bits.externalCommitted := !completionError
  io.completed.bits.errorDrained := completionError

  when(io.completed.valid) {
    lastCompletion := io.completed.bits
    completedCount := completedCount + 1.U
    normalPending := false.B
    owner := 0.U
    activeError := false.B
  }
  when((normalPending || owner =/= 0.U) && io.fault) { activeError := true.B }
  when(io.accepted.valid) {
    nextId := nextId + 1.U
    activeId := nextId
    activeError := io.fault
    normalPending := normalStart
    owner := io.accepted.bits.engine
  }
  when(io.command.valid && branchCommand && !io.command.ready) { waitCycles := waitCycles + 1.U }
  when(normalPending || owner =/= 0.U) { activeCycles := activeCycles + 1.U }
  when(io.command.fire && branchCommand && io.fault) { rejected := rejected + 1.U }

  io.owner := owner
  io.busy := owner =/= 0.U
  io.lastCompletion := lastCompletion
  io.acceptedCount := nextId
  io.completedCount := completedCount
  io.admissionWaitCycles := waitCycles
  io.activeCycles := activeCycles
  io.rejectedCommands := rejected
  for (i <- 0 until 2) {
    assert(!io.branchBusy(i) || owner === (i + 1).U, "branch used memory without ownership")
    when(io.branchCompleted(i).valid) {
      assert(owner === (i + 1).U, "completion without the branch's outstanding job")
      assert(!io.branchBusy(i), "branch completion preceded local/external drain")
    }
  }
  assert(!(io.branchBusy(0) && io.branchBusy(1)), "exclusive branches overlapped")
}
