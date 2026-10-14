package gemmini

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class BranchDispatcherSpec extends AnyFlatSpec with ChiselScalatestTester {
  private def initialize(d: BranchDispatcher): Unit = {
    d.io.command.valid.poke(false.B); d.io.command.bits.poke(0.U)
    d.io.normal.ready.poke(true.B); d.io.status.ready.poke(true.B)
    d.io.abortCorr.ready.poke(true.B)
    d.io.normalBusy.poke(false.B); d.io.fault.poke(false.B)
    for (i <- 0 until 2) {
      d.io.branches(i).ready.poke(true.B); d.io.branchBusy(i).poke(false.B)
      d.io.branchCompleted(i).valid.poke(false.B); d.io.branchCompleted(i).bits.poke(false.B)
    }
    d.clock.step()
  }
  private def present(d: BranchDispatcher, funct: Int): Unit = {
    d.io.command.valid.poke(true.B); d.io.command.bits.poke(funct.U)
  }
  private def complete(d: BranchDispatcher, branch: Int, error: Boolean = false): Unit = {
    d.io.branchBusy(branch).poke(false.B)
    d.io.branchCompleted(branch).valid.poke(true.B)
    d.io.branchCompleted(branch).bits.poke(error.B)
    d.io.completed.valid.expect(true.B)
    d.io.completed.bits.engine.expect((branch + 1).U)
    d.io.completed.bits.error.expect(error.B)
    d.io.completed.bits.localCommitted.expect((!error).B)
    d.io.completed.bits.externalCommitted.expect((!error).B)
    d.io.completed.bits.errorDrained.expect(error.B)
    d.clock.step()
    d.io.branchCompleted(branch).valid.poke(false.B)
  }

  it should "keep normal RS traffic concurrent and join its full drain before a branch" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      present(d, 2) // mvin, first normal epoch
      d.io.accepted.valid.expect(true.B); d.io.accepted.bits.id.expect(0.U)
      d.clock.step(); d.io.normalBusy.poke(true.B)
      present(d, 6) // normal busy must not serialize PRELOAD behind mvin
      d.io.normal.valid.expect(true.B); d.io.command.ready.expect(true.B)
      d.io.accepted.valid.expect(false.B); d.clock.step()
      present(d, 43)
      d.io.command.ready.expect(false.B); d.io.branches(0).valid.expect(false.B)
      d.clock.step(7) // e.g. RS retired but DMA/local pipeline still draining
      d.io.completedCount.expect(0.U)
      d.io.normalBusy.poke(false.B)
      d.io.completed.valid.expect(true.B); d.io.completed.bits.id.expect(0.U)
      d.io.branches(0).valid.expect(true.B)
      d.io.accepted.bits.id.expect(1.U); d.clock.step()
      d.io.branchBusy(0).poke(true.B); d.io.command.valid.poke(false.B)
      d.io.owner.expect(1.U); d.io.completedCount.expect(1.U)
      d.clock.step(3)
      complete(d, 0)
      d.io.owner.expect(0.U); d.io.completedCount.expect(2.U)
      d.io.lastCompletion.id.expect(1.U); d.io.admissionWaitCycles.expect(7.U)
    }
  }

  it should "retain exclusive ownership through branch drain and status backpressure" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      present(d, 46); d.clock.step(); d.io.branchBusy(1).poke(true.B)
      present(d, 40); d.io.command.ready.expect(false.B)
      d.io.branches(0).valid.expect(false.B); d.clock.step(4)
      present(d, 47); d.io.status.ready.poke(false.B)
      d.io.status.valid.expect(true.B); d.io.command.ready.expect(false.B)
      d.clock.step(3); d.io.owner.expect(2.U)
      d.io.status.ready.poke(true.B); d.clock.step()
      present(d, 2); d.io.normal.valid.expect(false.B)
      d.io.command.ready.expect(false.B); d.clock.step(2)
      complete(d, 1)
      d.io.normal.valid.expect(true.B); d.io.command.ready.expect(true.B)
      d.io.lastCompletion.id.expect(0.U)
    }
  }

  it should "hold ownership across an idle gap until an explicit completion arrives" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      present(d, 43); d.clock.step()
      // Invalid descriptors and delayed drain acknowledgements need not assert
      // busy at all. Busy falling is not the central completion contract.
      present(d, 46); d.io.command.ready.expect(false.B)
      d.clock.step(5); d.io.completedCount.expect(0.U)
      complete(d, 0, error = true)
      d.io.command.ready.expect(true.B)
    }
  }

  it should "let fault rejection and Corr FLUSH advance without releasing DMA ownership" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      present(d, 43); d.clock.step(); d.io.branchBusy(0).poke(true.B)
      d.io.fault.poke(true.B)
      present(d, 45); d.io.branches(1).valid.expect(true.B)
      d.io.command.ready.expect(true.B); d.io.accepted.valid.expect(false.B)
      d.clock.step()
      present(d, 43); d.io.command.ready.expect(true.B)
      d.io.accepted.valid.expect(false.B); d.clock.step()
      present(d, 7); d.io.abortCorr.valid.expect(true.B)
      d.io.normal.valid.expect(false.B); d.clock.step()
      d.io.fault.poke(false.B); d.io.command.valid.poke(false.B)
      d.clock.step(4); d.io.owner.expect(1.U); d.io.rejectedCommands.expect(2.U)
      complete(d, 0, error = true)
      d.io.acceptedCount.expect(1.U); d.io.completedCount.expect(1.U)
    }
  }

  it should "reject branch commands behind faulted normal DMA and permit its normal FLUSH" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      present(d, 2); d.clock.step(); d.io.normalBusy.poke(true.B)
      d.io.fault.poke(true.B)
      for (funct <- Seq(40, 41, 42, 43, 45, 46)) {
        present(d, funct); d.io.command.ready.expect(true.B)
        d.io.accepted.valid.expect(false.B); d.clock.step()
      }
      present(d, 7); d.io.normal.valid.expect(true.B); d.clock.step()
      d.io.command.valid.poke(false.B); d.io.fault.poke(false.B)
      d.io.normalBusy.poke(false.B)
      d.io.completed.valid.expect(true.B); d.io.completed.bits.errorDrained.expect(true.B)
      d.clock.step(); d.io.rejectedCommands.expect(6.U)
      d.io.completedCount.expect(1.U)
    }
  }

  it should "not treat configuration or status as a special branch job" in {
    test(new BranchDispatcher) { d =>
      initialize(d)
      for (funct <- Seq(40, 41, 42, 44, 45, 47)) {
        present(d, funct); d.io.command.ready.expect(true.B)
        d.io.accepted.valid.expect(false.B); d.clock.step()
      }
      d.io.acceptedCount.expect(0.U); d.io.completedCount.expect(0.U)
    }
  }
}
