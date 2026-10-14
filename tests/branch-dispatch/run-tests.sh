#!/usr/bin/env bash
set -euo pipefail
# Phase-1 control tests plus the numerical/local-memory regressions whose
# admission paths changed. This runs RTL simulations, never an FPGA build.
cd -- "$(dirname -- "$0")/../../../.."
export MAKEFLAGS="${MAKEFLAGS:-} VM_PARALLEL_BUILDS=0"
exec sbt --batch --no-server -J-Xmx8G -J-XX:ActiveProcessorCount=8 \
  -Dsbt.server.forcestart=true -Dsbt.offline=true -Dsbt.supershell=false \
  'set gemmini / libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test, "org.scalatest" %% "scalatest" % "3.2.19" % Test)' \
  'set gemmini / Test / unmanagedSources := Seq("tests/branch-dispatch/BranchDispatcherSpec.scala", "tests/branch-dispatch/BranchControllerSpec.scala", "tests/softagg/SoftAggSpec.scala", "tests/softagg/SoftAggScratchpadSpec.scala", "tests/nhwc-normalization/NhwcNormalizerSpec.scala", "tests/correlation-gather/src/test/scala/CorrelationGatherSpec.scala", "src/test/scala/gemmini/CorrelationDmaSpec.scala").map(p => file("generators/gemmini") / p)' \
  'set gemmini / Test / parallelExecution := false' \
  'gemmini/testOnly gemmini.BranchDispatcherSpec gemmini.BranchControllerSpec gemmini.SoftAggSpec gemmini.SoftAggScratchpadSpec gemmini.NhwcNormalizerSpec gemmini.CorrelationGatherSpec gemmini.CorrelationDmaSpec gemmini.CorrelationStreamReaderSpec gemmini.CorrelationLoopRestoreSpec'
