#!/usr/bin/env bash
set -euo pipefail
# Run from the enclosing Chipyard build; the extra dependency/source filter
# applies only to this sbt invocation and does not edit the shared build files.
cd -- "$(dirname -- "$0")/../../../.."
exec sbt \
  'set gemmini / Test / unmanagedSources := Seq((gemmini / baseDirectory).value / "test/scala/gemmini/CorrelationDmaSpec.scala")' \
  'set gemmini / libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test' \
  'gemmini/testOnly gemmini.CorrelationDmaSpec gemmini.CorrelationStreamReaderSpec gemmini.CorrelationLoopRestoreSpec'
