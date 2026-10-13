#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "$0")/../../../.."
export MAKEFLAGS="${MAKEFLAGS:-} VM_PARALLEL_BUILDS=0"
# sbt 1.8.2 supports continuing without its boot socket when IPC is unavailable
# (Main.scala getSocketOrExit / SysProp.forceServerStart). Keep the command server
# disabled and use cached dependencies only; this does not require network/IPC.
exec sbt --batch --no-server -J-Xmx8G -J-XX:ActiveProcessorCount=8 \
  -Dsbt.server.forcestart=true -Dsbt.offline=true -Dsbt.supershell=false \
  'set gemmini / libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test, "org.scalatest" %% "scalatest" % "3.2.19" % Test)' \
  'set gemmini / Test / unmanagedSources := Seq(file("generators/gemmini/tests/softagg/SoftAggSpec.scala"), file("generators/gemmini/tests/softagg/SoftAggScratchpadSpec.scala"))' \
  'gemmini/testOnly gemmini.SoftAggSpec gemmini.SoftAggScratchpadSpec'
