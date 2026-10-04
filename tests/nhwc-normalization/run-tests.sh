#!/usr/bin/env bash
set -euo pipefail
# Only this invocation selects the focused sources and adds ChiselTest.
cd -- "$(dirname -- "$0")/../../../.."
# ChiselTest 6 and some newer Verilator/GCC packages disagree on the generated
# precompiled-header flags. A single C++ translation unit avoids that mismatch.
export MAKEFLAGS="${MAKEFLAGS:-} VM_PARALLEL_BUILDS=0"
exec sbt -Dsbt.supershell=false -Dsbt.server.autostart=false \
  'set gemmini / libraryDependencies ++= Seq("edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test, "org.scalatest" %% "scalatest" % "3.2.19" % Test)' \
  'set gemmini / Test / unmanagedSources := Seq(file("generators/gemmini/tests/nhwc-normalization/NhwcNormalizerSpec.scala"))' \
  'gemmini/testOnly gemmini.NhwcNormalizerSpec' \
  'chipyard/compile' \
  'firechip/compile'
