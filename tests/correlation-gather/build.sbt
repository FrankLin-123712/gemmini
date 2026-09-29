// Isolated test project: avoids the unrelated legacy iotesters sources and does
// not require elaborating Rocket/Chipyard. Versions match the enclosing Chipyard.
scalaVersion := "2.13.16"
libraryDependencies ++= Seq(
  "org.chipsalliance" %% "chisel" % "6.7.0",
  "edu.berkeley.cs" %% "chiseltest" % "6.0.0" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test)
addCompilerPlugin("org.chipsalliance" % "chisel-plugin" % "6.7.0" cross CrossVersion.full)
Compile / unmanagedSources := {
  val rtl = baseDirectory.value / "../../src/main/scala/gemmini"
  Seq(rtl / "CorrelationGatherController.scala", rtl / "CorrelationRowPacker.scala")
}
Test / parallelExecution := false
