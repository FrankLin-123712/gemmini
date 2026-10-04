package chipyard

import org.chipsalliance.cde.config.Config

// ------------------------------
// Configs with Gemmini RoCC
// ------------------------------

// DOC include start: GemminiRocketConfig
class GemminiRocketConfig extends Config(
  new gemmini.DefaultGemminiConfig ++                            // use Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)
// DOC include end: GemminiRocketConfig

class FPGemminiRocketConfig extends Config(
  new gemmini.GemminiFP32DefaultConfig ++                         // use FP32Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class LeanGemminiRocketConfig extends Config(
  new gemmini.LeanGemminiConfig ++                                 // use Lean Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class LeanGemminiPrintfRocketConfig extends Config(
  new gemmini.LeanGemminiPrintfConfig ++                                 // use Lean Gemmini systolic array GEMM accelerator
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(128) ++
  new chipyard.config.AbstractConfig)

class ReRoCCManyGemminiConfig extends Config(
  new rerocc.WithReRoCC ++
  new gemmini.LeanGemminiConfig ++                              // rerocc tile3 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile2 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile1 is gemmini
  new gemmini.LeanGemminiConfig ++                              // rerocc tile0 is gemmini
  new freechips.rocketchip.rocket.WithNHugeCores(4) ++           // 4 rocket cores
  new chipyard.config.AbstractConfig)

class GemminiShuttleConfig extends Config(
  new gemmini.DefaultGemminiConfig ++                            // use Gemmini systolic array GEMM accel
  new shuttle.common.WithNShuttleCores ++
  new chipyard.config.AbstractConfig)

// ============= Our custom GemminiRocket chip config for running DPVO ===============
class DPVOFP32Dim16GemminiRocketConfig extends Config(
  new gemmini.GemminiCustomConfig ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class DPVOFP32Dim16CorrGatherGemminiRocketConfig extends Config(
  new gemmini.GemminiCustomConfig(gemmini.GemminiCustomConfigs.dpvoFP32Dim16CorrGatherConfig) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class DPVOFP32Dim16NhwcNormGemminiRocketConfig extends Config(
  new gemmini.GemminiCustomConfig(gemmini.GemminiCustomConfigs.dpvoFP32Dim16NhwcNormConfig) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class DPVOFP32Dim16CorrGatherNhwcNormGemminiRocketConfig extends Config(
  new gemmini.GemminiCustomConfig(gemmini.GemminiCustomConfigs.dpvoFP32Dim16CorrGatherNhwcNormConfig) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)

class DPVOFP16Dim32GemminiRocketConfig extends Config(
  new gemmini.GemminiCustomConfig(gemmini.GemminiCustomConfigs.dpvoFP16Dim32Config) ++
  new freechips.rocketchip.rocket.WithNHugeCores(1) ++
  new chipyard.config.WithSystemBusWidth(512) ++
  new chipyard.config.AbstractConfig)
// ============= Our custom GemminiRocket chip config for running DPVO ===============
