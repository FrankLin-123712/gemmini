package gemmini

import org.chipsalliance.cde.config.{Config, Parameters}
import chisel3._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile.BuildRoCC


object GemminiCustomConfigs {
  // Default configurations
  val defaultConfig = GemminiConfigs.defaultConfig
  val defaultFpConfig = GemminiFPConfigs.defaultFPConfig

  // Create your own configs here
  val baselineInferenceConfig = defaultConfig.copy(
    has_training_convs = false,
  )

  val highPerfInferenceConfig = defaultConfig.copy(
    meshRows = 32,
    meshColumns = 32,

    has_training_convs = false,

    sp_capacity = CapacityInKilobytes(512),
    acc_capacity = CapacityInKilobytes(128),
  )

  val trainingConfig = defaultFpConfig.copy(
    inputType = Float(expWidth = 8, sigWidth = 24),
    accType = Float(expWidth = 8, sigWidth = 24),

    meshRows = 8,
    meshColumns = 8,

    has_training_convs = true,
    has_max_pool =  false,

    sp_capacity = CapacityInKilobytes(512),
    acc_capacity = CapacityInKilobytes(128),
  )

  val ibertInferenceConfig = defaultConfig.copy(
    has_training_convs = false,
    has_max_pool =  false,
    has_normalizations = true,

    acc_capacity = CapacityInKilobytes(128),
  )

  val unifiedMemConfig = defaultConfig.copy(
    has_training_convs = false,
    has_max_pool = false,
    use_tl_ext_mem = true,
    sp_singleported = false,
    spad_read_delay = 8,
    use_shared_ext_mem = true,
    acc_sub_banks = 1
  )

  // ================ Our custome design for running DPVO ================
  val dpvoFP32Dim16Config = GemminiFPConfigs.FP32DefaultConfig.copy(
    // array
    tileRows = 1,
    tileColumns = 1,
    meshRows = 16,
    meshColumns = 16,
    dataflow = Dataflow.WS,

    // on-chip memory
    sp_capacity = CapacityInKilobytes(256),
    acc_capacity = CapacityInKilobytes(64),
    sp_banks = 4,
    acc_banks = 2,
    sp_singleported = false,
    acc_singleported = false,
    acc_sub_banks = -1,

    // timing
    tile_latency = 2,
    mesh_output_delay = 2,
    spad_read_delay = 1,
    acc_latency = 2,

    // DMA / memory
    dma_buswidth = 512,
    dma_maxbytes = 64,
    aligned_to = 1,
    max_in_flight_mem_reqs = 16,

    // TLB
    tlb_size = 4,
    use_tlb_register_filter = true,
    use_shared_tlb = true,

    // queues / reservation station
    ld_queue_length = 8,
    st_queue_length = 2,
    ex_queue_length = 8,
    reservation_station_entries_ld = 8,
    reservation_station_entries_st = 4,
    reservation_station_entries_ex = 16,

    // execute path: keep flexible for first profiling
    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true,
    hardcode_d_to_garbage_addr = false,

    // DNN features
    has_training_convs = false,
    has_max_pool = false,
    has_nonlinear_activations = true,
    has_dw_convs = false,
    has_normalizations = false,
    has_first_layer_optimizations = true,
    has_loop_conv = true,
    has_im2col = true,

    // profiling / integration
    num_counter = 8,
    use_firesim_simulation_counters = true,
    clock_gate = false,
    headerFileName = "gemmini_params_dpvo_dim16.h"
  )

  val dpvoFP32Dim16CorrGatherConfig = dpvoFP32Dim16Config.copy(
    has_corr_gather = true,
    headerFileName = "gemmini_params_dpvo_fp32_dim16_corr_gather.h"
  )

  // Opt-in ABI: existing FP32 configurations and bitstreams stay unchanged.
  val dpvoFP32Dim16NhwcNormConfig = dpvoFP32Dim16Config.copy(
    has_nhwc_norm = true,
    // CONFIG_NORM and CONFIG_IM2COL share subcommand 3. LoopConv implements
    // its own spatial addressing and does not use this standalone unit.
    has_im2col = false,
    headerFileName = "gemmini_params_dpvo_fp32_dim16_nhwc_norm.h"
  )

  // Both engines share one Gemmini. The controller serializes correlation
  // gather against the ordinary execution path, including NHWC normalization.
  val dpvoFP32Dim16CorrGatherNhwcNormConfig = dpvoFP32Dim16NhwcNormConfig.copy(
    has_corr_gather = true,
    headerFileName = "gemmini_params_dpvo_fp32_dim16_corr_gather_nhwc_norm.h"
  )

  val dpvoFP32Dim16SoftAggConfig = dpvoFP32Dim16CorrGatherNhwcNormConfig.copy(
    has_softagg = true,
    headerFileName = "gemmini_params_dpvo_fp32_dim16_softagg.h"
  )

  val dpvoFP16Dim32Config = GemminiFPConfigs.FP16DefaultConfig.copy(
    // array
    tileRows = 1,
    tileColumns = 1,
    meshRows = 32,
    meshColumns = 32,
    dataflow = Dataflow.WS,

    // on-chip memory
    sp_capacity = CapacityInKilobytes(512),
    acc_capacity = CapacityInKilobytes(256),
    sp_banks = 8,
    acc_banks = 4,
    sp_singleported = false,
    acc_singleported = false,
    acc_sub_banks = -1,

    // timing
    tile_latency = 2,
    mesh_output_delay = 2,
    spad_read_delay = 1,
    acc_latency = 2,

    // DMA / memory
    dma_buswidth = 512,
    dma_maxbytes = 64,
    aligned_to = 1,
    max_in_flight_mem_reqs = 32,

    // TLB
    tlb_size = 8,
    use_tlb_register_filter = true,
    use_shared_tlb = true,

    // queues / reservation station
    ld_queue_length = 16,
    st_queue_length = 4,
    ex_queue_length = 16,
    reservation_station_entries_ld = 16,
    reservation_station_entries_st = 8,
    reservation_station_entries_ex = 32,

    // execute path: keep flexible for first profiling
    ex_read_from_spad = true,
    ex_read_from_acc = true,
    ex_write_to_spad = true,
    ex_write_to_acc = true,
    hardcode_d_to_garbage_addr = false,

    // DNN features
    has_training_convs = false,
    has_max_pool = false,
    has_nonlinear_activations = true,
    has_dw_convs = false,
    has_normalizations = false,
    has_first_layer_optimizations = true,
    has_loop_conv = true,
    has_im2col = true,

    // profiling / integration
    num_counter = 8,
    use_firesim_simulation_counters = true,
    clock_gate = false,
    headerFileName = "gemmini_params_dpvo_fp16_dim32.h"
  )
  // ================ Our custome design for running DPVO ================

  // Specify which of your custom configs you want to build here
  val customConfig = dpvoFP32Dim16Config

}


class GemminiCustomConfig[T <: Data : Arithmetic, U <: Data, V <: Data](
  gemminiConfig: GemminiArrayConfig[T,U,V] = GemminiCustomConfigs.customConfig
) extends Config((site, here, up) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      val gemmini = LazyModule(new Gemmini(gemminiConfig))
      gemmini
    }
  )
})
