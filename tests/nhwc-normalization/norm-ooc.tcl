# Identical constraints for before/after local implementation comparisons.
# The 2 ns I/O budget is an experiment assumption, not a SoC timing model.
# A passing result here does not establish full FireSim timing closure.
if {$argc != 2} {
  error "expected SystemVerilog path and clock period in ns"
}
set rtl_file [lindex $argv 0]
set clock_period [lindex $argv 1]
if {![string is double -strict $clock_period] || $clock_period <= 0} {
  error "clock period must be positive"
}
set_param general.maxThreads 4
create_project -in_memory -part xcu200-fsgd2104-2-e
set_property verilog_define SYNTHESIS [current_fileset]
read_verilog -sv $rtl_file
set constraints [open norm-ooc.xdc w]
puts $constraints "create_clock -name norm_clock -period $clock_period \[get_ports clock\]"
puts $constraints {set_clock_uncertainty 0.067 [get_clocks norm_clock]}
puts $constraints {set_input_delay -clock norm_clock -max 2.0 [remove_from_collection [all_inputs] [get_ports clock]]}
puts $constraints {set_input_delay -clock norm_clock -min 0.0 [remove_from_collection [all_inputs] [get_ports clock]]}
puts $constraints {set_output_delay -clock norm_clock -max 2.0 [all_outputs]}
puts $constraints {set_output_delay -clock norm_clock -min 0.0 [all_outputs]}
close $constraints
read_xdc norm-ooc.xdc
synth_design -top NhwcNormalizer -part xcu200-fsgd2104-2-e -mode out_of_context -flatten_hierarchy rebuilt
report_utilization -file utilization_synth.rpt
report_timing_summary -delay_type min_max -max_paths 10 -report_unconstrained -file timing_synth.rpt
write_checkpoint -force synthesized.dcp
opt_design
place_design
phys_opt_design -directive AggressiveExplore
route_design
phys_opt_design -directive AggressiveExplore
report_utilization -file utilization_routed.rpt
report_timing_summary -delay_type min_max -max_paths 10 -report_unconstrained -file timing_routed.rpt
report_timing -delay_type max -from [all_registers -clock norm_clock] -to [all_registers -clock norm_clock] -max_paths 10 -file timing_registers.rpt
report_timing -delay_type min -from [all_registers -clock norm_clock] -to [all_registers -clock norm_clock] -max_paths 10 -file timing_registers_hold.rpt
write_checkpoint -force routed.dcp
