#!/usr/bin/env bash
set -euo pipefail

# Consume the self-contained SystemVerilog emitted by NhwcNormalizerSpec.
# This does not elaborate the SoC or generate/program a bitstream.
if [[ $# -lt 2 || $# -gt 3 ]]; then
  echo "usage: $0 NhwcNormalizer.sv output-directory [clock-period-ns]" >&2
  exit 2
fi
rtl_file=$(realpath -- "$1")
output_dir=$(realpath -m -- "$2")
clock_period=${3:-16.665}
script_dir=$(cd -- "$(dirname -- "$0")" && pwd)
vivado_bin=${VIVADO:-vivado}
mkdir -p -- "$output_dir"
cp -- "$rtl_file" "$output_dir/NhwcNormalizer.input.sv"
sha256sum "$output_dir/NhwcNormalizer.input.sv" > "$output_dir/input.sha256"
cd -- "$output_dir"
exec "$vivado_bin" -mode batch -source "$script_dir/norm-ooc.tcl" \
  -log vivado.log -journal vivado.jou \
  -tclargs "$output_dir/NhwcNormalizer.input.sv" "$clock_period"
