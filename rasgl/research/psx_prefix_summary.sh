#!/usr/bin/env bash
set -euo pipefail

# research/psx_prefix_summary.sh
# Purpose: Print a heuristic per-prefix summary of symbol sizes to help
# identify which subsystems (core, pack, render, dma, scene, etc.) dominate.
# Usage: bash research/psx_prefix_summary.sh

ELF=bld_psx/ras_psx.elf
TMP=bld_psx/psx_syms.txt
TMP_SECS=bld_psx/psx_secs.txt

if [ ! -f "$ELF" ]; then
  echo "ELF not found: $ELF" >&2
  exit 1
fi

mkdir -p "$(dirname "$TMP")"

readelf -s "$ELF" | sed -n '4,$p' > "$TMP"
readelf -S "$ELF" > "$TMP_SECS"

python3 research/psx_prefix_summary.py "$TMP" "$TMP_SECS"
