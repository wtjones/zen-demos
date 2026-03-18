#!/usr/bin/env bash
set -euo pipefail

# research/psx_generate_csv.sh
# Purpose: Generate a CSV of all symbols in the PSX ELF (size,type,name).
# Usage: bash research/psx_generate_csv.sh
# Output: bld_psx/ras_psx_symbols.csv

ELF=bld_psx/ras_psx.elf
OUT=bld_psx/ras_psx_symbols.csv

if [ ! -f "$ELF" ]; then
  echo "ELF not found: $ELF" >&2
  exit 1
fi

readelf -s "$ELF" \
  | awk 'NR>3 {gsub(/"/,"\\\"",$8); printf "\"%d\",\"%s\",\"%s\"\n", $3, $4, $8}' \
  > "$OUT"

echo "Wrote $OUT"
