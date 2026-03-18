#!/usr/bin/env bash
set -euo pipefail

# research/psx_list_objects.sh
# Purpose: Print the top OBJECT symbols (size and name) from the PSX ELF
# and print the total byte count across all OBJECT symbols.
# Usage: bash research/psx_list_objects.sh

ELF=bld_psx/ras_psx.elf

if [ ! -f "$ELF" ]; then
  echo "ELF not found: $ELF" >&2
  exit 1
fi

# Print top OBJECT symbols (size, name) to stdout
readelf -s "$ELF" \
  | awk 'NR>3 && $4=="OBJECT" {printf "%d %s\n", $3, $8}' \
  | sort -nr | head -n 40

# Compute total bytes across all OBJECT symbols and print it
total_bytes=$(readelf -s "$ELF" | awk 'NR>3 && $4=="OBJECT" {sum+=$3} END{print sum+0}')
printf "\nTOTAL_BYTES %d\n" "$total_bytes"
