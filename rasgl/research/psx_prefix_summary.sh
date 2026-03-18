#!/usr/bin/env bash
set -euo pipefail

# research/psx_prefix_summary.sh
# Purpose: Print a heuristic per-prefix summary of symbol sizes to help
# identify which subsystems (core, pack, render, dma, scene, etc.) dominate.
# Usage: bash research/psx_prefix_summary.sh

ELF=bld_psx/ras_psx.elf
TMP=/tmp/psx_syms.txt

if [ ! -f "$ELF" ]; then
  echo "ELF not found: $ELF" >&2
  exit 1
fi

readelf -s "$ELF" | sed -n '4,$p' > "$TMP"

python3 - <<'PY'
from collections import defaultdict
groups=defaultdict(int)
with open('/tmp/psx_syms.txt') as f:
    for l in f:
        parts=l.split()
        if len(parts)<8: continue
        try:
            size=int(parts[2])
        except:
            continue
        name=parts[7].split('[')[0]
        prefix=name.split('_')[0].split('.')[0]
        groups[prefix]+=size
for k,v in sorted(groups.items(), key=lambda kv: kv[1], reverse=True):
    print(f"{v:8d} {k}")
PY
