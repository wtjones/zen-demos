#!/usr/bin/env python3
from collections import defaultdict
import re
import sys


def read_symbols(path):
    groups = defaultdict(int)
    try:
        with open(path) as f:
            for l in f:
                parts = l.split()
                if len(parts) < 8:
                    continue
                try:
                    size = int(parts[2])
                except Exception:
                    continue
                name = parts[7].split('[')[0]
                prefix = name.split('_')[0].split('.')[0]
                groups[prefix] += size
    except FileNotFoundError:
        print(f"Symbols file not found: {path}", file=sys.stderr)
        sys.exit(1)
    return groups

def read_sections(path):
    sec_sizes = {}
    # matches lines like: "  [ 1] .text PROGBITS 80010000 001000 008b54 ..."
    pattern = re.compile(r'^\s*\[\s*\d+\]\s+(\S+)\s+\S+\s+[0-9A-Fa-f]+\s+[0-9A-Fa-f]+\s+([0-9A-Fa-f]+)')
    try:
        with open(path) as f:
            for l in f:
                m = pattern.match(l)
                if not m:
                    continue
                name = m.group(1)
                try:
                    size = int(m.group(2), 16)
                except Exception:
                    continue
                sec_sizes[name] = sec_sizes.get(name, 0) + size
    except FileNotFoundError:
        print(f"Sections file not found: {path}", file=sys.stderr)
        sys.exit(1)
    return sec_sizes

def main():
    import argparse
    p = argparse.ArgumentParser(description='Summarize PSX ELF symbol and section sizes')
    p.add_argument('syms', help='path to symbol output (from readelf -s)')
    p.add_argument('secs', help='path to section output (from readelf -S)')
    args = p.parse_args()

    groups = read_symbols(args.syms)
    sym_total = sum(groups.values())
    for k, v in sorted(groups.items(), key=lambda kv: kv[1], reverse=True):
        print(f"{v:8d} {k}")
    print(f"{sym_total:8d} SYMBOLS_TOTAL")

    sec_sizes = read_sections(args.secs)
    text_sz = sec_sizes.get('.text', 0)
    data_sz = sec_sizes.get('.data', 0)
    bss_sz = sec_sizes.get('.bss', 0)
    sec_total = text_sz + data_sz + bss_sz
    print(f"{text_sz:8d} .text")
    print(f"{data_sz:8d} .data")
    print(f"{bss_sz:8d} .bss")
    print(f"{sec_total:8d} SECTIONS_TOTAL")

    rodata_sz = sec_sizes.get('.rodata', 0)
    sdata_sz = sec_sizes.get('.sdata', 0)
    sbss_sz = sec_sizes.get('.sbss', 0)
    app_text = text_sz + rodata_sz
    app_data = data_sz + sdata_sz
    app_bss = bss_sz + sbss_sz
    app_total = app_text + app_data + app_bss
    print("")
    print(f"{app_text:8d} APP.text (text + .rodata)")
    print(f"{app_data:8d} APP.data (data + .sdata)")
    print(f"{app_bss:8d} APP.bss (bss + .sbss)")
    print(f"{app_total:8d} APP_TOTAL")

if __name__ == '__main__':
    main()
