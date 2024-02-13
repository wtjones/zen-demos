import math
import sys
import pprint


def fixed_8_8_to_int_8(n):
    return n >> 8


def int_8_to_fixed_8_8(n):
    return n << 8


def float_to_fixed_8_8(n):
    whole = int(n)
    frac = n - whole
    fixed_frac = int(frac * 256)
    fixed_8_8 = whole * 256 + fixed_frac
    return fixed_8_8

def fixed_16_16_to_float(n):
    whole = n / 65536
    fixed_frac = n - (whole * 65536)
    return float(whole + float(fixed_frac) / 65536);

def multiply_8_by_fixed_8_8(int_8, fixed_8_8):
    f = int_8
    return fixed_8_8 * f


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print("Mode not provided. Running example...")
        original_float =  1.3
        f = float_to_fixed_8_8(original_float)
        print(f"float {original_float} as fixed: {f:016b}")
        m = multiply_8_by_fixed_8_8(10, f)
        print(f"multiply_8_by_fixed_8_8 result: {m:016b}")
        print(f"result: {m >> 8}")
        sys.exit(0)

    mode = sys.argv[1]
    if mode == "10":
        print(f"mode {mode}: float to fixed 8.8...")
        original_float =  float(sys.argv[2])
        f = float_to_fixed_8_8(original_float)
        print(f"float {original_float} as fixed: {f:016b}")
        print(f"result: {f}")
    elif mode == "20":
        print(f"mode {mode}: int to fixed 16.16...")
        original_fixed = int(sys.argv[2])
        print(f"input: {original_fixed}")
        print(f"input {original_fixed} as fixed: {original_fixed:032b}")
        f = fixed_16_16_to_float(original_fixed)
        print(f"result: {f}")
    else:
        print("mode not supported")
