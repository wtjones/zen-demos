import math
import sys
import pprint


def fixed_8_8_to_8(n):
    return n >> 8


def float_to_fixed_8_8(n):
    whole = int(n)
    frac = n - whole
    fixed_frac = int(frac * 256)
    fixed_8_8 = whole * 256 + fixed_frac
    return fixed_8_8


def multiply_8_by_fixed_8_8(int_8, fixed_8_8):
    f = int_8
    return fixed_8_8 * f


if __name__ == "__main__":
    original_float = 1.3
    f = float_to_fixed_8_8(original_float)
    print(f"float {original_float} as fixed: {f:016b}")
    m = multiply_8_by_fixed_8_8(10, f)
    print(f"multiply_8_by_fixed_8_8 result: {m:016b}")
    print(f"result: {m >> 8}")
