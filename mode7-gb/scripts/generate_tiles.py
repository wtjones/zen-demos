"""Produces a monochrome tile set of 16 tiles of 4 quadrants each.

A VRAM tile is 8 rows of 2 bytes = 16 bytes, so this output should be
doubled upon load.
"""
#
result = []
bytes_per_tile = 8

for tile in range(16):
    byte = 0
    byte = byte | 0b11110000 if 0b00000001 & tile > 0 else byte
    byte = byte | 0b00001111 if 0b00000010 & tile > 0 else byte
    result.extend([byte] * 4)

    byte = 0
    byte = byte | 0b11110000 if 0b00000100 & tile > 0 else byte
    byte = byte | 0b00001111 if 0b00001000 & tile > 0 else byte
    result.extend([byte] * 4)

tile = 0
print(f"; tile {tile}")
for i, byte in enumerate(result):
    print("    DB $" + format(byte, "02x"))

    if i + 1 > 0 and (i + 1) % bytes_per_tile == 0:
        tile += 1
        print("\n")
        print(f"; tile {tile}")
