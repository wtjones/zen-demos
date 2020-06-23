include "map.inc"

SECTION "map vars", WRAM0

map_x: DS 1
map_y: DS 1

SECTION "map code", ROM0

init_map::
    ret

; Resolution of FRAMEBUFFER_WIDTH * FRAMEBUFFER_HEIGHT
; Inputs:
;   d = x value
;   e = y coord
; Outputs:
;   d = tile value
; Destroys:
;   bc
get_map_tile::
    ld      a, d
    ld      [map_x], a
    ld      a, e
    ld      [map_y], a

    ; generate offset for tile address
    ; hl = y
    xor     a
    ld      h, a
    ld      l, e

    ; hl = y * MAP_BYTES_PER_ROW
    add     hl, hl
    add     hl, hl
    add     hl, hl
    add     hl, hl
    add     hl, hl
IF MAP_BYTES_PER_ROW == 64
    add     hl, hl
ENDC

    ; hl = hl + x
    ld      e, d
    ld      d, 0
    add     hl, de

    ; add offset to start of map
    ld      de, map_data
    add     hl, de
    ld      d, [hl]
    ret

map_lookup::
    GENERATE_MAP_LOOKUP
