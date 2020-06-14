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
;   hl = address of pixel in tilemap
; Destroys:
;   bc
get_map_addr::
    ld      a, d
    ld      [map_x], a
    ld      a, e
    ld      [map_y], a

    ; get start of y row addr = MAP_WIDTH * y
    ;
    ret

map_lookup::
    GENERATE_MAP_LOOKUP
