
INCLUDE	"map.inc"

SECTION "viewer vars", WRAM0

viewer_x:: DS 1
viewer_y:: DS 1

SECTION "viewer code", ROM0

update_viewer::
    ld      a, [viewer_x]
    inc     a
    cp      a, MAP_WIDTH
    jr      c, .skip_wrap_viewer    ; if MAP_WIDTH <= x
    xor     a
.skip_wrap_viewer                   ; end ifs
    ld      [viewer_x], a
    ret
