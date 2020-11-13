
INCLUDE	"map.inc"

SECTION "viewer vars", WRAM0

viewer_x:: DS 1
viewer_y:: DS 1
viewer_angle:: DS 1

SECTION "viewer code", ROM0

update_viewer::
    ; ld      a, [viewer_x]
    ; inc     a
    ; ld      [viewer_x], a
    ld      a, [viewer_angle]
    add     4
    ld      [viewer_angle], a
    ret
