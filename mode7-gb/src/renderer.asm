INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

SECTION "renderer vars", WRAM0

point_x:: DS 1
point_y:: DS 1
screen_x:: DS 1
screen_y:: DS 1

SECTION "renderer code", ROM0

render::
    xor     a
    ld      [screen_x], a
    ld      [screen_y], a
    ld      de, command_list
    ld      c, SCRN_Y_B / 2     ; render half of the screen
    inc     c
    jr      .skip_outer

.loop_outer
    ld      b, SCRN_X_B
    inc     b
    jr      .skip_inner

.loop_inner
    push    bc

    ld      a, [de]
    inc     a
    and     %00001111
    ld      [de], a
    inc     de
    ;DBGMSG "set to buffer"
;     jr      .skip_outer
;     ld      a, [screen_x]
;     ld      a, b
;     ld      a, [point_x]
;     cp      b
;     jr      z, .skip_point  ; if a = 0
;     ld      a, 15           ; draw the point
;     jr      .skip_tile
; .skip_point
;     ld      a, 0            ; draw empty
; .skip_tile
;     ld      [hl+], a

;     ld      a, [screen_x]

;     inc     a
;     ld      [screen_x], a

;     ld      [screen_y], a
;     inc     a
;     ld

    pop     bc
.skip_inner
    dec     b
    jr      nz, .loop_inner

.skip_outer
    dec     c
    jr      nz, .loop_outer
    ret
