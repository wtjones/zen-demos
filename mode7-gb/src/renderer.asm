INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

SECTION "renderer vars", WRAM0

point_x:: DS 1
point_y:: DS 1
screen_x:: DS 1
screen_y:: DS 1
current_tile:: DS 1

SECTION "renderer code", ROM0

render::
    xor     a
    ld      a, [viewer_x]       ; this is not correct, at least as
    ld      [screen_x], a       ; far as naming
    xor     a
    ld      a, [viewer_y]
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
    push    de

    ;xor     a
    ;ld      [current_tile], a

    ;
    ; top-left sub-tile
    ;
    ld      a, [screen_x]
    ld      d, a
    ld      b, a
    ld      a, [screen_y]
    ld      e, a
    ld      c, a

    call    get_map_tile   ; d = map tile

    ; set bit %00000001 if d = 1
    ld      a, d
    ld      [current_tile], a

    ;
    ; top-right sub-tile
    ;
    ; screen_x++
    inc     b
    ld      d, b
    ld      e, c

    call    get_map_tile   ; d = map tile

    ; set bit %00000010 if d = 1
    ld      a, d
    sla     a
    ld      hl, current_tile
    or      a, [hl]
    ld      [current_tile], a

    ;
    ; bottom-left sub-tile
    ;
    ; screen_x++
    dec     b
    ld      d, b
    ; screen_y--
    inc     c
    ld      e, c

    call    get_map_tile   ; d = map tile

    ; set bit %00000100 if d = 1
    ld      a, d
    sla     a
    sla     a
    ld      hl, current_tile
    or      a, [hl]
    ld      [current_tile], a

    ;
    ; bottom-right sub-tile
    ;
    ; screen_x++
    inc     b
    ld      d, b
    ld      e, c

    call    get_map_tile   ; d = map tile

    ; set bit %00001000 if d = 1
    ld      a, d
    sla     a
    sla     a
    sla     a
    ld      hl, current_tile
    or      a, [hl]

    ld      [current_tile], a

    ; move screen_x to top-left of next tile
    inc     b
    ld      a, b
    ld      [screen_x], a

    ; move screen_y to top left of next tile
    dec     c
    ld      a, c
    ld      [screen_y], a

    pop     de
    pop     bc

    ; write tile result to command buffer
    ld      a, [current_tile]
    ;ld      a, %00001101
    ld      [de], a
    inc     de  ; move to next byte in command buffer
.skip_inner
    dec     b
    jr      nz, .loop_inner

    ; end of vram tile row - move sub-tile to top-left of next row
    ld      a, [viewer_x]   ; TODO fix
    ld      [screen_x], a
    xor     a
    ld      a, [screen_y]
    add     2
    ld      [screen_y], a
.skip_outer
    dec     c
    jr      nz, .loop_outer
    ret
