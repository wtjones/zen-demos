IF MODE == 0

INCLUDE	"gbhw.inc"
INCLUDE "math.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

SECTION "renderer vars", WRAM0

; Sub-tiles are the 2x2 "pixels" of each BG tile
DEF SUB_TILE_X      EQU SCRN_X_B * 2
DEF SUB_TILE_Y      EQU SCRN_Y_B * 2

screen_x:: DS 1
screen_y:: DS 1
peek_x:: DS 1
peek_y:: DS 1
current_tile:: DS 1     ; the tile to be appended to command buffer
render_buffer: DS SUB_TILE_X * SUB_TILE_Y

peek_row_x_whole: DS 1
peek_row_x_frac: DS 1

peek_row_y_whole: DS 1
peek_row_y_frac: DS 1

peek_x_whole: DS 1
peek_x_frac: DS 1

peek_y_whole: DS 1
peek_y_frac: DS 1

SECTION "renderer code", ROM0

render::
    call set_rotation
    call render_to_buffer
    call render_to_command_list
    ret

render_to_buffer:
    xor     a
    ld      [screen_x], a
    ld      [screen_y], a

    ld      a, [viewer_x]       ; this is not correct, at least as
    ld      [peek_x], a         ; far as naming

    ld      a, [viewer_y]
    ld      [peek_y], a

    ;
    ; Translate the first sub-tile by adding the viewer and rotated lookup
    ;
    ; Convert viewer x to fixed 8.8
    ld      a, [viewer_x]
    ld      d, a
    ld      e, 0

    ; peek_x = rotated_x + viewer_x
    ADD_N16_N16_DE \
        peek_x_whole, \
        peek_x_frac, \
        rotated_x_whole, \
        rotated_x_frac

    ; Convert viewer y to fixed 8.8
    ld      a, [viewer_y]
    ld      d, a
    ld      e, 0

    ; peek_y = rotated_y + viewer_y
    ADD_N16_N16_DE \
        peek_y_whole, \
        peek_y_frac, \
        rotated_y_whole, \
        rotated_y_frac

    ld      de, render_buffer

    ld      c, SUB_TILE_Y     ; Outer loop on sub-tiles (4x4 pixels).
    inc     c
    jp      .skip_outer

.loop_outer

    ; copy to a separate peek var for row iteration
    ld      a, [peek_x_whole]
    ld      [peek_row_x_whole], a
    ld      a, [peek_x_frac]
    ld      [peek_row_x_frac], a
    ld      a, [peek_y_whole]
    ld      [peek_row_y_whole], a
    ld      a, [peek_y_frac]
    ld      [peek_row_y_frac], a

    ld      b, SUB_TILE_X       ; Inner loop on sub-tiles (4x4 pixels).
    inc     b
    jr      .skip_inner

.loop_inner
    push    bc
    push    de

    ; use the whole part for the map lookup
    ld      a, [peek_row_x_whole]
    ld      d, a
    ld      a, [peek_row_y_whole]
    ld      e, a

    call    get_map_tile    ; d = map tile
    ld      a, d
    ld      [current_tile], a

    ;
    ; increment peek x by delta from rotation table
    ;
    ld      a, [delta_x_whole]
    ld      d, a
    ld      a, [delta_x_frac]
    ld      e, a
    ld      a, [peek_row_x_whole]
    ld      h, a
    ld      a, [peek_row_x_frac]
    ld      l, a

    add     hl, de  ; hl = peek x + delta x

    ld      a, h
    ld      [peek_row_x_whole], a
    ld      a, l
    ld      [peek_row_x_frac], a

    ;
    ; increment peek y by delta from rotation table
    ;
    ld      a, [delta_y_whole]
    ld      d, a
    ld      a, [delta_y_frac]
    ld      e, a
    ld      a, [peek_row_y_whole]
    ld      h, a
    ld      a, [peek_row_y_frac]
    ld      l, a

    add     hl, de  ; hl = peek y + delta x

    ld      a, h
    ld      [peek_row_y_whole], a
    ld      a, l
    ld      [peek_row_y_frac], a

    ld      a, [screen_x]
    inc     a
    ld      [screen_x], a

    pop     de
    pop     bc

    ; write tile result to command buffer
    ld      a, [current_tile]
    ld      [de], a
    inc     de  ; move to next byte in command buffer
.skip_inner
    dec     b
    jr      nz, .loop_inner

    ; end of sub-tile row
    ld      a, [viewer_x]   ; TODO fix
    ld      [peek_x], a

    ld      a, [peek_y]
    inc     a
    ld      [peek_y], a

    ld      a, [screen_y]
    inc     a
    ld      [screen_y], a

    ;
    ; move down a row
    ;
    ; peek = (peek[0] + -line_dy, peek[1] + line_dx)

    push de
    ; handle x
    ; peek_x = peek_x + -delta_y
    SUB_N16_N16_N16 \
        peek_x_whole, \
        peek_x_frac, \
        peek_x_whole, \
        peek_x_frac, \
        delta_y_whole, \
        delta_y_frac

    ; handle y
    ; peek_y = peek_y + delta_x
    ADD_N16_N16_N16 \
        peek_y_whole, \
        peek_y_frac, \
        peek_y_whole, \
        peek_y_frac, \
        delta_x_whole, \
        delta_x_frac

    pop     de

.skip_outer
    dec     c
    jp      nz, .loop_outer
    ret

; Apply render buffer to command list by mapping to 2x2 subtiles
;
render_to_command_list:
    ld      hl, render_buffer
    ld      de, command_list

    ld      c, SCRN_Y_B
    inc     c
    jp      .skip_outer

.loop_outer
    ld      b, SCRN_X_B
    inc     b
    jr      .skip_inner

.loop_inner
    push    bc
    push    de

    ;
    ; top-left sub-tile
    ;
    ld      a, [hl]

    ; set bit %00000001 if a = 1
    ld      [current_tile], a
    ld      b, a

    ;
    ; bottom-left sub-tile
    ;
    push    hl
    ld      de, SUB_TILE_X
    add     hl, de
    ld      a, [hl+]
    ; set bit %00000100 if a = 1
    sla     a
    sla     a
    or      a, b
    ld      b, a

    ;
    ; bottom-right sub-tile
    ;
    ld      a, [hl]
    ; set bit %00001000 if a = 1
    sla     a
    sla     a
    sla     a
    or      a, b
    ld      b, a

    ;
    ; top-right sub-tile
    ;
    pop     hl
    inc     hl

    ld      a, [hl+]
    ; set bit %00000010 if a = 1
    sla     a
    or      a, b

    ld      [current_tile], a

    pop     de
    pop     bc

    ; write tile result to command buffer
    ld      a, [current_tile]
    ld      [de], a
    inc     de  ; move to next byte in command buffer
.skip_inner
    dec     b
    jr      nz, .loop_inner

    push    de
    ld      de, SUB_TILE_X
    add     hl, de
    pop     de

.skip_outer
    dec     c
    jp      nz, .loop_outer
    ret
ret

ENDC
