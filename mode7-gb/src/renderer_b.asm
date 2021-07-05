IF MODE == 1

INCLUDE	"gbhw.inc"
INCLUDE "math.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

SUB_TILE_TOP_LEFT_MASK      EQU %00000001
SUB_TILE_BOTTOM_LEFT_MASK   EQU %00000010
SUB_TILE_BOTTOM_RIGHT_MASK  EQU %00000100
SUB_TILE_TOP_RIGHT_MASK     EQU %00001000

; Sub-tiles are the 2x2 "pixels" of each BG tile
SUB_TILE_X      EQU SCRN_X_B * 2
SUB_TILE_Y      EQU SCRN_Y_B * 2

SECTION "renderer vars", WRAM0

screen_x:: DS 1
screen_y:: DS 1
peek_x:: DS 1
peek_y:: DS 1
current_tile:: DS 1     ; the tile to be appended to command buffer
current_command: DS 1

peek_row_x_whole: DS 1
peek_row_x_frac: DS 1

peek_row_y_whole: DS 1
peek_row_y_frac: DS 1

peek_x_whole: DS 1
peek_x_frac: DS 1

peek_y_whole: DS 1
peek_y_frac: DS 1

row_start_hi: DS 1
row_start_low: DS 1

SECTION "renderer code", ROM0


render::
    call set_rotation
    call render_to_command_list
    ret

render_to_command_list:
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

    ld      de, command_list
    ld      a, d
    ld      [row_start_hi], a
    ld      a, e
    ld      [row_start_low], a

    ld      c, SUB_TILE_Y / 2   ; Outer loop on sub-tiles (4x4 pixels).
                                ; Y / 2 because each iteration is unrolled to
                                ; to handle 2 rows.
    inc     c
    jp      .skip_outer

.loop_outer

    ;
    ; Start of even row loop
    ;

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
    jp      .skip_inner_even_row

.loop_inner_even_row
    push    bc
    push    de

    ld      a, [de]
    ld      [current_command], a

    ; use the whole part for the map lookup
    ld      a, [peek_row_x_whole]
    ld      d, a
    ld      a, [peek_row_y_whole]
    ld      e, a

    call    get_map_tile    ; d = map tile
    ld      a, d
    ld      [current_tile], a

    ;
    ; determine mask and sub-tile
    ;

    ; if column is even, sub-tile is top-left
    ld      a, [screen_x]
    and     %00000001
    jr     nz, .skip_top_left

    ; top left is the first sub-tile, so no need to mask to preserve
    ld      a, [current_tile]
    ld      [current_command], a
    jr      .end_even_row_command
.skip_top_left
    ; top right
    ld      a, [current_command]
    ld      b, a
    ld      a, [current_tile]
    ; set bit %00000010 if a = 1
    sla     a

    or      a, b
    ld      [current_command], a
.end_even_row_command

    ld      a, [screen_x]
    sub     SUB_TILE_X - 1
    jr      z, .skip_even_x_inc
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


.skip_even_x_inc
    ld      a, [screen_x]
    inc     a
    ld      [screen_x], a

    pop     de
    pop     bc

    ; write tile result to command list
    ld      a, [current_command]
    ld      [de], a

    ; increment de if column is odd

    ld      a, b
    and     %00000001
    jr     z, .skip_inc_de_even_row

    inc     de  ; move to next byte in command buffer
.skip_inc_de_even_row


.skip_inner_even_row
    dec     b
    jp      nz, .loop_inner_even_row

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

    ; At the end of an even row, revert de to the start of the current row
    ; in the command list so that bottom-left and bottom-right sub-tiles may
    ; my be written

    ld      a, [row_start_hi]
    ld      d, a
    ld      a, [row_start_low]
    ld      e, a

    ;
    ; end of even sub-tile row
    ;

    ;
    ; Start of odd row loop
    ;

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
    jp      .skip_inner_odd_row

.loop_inner_odd_row
    push    bc
    push    de

    ld      a, [de]
    ld      [current_command], a
    ; use the whole part for the map lookup
    ld      a, [peek_row_x_whole]
    ld      d, a
    ld      a, [peek_row_y_whole]
    ld      e, a

    call    get_map_tile    ; d = map tile
    ld      a, d
    ld      [current_tile], a

    ;
    ; determine mask and sub-tile
    ;

    ld      a, [current_command]
    ld      b, a
    ; if column is odd, sub-tile is bottom-left
    ld      a, [screen_x]
    and     %00000001
    jr      nz, .skip_bottom_left

    ; bottom left
    ld      a, [current_tile]
    ; set bit %00000100 if a = 1
    sla     a
    sla     a
    or      a, b
    ld      b, a
    ld      [current_command], a
    jr      .end_odd_row_command
.skip_bottom_left
    ; bottom right
    ld      a, [current_tile]

    ; set bit %00001000 if a = 1
    sla     a
    sla     a
    sla     a
    or      a, b
    ld      b, a
    ld      [current_command], a
.end_odd_row_command


    ld      a, [screen_x]
    sub     SUB_TILE_X - 1
    jr      z, .skip_odd_x_inc

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

.skip_odd_x_inc
    ld      a, [screen_x]
    inc     a
    ld      [screen_x], a

    pop     de
    pop     bc

    ; write tile result to command buffer
    ld      a, [current_command]
    ld      [de], a

    ; increment de if column is odd
    ld      a, b
    and     %00000001
    jr     z, .skip_inc_de_odd_row

    inc     de  ; move to next byte in command buffer
.skip_inc_de_odd_row


.skip_inner_odd_row
    dec     b
    jp      nz, .loop_inner_odd_row


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

    ; At the end of an odd row, move to the next byte in command buffer
    ; and preserve the start of the row
    ld      a, d
    ld      [row_start_hi], a
    ld      a, e
    ld      [row_start_low], a

    ;
    ; end of odd sub-tile row
    ;
.skip_outer
    dec     c
    jp      nz, .loop_outer
    ret

ENDC
