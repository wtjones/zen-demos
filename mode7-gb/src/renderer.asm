INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

SECTION "renderer vars", WRAM0

screen_x:: DS 1
screen_y:: DS 1
peek_x:: DS 1
peek_y:: DS 1
current_tile:: DS 1     ; the tile to be appended to command buffer
render_buffer: DS SCRN_X_B * 2 * SCRN_Y_B

SECTION "renderer code", ROM0

render::
    call render_to_buffer
    call render_to_command_list
    ret
    
render_to_buffer:
    xor     a
    ld      [screen_x], a
    ld      [screen_y], a
    
    ld      a, [viewer_x]       ; this is not correct, at least as
    ld      [peek_x], a       ; far as naming
    
    ld      a, [viewer_y]
    ld      [peek_y], a
    ld      de, render_buffer
    
    ld      c, SCRN_Y_B         ; Outer loop on sub-tiles (4x4 pixels).
                                ; Currently just render half of the screen.
    inc     c
    jp      .skip_outer

.loop_outer 
    ld      b, SCRN_X_B * 2     ;Inner loop on sub-tiles (4x4 pixels).
    inc     b
    jr      .skip_inner

.loop_inner 
    push    bc
    push    de

    ld      a, [peek_x]
    ld      d, a
    ld      a, [peek_y]
    ld      e, a

    call    get_map_tile    ; d = map tile
    ld      a, d
    ld      [current_tile], a

    ld      a, [peek_x]
    inc     a
    ld      [peek_x], a
    
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
    
.skip_outer
    dec     c
    jp      nz, .loop_outer
    ret

; Apply render buffer to command list by mapping to 2x2 subtiles
;
render_to_command_list:
    ld      hl, render_buffer
    ld      de, command_list
    
    ld      c, SCRN_Y_B / 2     ; Currently just render half of the screen.
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
    ld      de, SCRN_X_B * 2
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
    ld      de, SCRN_X_B * 2
    add     hl, de
    pop     de

.skip_outer
    dec     c
    jp      nz, .loop_outer
    ret
ret
