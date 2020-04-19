INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

COMMANDS_PER_FRAME_MAX  EQU 12
COMMAND_LIST_MAX        EQU (SCRN_X_B * SCRN_Y_B) / 2
COMMAND_LIST_SIZE       EQU 1
DEST_VRAM_START         EQU $9920 ; middle of screen
TILE_OFFSET_TO_NEXT_ROW EQU $C;12 * 16

SECTION "command list vars", WRAM0

command_list:: DS COMMAND_LIST_MAX * COMMAND_LIST_SIZE
push_value:: DS 1
next_item_high: DS 1
next_item_low: DS 1

SECTION "command list utility", ROM0

init_command_list::
    xor     a
    ld      hl, command_list
    ld      bc, COMMAND_LIST_MAX * COMMAND_LIST_SIZE
    ld      a, $0
    call    mem_Set
    ret

apply_command_list::
    ld      hl, command_list
    ld      de, DEST_VRAM_START

    ld      c, 3;SCRN_Y_B
    inc     c
    jr      .skip_outer

.loop_outer
    ld      b, SCRN_X_B
    inc     b
    jr      .skip_inner

.loop_inner
    push    bc
    ; ld      c, COMMANDS_PER_FRAME_MAX
    ; inc     c
    ; jr      .skip_frame
;
; .loop_frame
    ld      a, [hl+]
    ld      [de], a
    inc     de


    ; pop     hl

; .skip_frame
;     dec     c
;     jr      nz, .loop_frame

;     call    wait_vblank
    pop     bc
.skip_inner
    dec     b
    jr      nz, .loop_inner

    ; Tile vram is 32*32 vs the visible 20 * 18. Advance to the next visible
    ; tile.
    push    hl
    ld      h, HIGH(TILE_OFFSET_TO_NEXT_ROW)
    ld      l, LOW(TILE_OFFSET_TO_NEXT_ROW)
    add     hl, de
    push    hl
    pop     de  ; de = start of next row
    pop     hl
.skip_outer
    dec     c
    jr      nz, .loop_outer
    ret