INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "debug.inc"

COMMANDS_PER_FRAME_MAX  EQU SCRN_X_B * 3
COMMAND_LIST_MAX        EQU (SCRN_X_B * SCRN_Y_B) / 2
COMMAND_LIST_SIZE       EQU 1
DEST_VRAM_START         EQU $9920 ; middle of screen
TILE_OFFSET_TO_NEXT_ROW EQU $C

SECTION "command list vars", WRAM0

command_list:: DS COMMAND_LIST_MAX * COMMAND_LIST_SIZE
applied_this_frame: DS 1

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
    xor     a
    ld      [applied_this_frame], a

    ld      c, SCRN_Y_B / 2     ; half of the screen
    inc     c
    jr      .skip_outer

.loop_outer
    ld      b, SCRN_X_B
    inc     b
    jr      .skip_inner

.loop_inner
    ld      a, [hl+]
    ld      [de], a
    inc     de

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

    ld      a, [applied_this_frame]
    add     SCRN_X_B
    cp      COMMANDS_PER_FRAME_MAX
    jr      nz, .skip_list_limit        ; if applied_this_frame = limit
    push    hl
    call    wait_vblank
    pop     hl
    xor     a
.skip_list_limit                        ; end if
    ld      [applied_this_frame], a

.skip_outer
    dec     c
    jr      nz, .loop_outer
    ret
