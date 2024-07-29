INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"
INCLUDE "command_list.inc"
INCLUDE "debug.inc"

DEF COMMANDS_PER_FRAME_MAX  EQU SCRN_X_B * 3
DEF COMMAND_LIST_MAX        EQU SCRN_X_B * SCRN_Y_B
DEF COMMAND_LIST_SIZE       EQU 1
DEF DEST_VRAM_START         EQU _SCRN0
DEF TILE_OFFSET_TO_NEXT_ROW EQU $C

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

; Writes tiles in the list over three frames.
; First frame = 6 rows
; Second frame = 6 rows
; Third frame = 6 rows
apply_command_list::
    ld      hl, command_list
    ld      de, DEST_VRAM_START
    ld      b, HIGH(TILE_OFFSET_TO_NEXT_ROW)
    ld      c, LOW(TILE_OFFSET_TO_NEXT_ROW)

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

   ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    ; Wait for vblank
    push    hl
    call    wait_vblank
    pop     hl

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

     ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

     ADD_ROW_OFFSET

     REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    ; Wait for vblank
    push    hl
    call    wait_vblank
    pop     hl

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

     ADD_ROW_OFFSET

     REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

     ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ADD_ROW_OFFSET

    REPT 20
        ld      a, [hl+]
        ld      [de], a
        inc     de
    ENDR

    ret
