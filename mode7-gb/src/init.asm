INCLUDE	"gbhw.inc"


SECTION "init", ROM0

init::
    ld      hl, _OAMRAM
    ld      bc, 40 * 4
    ld      a, $00
    call    mem_Set

    call    init_random
    call    init_palette

    call    init_command_list
    xor     a
    ld      [frame_count], a
    ld      [joypad_state], a
ret
