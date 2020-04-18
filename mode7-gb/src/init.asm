INCLUDE	"gbhw.inc"


SECTION "init", ROM0

init::
    ld      hl, _OAMRAM
    ld      bc, 40 * 4
    ld      a, $00
    call    mem_SetVRAM

    ld      hl, _VRAM
    ld      bc, 384 * 16
    ld      a, $00
    call    mem_SetVRAM

    ld      hl, _SCRN0
    ld      bc, _SCRN1 - _SCRN0
    ld      a, $00
    call    mem_SetVRAM

    call    init_random
    call    init_palette
    call    init_checkered_tiles

    call    init_command_list
    xor     a
    ld      [frame_count], a
    ld      [joypad_state], a
ret
