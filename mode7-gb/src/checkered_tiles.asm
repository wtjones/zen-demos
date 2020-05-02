INCLUDE	"gbhw.inc"
INCLUDE "memory.inc"

SECTION "checkered tiles vars", WRAM0

SECTION "checkered tiles code", ROM0

init_checkered_tiles::
    ld      de, _VRAM
    ld      hl, checkered_tiles_data
    ld      c, 16 * 8
    call mem_CopyMono
    ret
