INCLUDE "gbhw.inc"
INCLUDE "debug.inc"
INCLUDE "timing.inc"

SECTION	"start", ROM0[$0150]

start::
    nop
    ; init the stack pointer
    di
    ld      sp, $FFF4

    ; enable only vblank interrupts
    ld      a, IEF_VBLANK
    ldh     [rIE], a	; load it to the hardware register

    ; standard inits
    sub     a	;	a = 0
    ldh     [rSTAT], a	; init status

    ldh     [rSCY], a
    ldh     [rSCX], a

    ldh     [rLCDC], a	; init LCD to everything off
    ei
    call    init

    ; enable LCD, sprites, bg
    ld      a, LCDCF_ON | LCDCF_BG8000 | LCDCF_OBJON | LCDCF_BGON
    ldh     [rLCDC], a


.main:
    DELAY   10
    DBGMSG "ran init!"
.main_loop:
    DBGMSG "calling render!"
    call    render
    DBGMSG "render done"
    call    wait_vblank
    call    apply_command_list
    call    update_viewer
.derp
    DELAY   10
    jr      .derp
    jr      .main_loop

draw:
stat:
timer:
serial:
joypad:
    reti

initial_mode:
    DB mode
