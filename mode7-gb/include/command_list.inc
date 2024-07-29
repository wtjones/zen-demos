

IF      !DEF(COMMAND_LIST_ASM)
DEF COMMAND_LIST_ASM = 1

; Inline add of offset while retaining hl
; de += bc
MACRO ADD_ROW_OFFSET
    push    hl
    ld      h, d
    ld      l, e
    add     hl, bc
    ld      d, h
    ld      e, l
    pop     hl
    ENDM
ENDC    ;COMMAND_LIST_ASM

