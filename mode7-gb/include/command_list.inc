

IF      !DEF(COMMAND_LIST_ASM)
COMMAND_LIST_ASM     SET  1

; Inline add of offset while retaining hl
; de += bc
ADD_ROW_OFFSET: MACRO
    push    hl
    ld      h, d
    ld      l, e
    add     hl, bc
    ld      d, h
    ld      e, l
    pop     hl
    ENDM
ENDC    ;COMMAND_LIST_ASM

