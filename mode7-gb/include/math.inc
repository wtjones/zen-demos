IF      !DEF(MATH_ASM)
DEF MATH_ASM = 1


; Treat each pair of params as 16-bit hi/low and perform:
;   \1_\2 = \3_\4 + de
; Inputs
;   \1 high byte of first variable
;   \2 low byte of first variable
;   \3 high byte of second variable
;   \4 low byte of second variable
; Destroys
;   DE, HL
MACRO ADD_N16_N16_DE
    ld      a, [\3]
    ld      h, a
    ld      a, [\4]
    ld      l, a

    add     hl, de

    ; store result
    ld      a, h
    ld      [\1], a
    ld      a, l
    ld      [\2], a

    ENDM

; Treat each pair of params as 16-bit hi/low and perform:
;   \1_\2 = \3_\4 + \5_\6
; Inputs
;   \1 high byte of first variable
;   \2 low byte of first variable
;   \3 high byte of second variable
;   \4 low byte of second variable
;   \5 high byte of third variable
;   \6 low byte of third variable
; Destroys
;   DE, HL
MACRO ADD_N16_N16_N16
    ld      a, [\3]
    ld      h, a
    ld      a, [\4]
    ld      l, a

    ld      a, [\5]
    ld      d, a
    ld      a, [\6]
    ld      e, a

    add     hl, de  ; hl = [\3\4] + [\5\6]

    ; store result
    ld      a, h
    ld      [\1], a
    ld      a, l
    ld      [\2], a

    ENDM

; Treat each pair of params as 16-bit hi/low and perform:
;   \1_\2 = \3_\4 - \5_\6
; Inputs
;   \1 high byte of first variable
;   \2 low byte of first variable
;   \3 high byte of second variable
;   \4 low byte of second variable
;   \5 high byte of third variable
;   \6 low byte of third variable
; Destroys
;   DE, HL
MACRO SUB_N16_N16_N16
    ld      a, [\5]
    cpl
    ld      h, a
    ld      a, [\6]
    cpl
    ld      l, a
    inc     hl    ; hl = -[\5\6]

    ld      a, [\3]
    ld      d, a
    ld      a, [\4]
    ld      e, a

    add     hl, de  ; hl = [\3\4] - [\5\6]

    ; store result
    ld      a, h
    ld      [\1], a
    ld      a, l
    ld      [\2], a

    ENDM
ENDC
