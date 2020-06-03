; A monochrome tile set of 16 tiles of 4 quadrants each.
; A VRAM tile is 8 rows of 2 bytes = 16 bytes, so this output should be
; doubled upon load.

SECTION "checkered tiles data", ROM0
checkered_tiles_data::
; tile 0
    DB $00
    DB $00
    DB $00
    DB $00
    DB $00
    DB $00
    DB $00
    DB $00


; tile 1
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $00
    DB $00
    DB $00
    DB $00

; tile 2
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $00
    DB $00
    DB $00
    DB $00

; tile 3
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $00
    DB $00
    DB $00
    DB $00

; tile 4
    DB $00
    DB $00
    DB $00
    DB $00
    DB $f0
    DB $f0
    DB $f0
    DB $f0

; tile 5
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $f0

; tile 6
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $f0
    DB $f0
    DB $f0
    DB $f0

; tile 7
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $f0
    DB $f0
    DB $f0
    DB $f0

; tile 8
    DB $00
    DB $00
    DB $00
    DB $00
    DB $0f
    DB $0f
    DB $0f
    DB $0f

; tile 9
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $0f
    DB $0f
    DB $0f
    DB $0f

; tile 10
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $0f

; tile 11
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $0f
    DB $0f
    DB $0f
    DB $0f

; tile 12
    DB $00
    DB $00
    DB $00
    DB $00
    DB $ff
    DB $ff
    DB $ff
    DB $ff

; tile 13
    DB $f0
    DB $f0
    DB $f0
    DB $f0
    DB $ff
    DB $ff
    DB $ff
    DB $ff

; tile 14
    DB $0f
    DB $0f
    DB $0f
    DB $0f
    DB $ff
    DB $ff
    DB $ff
    DB $ff

; tile 15
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $ff
    DB $ff
