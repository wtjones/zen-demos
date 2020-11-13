ROTATION_RECORD_SIZE    EQU 8

SECTION "rotation lookup vars", WRAM0

rotation_lookup_hi: DS 1
rotation_lookup_low: DS 1

rotated_x_whole:: DS 1
rotated_x_frac:: DS 1
rotated_y_whole:: DS 1
rotated_y_frac:: DS 1

delta_x_whole:: DS 1
delta_x_frac:: DS 1
delta_y_whole:: DS 1
delta_y_frac:: DS 1

SECTION "rotation lookup code", ROM0

; Set a pointer to rotation for angle zero
init_rotation_lookup::
    ld      hl, rotation_data
    ld      a, h
    ld      [rotation_lookup_hi], a
    ld      a, l
    ld      [rotation_lookup_low], a
    ret

; Set rotation vars by current angle
; Inputs:
;
; Outputs:
;
; Destroys:
;   bc, hl, de
set_rotation::
    ; hl = viewer_angle * ROTATION_RECORD_SIZE
    xor     a
    ld      h, a
    ld      a, [viewer_angle]
    ld      l, a
    add     hl, hl
    add     hl, hl
    add     hl, hl
    
    ; add offset to start of lookup
    ld      de, rotation_data
    add     hl, de
    
    ; cpu is little-endian
    ld      a, [hl+]
    ld      [rotated_x_frac], a
    ld      a, [hl+]
    ld      [rotated_x_whole], a
    ld      a, [hl+]
    ld      [rotated_y_frac], a
    ld      a, [hl+]
    ld      [rotated_y_whole], a
    
    ld      a, [hl+]
    ld      [delta_x_frac], a
    ld      a, [hl+]
    ld      [delta_x_whole], a
    ld      a, [hl+]
    ld      [delta_y_frac], a
    ld      a, [hl+]
    ld      [delta_y_whole], a
     
    ret
