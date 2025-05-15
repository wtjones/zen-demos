# Sharp 80 Compiler

## Program structure

core_init:

- zero out memory
- load fonts

init:

- set tiles

core_update:

- read keys

update:

- set OAM buffer

core_draw:

- start DMA

draw:

- update tiles

## Equates

Input

```lisp
(defequ 'COMMAND_LIST_SIZE 1)
(defequ 'COMMANDS_PER_FRAME_MAX (* SCRN_X_B 3))

```

Output

```asm
DEF COMMAND_LIST_SIZE       EQU 1
DEF COMMANDS_PER_FRAME_MAX  EQU SCRN_X_B * 3 ; only expanded, as rgbds will compute at compile time

```

## Variables

Input

```[lisp]
(defvar my_byte)

(setf my_byte 0)
(setf my_byte CONST1)
```

Output

```asm
my_byte: DS 1

ld      a, 0
ld      [my_byte], a

ld      a, CONST1
ld      [my_byte], a
```

## Subroutine

Input

```lisp
(defsub (:global init)
    setf my_byte 0))
```

Output

```asm
init::
    ld      a, 0
    ld      [my_byte], a
    ret
```

## Branching

Input

```lisp
(when (<= MAP_WIDTH pos_x)
    (setf my_result EMPTY_TILE)  
    (return))
```

Output

```asm
    ld  a, [pos_x]
    cp  a, MAP_WIDTH
    jr  c, .skip_oob1   ; if MAP_WIDTH <= x
    ld  a, EMPTY_TILE
    ld  [my_result], a  
    ret
.skip_oob1                  
```

Input

```lisp
(when (> MAP_WIDTH pos_x)
    (setf my_result EMPTY_TILE)
    (return))
```

Output

```asm
    ld      a,77
    sub     (hl)
    jp      nc,i_4 ; if MAP_WIDTH > x
    ld  a, EMPTY_TILE
    ld  [my_result], a
    ret
.i_4
```

Input

```lisp
(when (= cell_buffer_x 0)
    (inc_neighbor_count_buffer))
```

Output

```asm
    ld      a, [cell_buffer_x]
    cp      0
    jr      nz, .skip_count_row         ; if a == 0
    call    inc_neighbor_count_buffer
.skip_count_row                         ; end if
```

## Reference

<https://gitlab.com/nebogeo/co2/-/blob/master/co2.scm?ref_type=heads>

<https://inconvergent.net/2023/lets-write-a-dsl/>

<https://godbolt.org/z/WEhrrWrEW>
