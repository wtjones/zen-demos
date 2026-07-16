# PSX GTE

<https://psx.arthus.net/sdk/Psy-Q/DOCS/TRAINING/FALL96/gte.pdf>

## General data

### Input vector

VX0 VY0 VZ0
VX1 VY1 VZ1
VX2 VY2 VZ2

> Input vector-Vn(dreg0~5) R/W

What is the bit format?


## Control registers

### Rotaton

R11 R12 R13
R21 R22 R23
R31 R32 R33

> Rotation Matrix (creg0~4) R/W (1,3,12)

Is it this?:

`SWWWFFFFFFFFFFFF`

00  R11 R12
01  R13 R21
02  R22 R23
03  R31 R32
04  R33 XXX

### Translation vector

TRX
TRY
TRZ

> TranslationVector (creg5~7) R/W (1,31,0)

Fixed point?


## Access instructions

```
mem     <-swc2--    GTE
mem     --lwc2-->   GTE
                    GTE     --mfc2-->   CPU
                    GTE     <--mtc2--   CPU
                    GTE     --cfc2-->   CPU
                    GTE     <--ctc2--   CPU
```
