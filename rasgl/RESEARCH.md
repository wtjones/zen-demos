# RasGL Research

## Math

### CORDIC

https://github.com/francisrstokes/githublog/blob/main/2024/5/10/cordic.md

> CORDIC is an algorithm for computing trig functions like sin, cos, tan etc on low powered hardware, without an FPU (i.e. no floating point) or expensive lookup tables. In fact, it reduces these complex functions to simple additions and bit shifts.

## Platform

### Classic Mac

[Retro68](https://github.com/autc04/Retro68) cross-compiler

### PSX

To avoid WinXP: https://github.com/Lameguy64/PSn00bSDK

### PS2

The PS2 CPU is currently not a candidate for this project as it is [64-bit](https://en.wikipedia.org/wiki/R5000#Derivatives).

### Docker build

From [example](https://github.com/longjoel/ultimate-homebrew/blob/main/PS2.Dockerfile) that uses gcc via [ps2sdk](https://github.com/ps2dev/ps2sdk)

```
root@deef3201c7fa:/app# mips64r5900el-ps2-elf-gcc -v
Using built-in specs.
COLLECT_GCC=mips64r5900el-ps2-elf-gcc
COLLECT_LTO_WRAPPER=/usr/local/ps2dev/ee/libexec/gcc/mips64r5900el-ps2-elf/14.1.0/lto-wrapper
Target: mips64r5900el-ps2-elf
Configured with: ../configure --quiet --prefix=/usr/local/ps2dev/ee --target=mips64r5900el-ps2-elf --enable-languages=c,c++ --with-float=hard --with- 
sysroot=/usr/local/ps2dev/ee/mips64r5900el-ps2-elf --with-native-system-header-dir=/include --with-newlib --disable-libssp --disable-multilib --disable-nls --disable-tls --enable-cxx-flags=-G0 --enable-threads=posix --silent
Thread model: posix
Supported LTO compression algorithms: zlib
gcc version 14.1.0 (GCC)
```
