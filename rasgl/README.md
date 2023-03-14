## Proposed Structure

```
/src
    /demo
        demo_main.c     // uses gfx
    /platform
        /posix
            /main.c     // calls demo_main()
            /ras_plat.c
        /dos
            /main.c
            /ras_plat.c
    /raslib
        ras.h
        ras_plat.h
        ras.c           // uses ras_plat.c

```


## Build and Run

```
cmake -S . -B build
cmake --build build
./build/ras_sdl
```
