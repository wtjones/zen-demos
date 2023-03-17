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

## Posix/SDL

```
cmake -S . -B build
cmake --build build
./build/ras_sdl
```

## DOS

### Get DJGPP

Obtain DJGPP in some manner such as https://github.com/andrewwutw/build-djgpp.

Extract somewhere such as `/opt/djgpp`.

Set variable `DJGPP_PREFIX` in shell or profile:
```
export DJGPP_PREFIX="/opt/djgpp"
```

Set PATH or use provided shell script.

`PATH="${DJGPP_PREFIX}/bin:${PATH}"`

### Build

```
cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -B build
cmake --build build -t ras_dos
```

### Run

Set variable `$DOSBOX_BIN`

```
export DJGPP_PREFIX="/usr/local/bin/dosbox-x"
```

```
./run_dos.sh
```
