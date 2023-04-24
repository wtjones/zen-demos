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
    /core
        ras.h
        ras_plat.h
        ras.c           // uses ras_plat.c

```


## Build and Run

## Posix/SDL

```
cmake -S . -DRAS_PLATFORM=ras_sdl -B build
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

### Get Allegro

Clone repo somewhere locally: https://github.com/wtjones/allegro-4.2.2-xc

```
cd allegro-4.2.2-xc
```
Review and run `xmake.sh` to generate files such as `lib/djgpp/liballeg.a`

Set variable `ALLEGRO` in shell or profile:
```
export ALLEGRO="/home/myuser/dev/allegro-4.2.2-xc"
```

### Build

```
cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -DRAS_PLATFORM=ras_dos -B build
cmake --build build -t demo
```

### Run

Set variable `$DOSBOX_BIN`

```
export DJGPP_PREFIX="/usr/local/bin/dosbox-x"
```

```
./run_dos.sh
```
