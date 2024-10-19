# RasGL - Rendering Library for Retro Systems

## Overview

RasGL is software rendering library with cross-platform support for 90's era targets.

The key attributes of RasGL:

* Fixed-point calculations
  * Many 32-bit platforms of the mid 90's lack strong floating-point support.
* Triangle-based polygon pipeline
  * A single primative allows for a narrower focus and aligns with pipelines of the era.

See [TODO.md](TODO.md) and [RESEARCH.md](RESEARCH.md).

## Features

* Working
  * Fixed-point math
    * Currently 16.16
  * Transformation pipeline based on 4x4 matrix math
  * View frustum creation from projection matrix
  * Backface culling
  * Wireframe polygon rendering
* Planned
  * Frustum culling
  * Polygon clipping
  * Flat shaded polygons
  * Texture-mapped polygons
  * Depth sorting

## Target platforms

The ideal candidate platform has these attributes:

* 32-bit CPU with integer mul/div
* framebuffer graphics
* 8-bit color support

Platform support progress:

* Working
  * POSIX - Linux/Mac OS
    * Default target for development
    * Uses SDL2
  * DOS protected mode
    * DJGPP
    * Allegro.cc for rending primatives and input
    * Tested with dosbox-x built-in extender
* Proposed
  * GBA
    * Framebuffer modes should be suitable
  * PSX
    * Has hardware 3D support. Could either support the hardware via abstraction or use framebuffer rendering.

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

## Render Pipeline

Transform object AABB points to view space
Construct view AABB from transformed points
Transform frustum to view space
Test AABB points against frustum
    - could be optimized
Test object view space AABB against view-space frustum
Transform object vertices to world vertices
Transform world vertices to view vertices
Transform view vertices to screen vertices

Push vertex to render array, get index.

For each triangle:
    Push the 3 vertex indices.

### Map render

Assume world size of 5x5.

```
p0-------p1-------p2
|  t0  / |  t2  / |
|    /   |    /   |
|  /  t1 |  /  t3 |
p6-------p7-------p8
```

## Coordinate system

A [right-handed system](https://learnopengl.com/Getting-started/Coordinate-Systems) is used.

```
            +Y
            |
            |     -Z
            |   /
            | /
-X ------------------- +X
           /|
         /  |
       /    |
    +Z      |
           -Y
```

## Build and Run

### Build dependencies

Run `build_deps.sh` to build the script system.

### Tests

`run_tests.sh`

### Posix/SDL

```
cmake -S . -DRAS_PLATFORM=ras_sdl -B build
cmake --build build
./build/ras_sdl
```

or `run_posix.sh [world | poly] [0 | 1] [scene path]`

example: `./run_posix.sh poly 0 assets/scenes/tri.lsp`

### DOS

#### Get DJGPP

Build or get DJGPP binaries from <https://github.com/andrewwutw/build-djgpp>.

Extract somewhere such as `/opt/djgpp`.

Set variable `DJGPP_PREFIX` in shell or profile:

```
export DJGPP_PREFIX="/opt/djgpp"
# if building with defaults:
export DJGPP_PREFIX="/usr/local/djgpp"

```

Optional: Set PATH or use provided shell script.

`PATH="${DJGPP_PREFIX}/bin:${PATH}"`

#### Get Allegro

Clone repo somewhere locally: <https://github.com/wtjones/allegro-4.2.2-xc>

```
cd allegro-4.2.2-xc
```

Review and run `xmake.sh` to generate files such as `lib/djgpp/liballeg.a`. On Linux, it may output a warning about `setup.exe`, but this is fine.

```text
i586-pc-msdosdjgpp-ar: creating lib/djgpp/liballeg.a
make: *** No rule to make target 'setup/setup.exe', needed by 'setup'.  Stop.
```

Set variable `ALLEGRO` in shell or profile:

```
export ALLEGRO="/home/myuser/dev/allegro-4.2.2-xc"
```

#### Build

```
cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -DRAS_PLATFORM=ras_dos -B build
cmake --build build -t demo
```

#### DOSBox

[DOSBox-X](https://github.com/joncampbell123/dosbox-x/) is recommended due to the built-in 32-bit extender.

```bash
export DOSBOX_BIN="/usr/local/bin/dosbox-x"
```

Flatpak example:

```bash
flatpak install flathub com.dosbox_x.DOSBox-X
export DOSBOX_BIN="flatpak run --filesystem=/tmp com.dosbox_x.DOSBox-X"
```

#### Run

```
./run_dos.sh [world | poly] [0 | 1] [scene path]`
```

example: `./run_dos.sh poly 0 assets/secenes/tri.lsp`

## Demos

### Common Controls


* Toggle projection mode: p
* Toggle backface culling mode: b
* Cycle polygon render modes: o
* Flush per-frame logs: f


### Demo: world

Demonstrates movement controls over world geometry.

#### Controls

* Move camera: WASD
* Rotate camera: Q/E
* Toggle view mode: TAB
* Adjust FOV: left/right bracket

### Demo: poly

```
./run_posix poly 0
```

Provide a model obj file as an optional param.

```
./run_posix poly 0 ./assets/models/cube.obj
./run_posix poly 0 ./assets/models/ico.obj
./run_posix poly 0 ./assets/models/tri.obj
```

#### Controls

* Move camera: WASD
* Rotate camera: Q/E
* Toggle view mode: TAB
* Adjust FOV: left/right bracket

* Move model left/right along X: key left/right
* Move model up/down along Y: key up/down
* Move model in/out along Z: key up/down + shift
* Tilt model forward/backward around X: up/down + ctrl + shift
* Rotate model left/right around Y: left/right + ctrl
* Tilt model left/right around Z: up/down + ctrl


## Debug output

Pass a value of 1 in the run script:

```
run_tests.sh 1
run_posix.sh [world | poly] 1
run_dos.sh [world | poly] 1
```

## VS Code

The [cmake variant file](https://vector-of-bool.github.io/docs/vscode-cmake-tools/variants.html) allows for the selection of a buildType/platform/demo combo. As more demos are added, it may be preferable to use build targets.

## External libraries

* [log.c](https://github.com/rxi/log.c)
* [fpsqrt](https://github.com/chmike/fpsqrt)

## References

* [How to write a (software) 3d polygon pipeline](https://www.cbloom.com/3d/techdocs/pipeline.txt)
* [OBJ format](https://www.cs.cmu.edu/~mbz/personal/graphics/obj.html)
* [OBJ sample](https://people.sc.fsu.edu/~jburkardt/data/obj/cube.obj)
