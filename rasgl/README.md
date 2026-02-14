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
  * Bitmap font rendering
  * Frustum culling
  * Polygon clipping
  * Flat shaded polygons
  * Gridmap with back-to-front rendering
  * Advanced gridmap "tombmap" with variable hights
* Planned
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

```text
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
    /hosted
      impl.c           // implementations for hosted platforms (non-embedded)

```

## Render Pipeline

1. **Object bounds culling**

   * Transform object AABB corners into view space.
   * Build a view-space AABB from the transformed corners.
   * Transform the frustum into view space.
   * Test the object’s AABB against the frustum (can be optimized).
     * Far plane is ignored based on `RasClipSideMode`.
   * If the AABB is completely outside, reject the object early.

2. **Vertex transformation**

   * Transform object vertices → world space.
   * Transform world vertices → view space.
   * Transform view vertices → clip space (projection matrix).
   * Cull faces outside of at least one frustum plane.
     * Far plane is ignored based on `RasClipSideMode`.
   * Clip vertices/triangles against the view frustum in clip space.
   * Perform perspective divide → NDC.
     * Near plane clipped vertices are omitted.
   * Map NDC → screen coordinates.
   * Perform backface removal.
   * Transform face normals from model space to view space.
   * Calculate lighting from view space normals.

3. **Geometry setup**

   * Push each final screen-space vertex into the render array and record its index.
   * For each triangle, push the 3 vertex indices.

## Render layers

### Scene pass

* Gridmaps
* Objects

### UI pass

* Text rendering
* Orthographic mode

## Coordinate system

A [right-handed system](https://learnopengl.com/Getting-started/Coordinate-Systems) is used.

* Right-handed camera, looking down −Z
* +Y is up
* Clip space: −w ≤ x ≤ w, −w ≤ y ≤ w, 0 ≤ z ≤ w
* Perspective divide → NDC: x,y ∈ [−1,1], z ∈ [0,1]
* Triangle winding order is counter-clockwise when viewed from the front.

```text
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

```bash
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

```bash
export DJGPP_PREFIX="/opt/djgpp"
# if building with defaults:
export DJGPP_PREFIX="/usr/local/djgpp"

```

Optional: Set PATH or use provided shell script.

`PATH="${DJGPP_PREFIX}/bin:${PATH}"`

#### Get Allegro

Clone repo somewhere locally: <https://github.com/wtjones/allegro-4.2.2-xc>

```bash
cd allegro-4.2.2-xc
```

Review and run `xmake.sh` to generate files such as `lib/djgpp/liballeg.a`. On Linux, it may output a warning about `setup.exe`, but this is fine.

```text
i586-pc-msdosdjgpp-ar: creating lib/djgpp/liballeg.a
make: *** No rule to make target 'setup/setup.exe', needed by 'setup'.  Stop.
```

Set variable `ALLEGRO` in shell or profile:

```bash
export ALLEGRO="/home/myuser/dev/allegro-4.2.2-xc"
```

#### Build deps

The _larse_ project must be build with djgpp.

```bash
./build_deps_dos.sh
```

If an error like the following is encountered, try deleting the _larse_ `bld_dos` folder.

> gcc-12: fatal error: cannot execute ‘cc1’: execvp: No such file or directory

#### Build

```bash
cmake -S . -DCMAKE_TOOLCHAIN_FILE=tools/djgpp.cmake -DRAS_PLATFORM=ras_dos -B bld_dos
cmake --build bld_dos -t demo
```

#### DOSBox

[DOSBox-X](https://github.com/joncampbell123/dosbox-x/) is recommended due to the built-in 32-bit extender.

**Flatpak**

`run_dos.sh` will attempt to use flatpak with $pwd permissions. I'd prefer to wire this up in the shell profile, but had issues with the sandbox.

**Non-flatpak**

```bash
export DOSBOX_BIN="/usr/local/bin/dosbox-x"
```

#### Run

```bash
./run_dos.sh [world | poly] [0 | 1] [scene path]`
```

example: `./run_dos.sh poly 0 assets/secenes/tri.lsp`

### PSX

Implementation is currently limited.

#### Build

```bash
./build_psx.sh
```
Optionally set max frames for testing:

```bash
RAS_MAX_FRAMES=1000 ./build_psx.sh
```
#### Run

Refer to `run_psx.sh`.

## Demos

### Common Controls

| Action                        | Key          |
| ----------------------------- | ------------ |
| Help                          | F1           |
| Toggle UI layer               | F3           |
| Toggle projection mode        | ctrl-p       |
| Toggle backface culling mode  | ctrl-b       |
| Cycle Z scale LUT modes       | ctrl-z       |
| Cycle polygon render modes    | ctrl-o       |
| Cycle polygon outline modes   | ctrl-shift-o |
| Cycle polygon clipping modes  | ctrl-c       |
| Cycle polygon clip sides[^1]  | ctrl-shift-c |
| Cycle normal diagnostic modes | ctrl-n       |
| Cycle grid output modes       | ctrl-g       |
| Cycle pipeline modes          | ctrl-l       |
| Flush per-frame logs          | ctrl-f       |

[^1]: Far plane off (default), far plane on, near plane only


### Console Controls

| Action         | Key      |
| -------------- | -------- |
| Toggle console | `` ` ``  |
| Recall back    | KEY_UP   |
| Recall forward | KEY_DOWN |

#### Camera Controls

| Action              | Key                |
| ------------------- | ------------------ |
| Move camera         | WASD               |
| Rotate camera       | Q/E                |
| Move up/down        | Z/C                |
| Adjust FOV          | left/right bracket |
| Decrease far plane  | ctrl-`[`           |
| Increase far plane  | ctrl-`]`           |
| Decrease near plane | ctrl-shift-`[`     |
| Increase near plane | ctrl-shift-`]`     |

### Demo: world

Demonstrates movement controls over world geometry.

This demo is deprecated in favor of loading scene files in the poly demo.

#### Map geometry

Assume world size of 5x5.

```text
p0-------p1-------p2
|  t0  / |  t2  / |
|    /   |    /   |
|  /  t1 |  /  t3 |
p6-------p7-------p8
```

#### Controls

| Action           | Key |
| ---------------- | --- |
| Toggle view mode | TAB |

### Demo: poly

```bash
./run_posix poly 0
```

Provide a scene file as an optional param.

```bash
./run_posix poly 0 ./assets/scenes/tri.lsp
./run_posix poly 0 ./assets/scenes/cube.lsp
./run_posix poly 0 ./assets/scenes/ico.lsp
./run_posix poly 0 ./assets/scenes/tmap01.lsp
```

#### Controls

| Action                        | Key                    |
| ----------------------------- | ---------------------- |
| Toggle selected model         | j                      |
| Toggle model animation        | k                      |
| Move model left/right along X | key left/right         |
| Move model up/down along Y    | key up/down            |
| Move model in/out along Z     | key up/down + shift    |
| Tilt model around X           | up/down + ctrl + shift |
| Rotate model around Y         | left/right + ctrl      |
| Tilt model around Z           | up/down + ctrl         |

## Debug output

Pass a value of 1 in the run script:

```bash
run_tests.sh 1
run_posix.sh [world | poly] 1
run_dos.sh [world | poly] 1
```

## VS Code

The [cmake variant file](https://vector-of-bool.github.io/docs/vscode-cmake-tools/variants.html) allows for the selection of a buildType/platform/demo combo. As more demos are added, it may be preferable to use build targets.

## External libraries

* [font8x8](https://github.com/dhepper/font8x8)
* [fpsqrt](https://github.com/chmike/fpsqrt)
* [log.c](https://github.com/rxi/log.c)
* [ps1-bare-metal](https://github.com/spicyjpeg/ps1-bare-metal)
* [mpack](https://github.com/ludocode/mpack)

## References

* [How to write a (software) 3d polygon pipeline](https://www.cbloom.com/3d/techdocs/pipeline.txt)
* [OBJ format](https://www.cs.cmu.edu/~mbz/personal/graphics/obj.html)
* [OBJ sample](https://people.sc.fsu.edu/~jburkardt/data/obj/cube.obj)
