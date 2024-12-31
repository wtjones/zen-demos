# zen-demos

A monorepo of demos, research, and experiments.

## Rendering

* [Py Demos](py_demos/)

    - Sine/cosine math
    - Texture rotation
    - Mode 7
    - Fixed math

* [RasGL](rasgl/)

    - Sofware rasterizer
    - Targets multiple retro platforms

* [Sand](sand/)

    - Sand simulation in C

## Game Boy mode 7 demo

```
cd mode7-gb
make -B
```

Load the rom in `/build` or use `run.sh`

### Renderers

#### Renderer A

Default renderer. Uses a buffer to process the screen-space rotation.

#### Renderer B

Performs rotation and 'sub-tile' translation in the same pass. Runs slightly slower.

`make renderer_b -B`

## Language experiments

* [Loitar](loitar/) - Lisp variant
* [Larse](larse/) - Lisp-like config library

## Samples

* [xform](samples/xform/) Simple matrix transforms in C
* [cmake_mono_export](samples/cmake_mono_export/) CMake monorepo dependency example

## Games

* [Rexile](rexile/) - Solitaire variant with a TUI

## Tools

* [Common Lisp env](tools/cl/) SBCL container
