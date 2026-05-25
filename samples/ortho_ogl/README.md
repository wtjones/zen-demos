# ortho_ogl

Simple C + CMake OpenGL (legacy) demo that uses an orthographic 320x240 logical screen and pixel-doubles onto a 640x480 window.

Dependencies (on Debian/Ubuntu):

- cmake
- build-essential
- libglfw3-dev
- libgl1-mesa-dev

Build (cmake):

```bash
mkdir -p build
cd build
cmake ..
cmake --build .
```

Run (binary):

```bash
./ortho_ogl
```

What it draws

- Several filled 8×8 and 16×16 quads in a logical 320×240 coordinate space.
- The default window is 640×480 so each logical pixel maps to a 2×2 block on the screen (pixel-doubling).

Scripts

There are two convenience scripts at the project root:

- `build.sh`: Configures and builds the project. Usage:

```bash
# Release (default)
./build.sh 0
# Debug
./build.sh 1
```

- `run.sh`: Always rebuilds (uses `build.sh`) and runs the demo binary `ortho_ogl`.

```bash
# Rebuild (Release) and run
./run.sh
# Rebuild (Debug) and run
./run.sh 1
```

Coordinate origin

- The demo uses a conventional OpenGL bottom-left origin (+Y up). The source converts the original top-left-style coordinates so the quads appear at the same places as intended.


