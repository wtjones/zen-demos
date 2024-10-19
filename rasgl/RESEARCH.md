# RasGL Research

## Math

### CORDIC

https://github.com/francisrstokes/githublog/blob/main/2024/5/10/cordic.md

> CORDIC is an algorithm for computing trig functions like sin, cos, tan etc on low powered hardware, without an FPU (i.e. no floating point) or expensive lookup tables. In fact, it reduces these complex functions to simple additions and bit shifts.

## Scene management

### Standards

The standard for 3d scenes is [gltf](https://github.com/KhronosGroup/glTF/blob/main/specification/2.0/figures/gltfOverview-2.0.0d.png).

### Scene primatives

Consider [kons-9](https://kaveh808.github.io/kons-9/#KONS-9:@POINT%20MGL-PAX:SECTION).

### Scene format

From Trials camera [example](https://github.com/Shirakumo/trial/blob/ee2b107d5f21f089e8a78fb7e4272b26e9ada99b/examples/scene-loader.lisp#L52):

```
(setf (camera scene) camera)
(enter (make-instance 'editor-camera :name :camera :location (VEC3 10.0 20 14) :rotation (vec3 0.75 5.5 0.0) :fov 50 :move-speed 0.1) scene)

```

RasGL ideas

```
(scene "MyScene"
  (objects
    (object "object1" "/path/to/object1.obj" :position (vector 1.0 2.0 3.0) :orientation (vector 0.0 0.0 0.0))
    (object "object2" "/path/to/object2.obj" :position (vector 4.0 5.0 6.0) :orientation (vector 0.0 0.0 0.0))
    (object "object3" "/path/to/object3.obj" :position (vector 7.0 8.0 9.0) :orientation (vector 0.0 0.0 0.0))
  )
  (camera :position (vector 10.0 11.0 12.0) :orientation (vector 0.0 0.0 0.0))
)
```

## Modeling

### Vertex colors

> Assigning Colors to Vertices Using Vertex Paint
>
> Vertex colors allow you to paint colors directly onto the vertices of your model, and Blender interpolates the color across the faces. This method is common in game development and low-poly art where simple vertex coloring is used.
>
> Technically, the .obj format does not natively support vertex colors according to its official specification, but Blender extends the format to include this data.
>
> So, in this case, Blender is exporting vertex positions with vertex colors.
> Example Breakdown:

```plaintext
v 1.152022 0.647829 -0.910811 1.0000 0.2000 0.4392
```

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
