# RasGL Research

## Math

### CORDIC

<https://github.com/francisrstokes/githublog/blob/main/2024/5/10/cordic.md>

> CORDIC is an algorithm for computing trig functions like sin, cos, tan etc on low powered hardware, without an FPU (i.e. no floating point) or expensive lookup tables. In fact, it reduces these complex functions to simple additions and bit shifts.

## Scene management

### Standards

The standard for 3d scenes is [gltf](https://github.com/KhronosGroup/glTF/blob/main/specification/2.0/figures/gltfOverview-2.0.0d.png).

### Scene primatives

Consider [kons-9](https://kaveh808.github.io/kons-9/#KONS-9:@POINT%20MGL-PAX:SECTION).

### Scene format

From Trials camera [example](https://github.com/Shirakumo/trial/blob/ee2b107d5f21f089e8a78fb7e4272b26e9ada99b/examples/scene-loader.lisp#L52):

```lisp
(setf (camera scene) camera)
(enter (make-instance 'editor-camera :name :camera :location (VEC3 10.0 20 14) :rotation (vec3 0.75 5.5 0.0) :fov 50 :move-speed 0.1) scene)
```

RasGL ideas

```lisp
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

*Update*: Vertex colors are not useful with palette-based rendering.

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

## Colors

### Color ramps

From [Pixelblog](https://www.slynyrd.com/blog/2018/1/10/pixelblog-1-color-palettes):

> A color ramp is a specific range of colors that work well together, arranged according to brightness. Here is an example of what I consider a good color ramp.
>
> I want 8 ramps total so I will shift the hues of each ramp by 45 degrees to complete the 360 degree cycle around the color wheel.

#### Color ramp creator

<http://mycours.es/crc/80700921>

## Platform

### Classic Mac

[Retro68](https://github.com/autc04/Retro68) cross-compiler

### PSX

To avoid WinXP: <https://github.com/Lameguy64/PSn00bSDK>

### PS2

The PS2 CPU is currently not a candidate for this project as it is [64-bit](https://en.wikipedia.org/wiki/R5000#Derivatives).

### Docker build

From [example](https://github.com/longjoel/ultimate-homebrew/blob/main/PS2.Dockerfile) that uses gcc via [ps2sdk](https://github.com/ps2dev/ps2sdk)

```bash
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

## Text

Ideal situation is to use the same 3d pipline by rendering characters as orthographic two poly sprites.

### Phase 1 - direct screen space

1. App calls core_draw_text()
    - provide font, top-left coords
    - coords are screen absolute
1. Determine top left screen coord of first char from given pos.
    - Ortho matrix is skipped here. Coord should match.
1. Generate 4 verts
1. Generate two triangles via vert indices.
1. Map material in some way.
    - Lacking texture mapping, add a special sprite mode that assumes two tris.

### Phase 2 - texture mapping

1. App calls core_draw_text()
    - provide font, top-left coords
    - coords are screen absolute
1. Build ortho matrix
1. Generate 4 verts, 2 tris per character.
1. Apply UVs via texture mapping.

## Console

### Project

```text
/src
  /core
    - console - ux for render, input
    - repl  - parses and processes commands
  /shell - optional cli target
    uses repl
```

### Commands

`exec <script>`: Run a larse script
`help`
`clear`
`scene load <filename.lsp>`: Load a scene
`scene show`: Dump out current scene
`scene restart`
`scene save <out.lsp>`: Save out state of scene
`material list`: Get material atlas

### Qs

How does the repl access the camera?
How does the repl reload the scene?

Create a world concept.
Register the scene with the world.
Move animation code to world.
Register camera with world.
Repl takes world reference.

### Render

#### option 1

We need pull n visible rows/lines from ring buffer.

Build array of ints that index the start of each line.

Determine index of first row of console via max index - rows - 1.

For each visible row, call function that builds a line from current index to next - 1.

- Discard trailing newline if exists. It may not if wrap is supported.  

#### option alt

From tail, search backward to find newline.
Write to last visible row.

#### Render scenarios

```text
// start_row = max_row - index_count
// start_row = start_row < 0 ? 0 : start_row
// start_row = 5 - 2 = 3
// start_line = index_count - (max_row - start_row)
// start_line = 2 - (5 - 3) = 0
// start_y = start_row *(8 + 2)
// start_y = 3 * (10) = 30
/*
  -4    y (8 plus 2 padding)
0 -3    0
1 -2    10
2 -1    20
3 0     30
4 1
5 P
*/

// start_row = max_row - index_count
// start_row = start_row < 0 ? 0 : start_row
// start_row = 5 - 7 = -2 = 0
// start_line = index_count - (max_row - start_row)
// start_line = 7 - (5 - 0) = 2

/*
  0
  1
0 2
1 3
2 4
3 5
4 6
5 P
*/
  ```

## Ring buffer

Imagine a buffer with 5 slots:

```text
[0] [1] [2] [3] [4]
 ↑    ↑
head  tail
```

head → where to read

tail → where to write

As data is added (tail++) or removed (head++), both wrap back to 0 when they reach the end.

Full when tail + 1 == head (modulo capacity)

- One slot is intentionally left empty to distinguish between full and empty. This is the 1-slot reservation strategy.

Empty when head == tail

## CMake peer libraries

<https://github.com/pr0g/cmake-examples/blob/main/examples/core/shared/application/configure.sh>

## Rendering optimizations

Frustum cull in object space (transform frustum planes to object space).

## Pipeline

### Sorting concerns

Stages work on the scene. This allows flexibility order of processing.

### Clipping

This could help remove the recursive clipping. Build out a list of faces to clip that is ammended during clipping.

### separate array option

```text
struct RasMesh

    RasPipelineVertex *pipeline_verts;
    uint32_t num_pipeline_verts;

    uint32_t *visible_indexes;
    uint32_t num_visible_indexes;

    // not used much yet
    RasPipelineFace visible_faces[MAX_PIPELINE_VERTS / 3];
    uint32_t num_visible_faces;

    int32_t *material_indexes;
    uint32_t num_material_indexes; // Will be num_visible_indexes / 3


struct RenderData
    // index of each object
    int object_mesh[MAXOBJECTS]
    // after aabb
    // there will be gaps in renderstate messages
    int visible_object_meshes[MAXOBJS]


struct RenderState

    RasMesh meshes[MAXOBJECTS *= 20%]
```

## Sorting

Spliting out to the mesh structure adds a challenge to sorting.

Decouple storage (VAO) from render order by introducing a render queue, often called a sort list or order table:

```text
struct RenderFace {
    float depth;
    int vao_id;         // Identifier for VAO or mesh
    int start_index;    // Index in element buffer (EBO)
    int count;          // Number of indices (3)
};
```

For each visible face, compute:

- Average view-space Z (depth)
- Store a RenderFace in a render list

Compute the view-space Z by transforming each vertex of the face with the view matrix and averaging the Z values.

Use a simple sort algorithm (quick sort, radix sort, etc.) on depth, in back-to-front order (larger Z rendered first for transparency).
