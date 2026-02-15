# PSX Research

To avoid WinXP: <https://github.com/Lameguy64/PSn00bSDK>

## Library Refactoring Option

**Goal**: Split `rasgl/core` to support PSX platform which:

- Lacks FILE I/O.
- Limited libc for parsing and string manip.

**Proposed Architecture**:

1. **`rasgl_core`** - Pure 3D graphics pipeline (platform-agnostic, PSX-compatible)
   - Math, matrices, camera, transformations
   - Rendering pipeline (rasterization, clipping, frustum, culling)
   - Graphics primitives and buffers
   - Scene/model data structures and in-memory manipulation
   - Utilities (debug buffering, events, text, string ops)
   - **No dependencies on**: FILE I/O, parsing libraries, POSIX-specific APIs

2. **`rasgl_loaders`** - Asset loading (POSIX/DOS only)
   - File-based model loading (`.obj` parsing, `core_load_model()`)
   - File-based scene loading (Larse script parsing, gridmap/tombmap loaders)
   - Dependencies: `rasgl_core` + FILE I/O + `larse` parser
   - **Platforms**: POSIX, DOS

3. **Platform-specific implementations**:
   - **POSIX/DOS**: Link `rasgl_core` + `rasgl_loaders` + `log.c` (with FILE I/O)
   - **PSX**: Link `rasgl_core` + PSX-specific `log.c` stub/serial logger
   - **Asset strategy for PSX**: Pre-baked data arrays compiled into executable (no runtime loading)

**Logging Proposal**:
- `debug.c`/`debug.h` stays in `rasgl_core` (uses memory buffers, no direct I/O)
- `log.c` (external lib) is platform-specific:
  - POSIX/DOS: Uses `fprintf()`, `stderr`, FILE I/O
  - PSX: Serial port logger

**Also needed**:

- Packer/unpacker to convert models/scenes to binary blobs for PSX.
- Refactor scene to avoid pointer xref.

## PSX target challenge

Current layout is demo (exe) -> platform (library), similar to SDL. Since PSX uses a special linker, this layout is a challenge.

**Option**: Platform provides build function via CMake function registration.

Each platform defines `ras_build_demo()` function that knows how to build executables for that platform:

```cmake
# src/platform/posix/CMakeLists.txt
function(ras_build_demo DEMO_NAME DEMO_SOURCES)
    add_executable(${DEMO_NAME} ${DEMO_SOURCES})
    target_link_libraries(${DEMO_NAME} ras_sdl core)
endfunction()

# src/platform/psx/CMakeLists.txt
function(ras_build_demo DEMO_NAME DEMO_SOURCES)
    addPS1Executable(${DEMO_NAME} ${DEMO_SOURCES})
    target_link_libraries(${DEMO_NAME} common core)
    addBinaryFileWithSize(${DEMO_NAME} textData textDataSize src/platform/psx/data.txt)
endfunction()

# src/demo/world/CMakeLists.txt
ras_build_demo(world "main.c;scene.c;...")
```

Benefits:

- Demo code is platform-agnostic - just calls one function
- No conditionals in demo CMakeLists
- Each platform controls its own build process

Cons:

- If demo-specific content loading is needed, more abstractions may be required.
  - With the move to scene files, this may be less of an issue.


## Packing

### RasScene Structure

- `RasScene`
  - `name` : char[MAX_SCENE_NAME]
  - `models` : `RasSceneModel*` (pointer to array)
  - `num_models` : `size_t`

  - `RasSceneModel` (each entry in `models`)
    - `name` : char[MAX_SCENE_NAME]
    - `path` : char[MAX_FILE_PATH]
    - `element` : `RasPipelineElement`

  - `objects` : `RasSceneObject*` (pointer to array)
  - `num_objects` : `size_t`

  - `RasSceneObject` (each entry in `objects`)
    - `model_index` : int32_t
    - `position` : `RasVector3f`
    - `rotation` : `RasVector3f`
    - `rotation_delta` : `RasVector3f`
    - `animation` : `RasSceneObjectAnimation*` (nullable)
    - `mesh_index` : uint32_t

  - `RasSceneObjectAnimation` (pointed to by `animation`)
    - `rotation` : `RasSceneObjectAnimationRotation`

  - `RasSceneObjectAnimationRotation`
    - `axis` : `RasVector3f`
    - `speed` : `RasFixed`

  - `gridmaps` : `RasSceneGridMap*` (pointer to array)
  - `num_gridmaps` : `size_t`

  - `tombmaps` : `RasSceneTombMap*` (pointer to array)
  - `num_tombmaps` : `size_t`

  - `cameras` : `RasCamera*` (pointer to array)
  - `num_cameras` : `size_t`

  - `RasCamera` (each entry in `cameras`)
    - `position` : `RasVector3f`
    - `angle` : int32_t
    - `fov` : `RasFixed`
    - `aspect_ratio` : `RasFixed`
    - `near` : `RasFixed`
    - `far` : `RasFixed`
    - `projection_mode` : `RasProjectionMode`
    - `projection_matrix` : `RasFixed[4][4]`
    - `last_changed_frame` : uint32_t

