# RasGL

## Todo

### General

- [ ] Support CTest
- [ ] POSIX - screen resolution shouldn't be hard-coded
- [ ] Log buffer overflow - DOS only?
  - Move model to left clip side to trigger.

### Rendering

- [ ] Add AABB debug view toggle
- [ ] Render grid lines
- [ ] Validate screen points are in screen bounds
- [ ] Support DOS poly fill
- [ ] Support DOS color ramps
- [ ] Tidy diagnosic rendering to another module
- [ ] Support render layers in DOS
- [ ] Toggle UI layer
- [ ] Fix naming or consolidate *projected_to_screen_point()

### Lighting

- [ ] Add gamma adjustment

### Clipping - Polygon

- [ ] AABB should remain correct when model rotates.
- [ ] Consider a bitmask for planes to simplify cases such as `core_sg_visible_faces()`.
- [ ] Clipping plane flag sometimes remains on pv.
- [ ] NDC transform should be able to skip vertices based on clip flags.
- [ ] Outline should copy to created faces.

### Math

- [ ] Increase whole part of fixed-point scheme from 16 bits
- [ ] Fix model rotation origin

### Models

- [ ] Free model on error during load
- [ ] Assume muliple usemtl lines can refer to same material

### Scene loading

- [ ] Reference larse as a target or package
- [ ] Add scene explanation to README
- [ ] Support parameterized scene loading
- [ ] Configurable background color
- [ ] Objects should not be mandatory.

### Scene management

- [ ] Add scene reset operation.

### Text

- [ ] Text clipping
  - Might be easier once ortho frustum added.
- [ ] Manage font bitmap via materials
- [ ] Font colors
- [ ] DOS support
- [ ] Height metrics with wrapped text

### Console

- [ ] DOS support
- [ ] Pull down/up animation
- [ ] Prompt echo
- [ ] Size console to visible rows.
- [ ] Scene should continue to animate when visible.
- [ ] Flush is triggered when console option. Remap/disable?

### Input

- [ ] Need a more elegant mapping for repeat-enabled keys.

### Grid

### Tombmap

- [ ] Fix pillar sort.

### Pipeline

- [ ] Support dynamic allocation of meshes.

### Build

- [ ] Auto build larse deps.

## In Progress

### Rendering

### Tombmap

### Text

### Console

### Models

### Logging

- [ ] Summary: Added summary flush hotkey.

## Done ✓

### General

- [x] Consider macro for check result pattern
- [x] Add frustum repr function
- [x] Frame-based log buffering

### Camera

- [x] Consolidate camera with view pos

### Clipping - Object

- [x] Calculate AABB in model space
- [x] Add frustum to poly demo
- [x] Transform object AABB to world
- [x] Set clip flags based on AABB/frustum

### Clipping - Polygon

- [x] Exclude faces outside of frustum
- [x] Clip faces that intersect frustum
- [x] Recalc clip flags when new PVs added
- [x] Carry over pipeline face to clipped polys
- [x] Fix assert on certain far plane clip scenarios
- [x] Fix assert when a triangle is rotating and clipping on the left side
- [x] Fix disappearing poly in some near camera clipping cases.
  - Proposed fix is to perform backface culling in NDC after clipping.

### Models

- [x] Fix DOS version not taking model param
- [x] Support dynamic allocation for models
- [x] Support material colors

### Rendering

- [x] Backface culling is broken
- [x] Filled polygon mode
- [x] Material colors in filled polygon mode
- [x] Shaded polygon mode
- [x] Add flat shading toggle
- [x] Render origin
- [x] Support color ramps
- [x] Fix poly fill gaps

### Scene Loading

- [x] Scene camera position
- [x] Scene model position
- [x] Render multiple scene objects
- [x] DOS scene support

### Scene Management

- [x] Support fractional rotation
- [x] Support object selection
- [x] Add animation disable

### Text

- [x] Implement font bitmap polygon mode
- [x] Text culling
- [x] Font metrics
- [x] Format printing
- [x] UI render toggle

### Console

- [x] Trim ring buffer
- [x] Render buffer text
- [x] Suppress scene input when console active.
- [x] Suppress console input when inactive.
- [x] Separate prompt character from prompt text.
- [x] Recall history with up/down arrows.
- [x] Trim whitespace
- [x] Show cursor
- [x] Add key repeat for backspace

### Grid overlay

- [x] Shade points
- [x] Cull points outside of frustum.

### Pipeline

- [x] Fix culled object warping
- [x] Clip polygons added during clipping
- [x] Copy materials in new clipping pipeline.
- [x] Support dynamic allocation of elements.

### Tombmap

- [x] Render pits (inverted pillars)

### Build

- [x] Use distinct build folders to avoid platform clash.
