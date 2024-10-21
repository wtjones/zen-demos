# RasGL

## Todo

### General

- [ ] Support CTest

### Rendering

- [ ] Add AABB debug view toggle
- [ ] Render origin and grid lines
- [ ] Validate screen points are in screen bounds
- [ ] Fix poly fill gaps
- [ ] Support DOS poly fill
- [ ] Material colors in filled polygon mode
- [ ] Shaded polygon mode

### Clipping - Polygon

- [ ] Fix assert on certain far plane clip scenarios
- [ ] Fix assert when a triangle is rotating and clipping on the left side
- [ ] AABB should remain correct when model rotates

### Math

- [ ] Increase whole part of fixed-point scheme from 16 bits
- [ ] Fix model rotation origin

### Models

- [ ] Free model on error during load
- [ ] Support material colors

### Scene loading

- [ ] Reference larse as a target or package
- [ ] Add scene explanation to README
- [ ] Support parameterized scene loading

## In Progress

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

### Models

- [x] Fix DOS version not taking model param
- [x] Support dynamic allocation for models

### Rendering

- [x] Backface culling is broken
- [x] Filled polygon mode

### Scene Loading

- [x] Scene camera position
- [x] Scene model position
- [x] Render multiple scene objects
- [x] DOS scene support
