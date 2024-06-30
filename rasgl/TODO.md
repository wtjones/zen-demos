# RasGL

## Todo

### General

- [ ] Support CTest

### Rendering

- [ ] Add AABB debug view toggle
- [ ] Render origin and grid lines
- [ ] Validate screen points are in screen bounds
- [ ] Filled polygon mode
- [ ] Shaded polygon mode

### Clipping - Polygon

- [ ] Fix assert on certain far plane clip scenarios

### Math

- [ ] Increase whole part of fixed-point scheme from 16 bits
- [ ] Fix model rotation origin

### Models

- [ ] Support dynamic allocation for models

### Scene loading

- [ ] DOS scene support
- [ ] Reference larse as a target or package
- [ ] Add scene explanation to README

## In Progress

### Scene loading

- [ ] Render multiple scene objects

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

### Scene Loading

- [x] Scene camera position
- [x] Scene model position
