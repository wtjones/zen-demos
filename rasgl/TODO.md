# RasGL

### Todo

- [ ] Render origin and grid lines
- [ ] Increase whole part of fixed-point scheme from 16 bits
- [ ] Add AABB debug view toggle
- [ ] Validate screen points are in screen bounds
- [ ] Filled polygon mode
- [ ] Shaded polygon mode
- [ ] Fix assert on certain far plane clip scenarios
- [ ] Scene loading
    [ ] DOS support
- [ ] Fix model rotation origin
- [ ] Support dynamic allocation for models
- [ ] Reference larse as a target or package

### In Progress

- [ ] Scene loading
  - [ ] Scene camera position
  - [ ] Scene model position

### Done ✓

- [x] Add frustum repr function
- [x] Consolidate camera with view pos
- [x] Frame-based log buffering
- [x] Object AABB clipping
  - [x] Calculate AABB in model space
  - [x] Add frustum to poly demo
  - [x] Transform object AABB to world
  - [x] Set clip flags based on AABB/frustum
- [x] Polygon clipping
    - [x] Exclude faces outside of frustum
    - [x] Clip faces that intersect frustum
    - [x] Recalc clip flags when new PVs added
- [x] Fix DOS version not taking model param
