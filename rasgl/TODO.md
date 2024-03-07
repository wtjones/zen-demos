# RasGL

### Todo

- [ ] Render origin and grid lines
- [ ] Increase whole part of fixed-point scheme from 16 bits
- [ ] Fix DOS version not taking model param
- [ ] Add AABB debug view toggle

### In Progress

- [ ] Polygon clipping
    - [x] Exclude faces outside of frustum
    - [x] Clip faces that intersect frustum
    - [x] Recalc clip flags when new PVs added

### Done ✓

- [x] Add frustum repr function
- [x] Consolidate camera with view pos
- [x] Frame-based log buffering
- [x] Object AABB clipping
  - [x] Calculate AABB in model space
  - [x] Add frustum to poly demo
  - [x] Transform object AABB to world
  - [x] Set clip flags based on AABB/frustum
