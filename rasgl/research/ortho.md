# Orthographic Pixel Mapping Notes

## Right-Handed Coordinate System

In a right-handed coordinate system:

+X = right
+Y = up
+Z = toward the viewer (or out of the screen)

## Pixel Boundaries vs Pixel Centers

Screen coordinates usually represent **pixel boundaries**, not pixel indices.

For example, an 8-pixel-wide region spans:

```text
x = 0 → 8
```

This covers pixels:

```text
0 1 2 3 4 5 6 7
```

The right edge lands at `x = 8` because it is the boundary *after* pixel 7.

---

## Pixel Coverage

Pixels occupy intervals between integer coordinates:

```text
pixel 0 => [0,1]
pixel 1 => [1,2]
...
pixel 7 => [7,8]
```

Pixel centers are typically:

```text
0.5, 1.5, 2.5, ...
```

Rasterizers determine coverage using these centers.

---

## 8x8 Quad Example

An 8×8 bitmap quad should use:

```text
(0,0)
(8,0)
(8,8)
(0,8)
```

---

## Vertices to pixels

Vertices define **edges**, not pixels.

So:

```text
quad width = right - left
```

Meaning:

```text
8 - 0 = 8 pixels
```

---

## Common Ortho Projection

Top-left origin:

```text
left   = 0
right  = screen_width
top    = 0
bottom = screen_height
```

A sprite at `(0,0)` with size `8x8` occupies:

```text
x = 0 → 8
y = 0 → 8
```

covering screen pixels:

```text
x = 0..7
y = 0..7
```

---

## Summary

In orthographic rendering:

* Integer coordinates represent pixel boundaries
* Pixels live between those boundaries
* An 8-pixel-wide object spans `0→8`
* The final covered pixel is still `7`

Getting a transformed vertex at `x = 8` is correct for an 8-pixel-wide quad.
