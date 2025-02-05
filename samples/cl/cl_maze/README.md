# Maze in Common Lisp

## Usage

```lisp
(load "maze.asd")
(asdf:load-system :maze)
```

Random seed

```lisp
(let ((mz (maze::generate-maze 5 9)))
  (maze::print-maze mz))
```

Specific seed

```lisp
(let ((mz (maze::generate-maze 5 9 128)))
  (maze::print-maze mz))
```

## Maze Gen

Determine size of maze constrained to odd dimentions.

Example: 9 x 9

_Pillars_ are the anchor points for possible starting point for walls spans.

```text
+——————————————————+
|                  |
|  ██  ██  ██  ██  |
|                  |
|  ██  ██  ██  ██  |
|                  |
|  ██  ██  ██  ██  |
|                  |
|  ██  ██  ██  ██  |
|                  |
+——————————————————+
```

Logic

```text
while !pillarsRemain:
    Choose an unused pillar at random // unused means no wall yet
    Choose a random direction
    Move along direction:
        If out-of-bounds: continue
        If wall: continue
        Set wall // this will overwrite pillars
```

Generated maze

```text
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓          ░░      ▓▓
▓▓  ░░░░░░  ░░  ░░  ▓▓
▓▓  ░░      ░░  ░░  ▓▓
▓▓░░░░  ░░  ░░  ░░  ▓▓
▓▓      ░░  ░░  ░░  ▓▓
▓▓  ░░░░░░  ░░  ░░░░▓▓
▓▓      ░░          ▓▓
▓▓  ░░░░░░░░░░  ░░  ▓▓
▓▓      ░░      ░░  ▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
```

Goal generation

Step 1: Determine goal start by walking clockwize top-left to bottom-right until a 'leaf' is found.

In this example, the first spot is a leaf, and walking is not needed.

```
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓>>    ░░      ░░      ░░      ▓▓
▓▓░░░░  ░░  ░░░░░░  ░░░░░░  ░░░░▓▓
▓▓                              ▓▓
▓▓░░░░░░░░░░░░░░░░░░░░░░░░░░░░  ▓▓
▓▓                            EE▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
```

Step 2: Determine goal end by walking clockwise bottom-right to top-left until a leaf is found.

In this example, the bottom-right starting point is not a leaf, so the cursor walks clockwise until the first leaf is found.

```
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓EE    ░░      ░░      ░░      ▓▓
▓▓░░░░  ░░  ░░░░░░  ░░░░░░  ░░░░▓▓
▓▓                              ▓▓
▓▓░░░░░░░░░░░░░░░░░░░░░░░░░░░░  ▓▓
▓▓👍                          <<▓▓
▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
```

Result

```text
▓▓SS▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
▓▓      ░░      ░░      ░░      ▓▓
▓▓░░░░  ░░  ░░░░░░  ░░░░░░  ░░░░▓▓
▓▓                              ▓▓
▓▓░░░░░░░░░░░░░░░░░░░░░░░░░░░░  ▓▓
▓▓                              ▓▓
▓▓  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
```

There are special cases where start and land on the same spot. The end goal range is moved over two spots from either corner to avoid.
