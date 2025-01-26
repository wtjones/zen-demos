# Maze in Common Lisp

## Usage

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
