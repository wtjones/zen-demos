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

```text
while !pillarsRemain:
    Choose an unused pillar at random // unused means no wall yet
    Choose a random direction
```
