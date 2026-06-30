# Merge operations

## Terms

### mege addressible path

A path without a list selector to either:

- a property `(settings max_colors) ; a path`
  - Assuming that `:max_colors` is a property, `settings` is mergable.
- an array `(settings colors) ; a path`
  - Assuming that `(colors 10 20 30)` is an array, `colors` is replacable in a merge.

### object map

A list with an optional head symbol, followed by 1 or more lists with unique head symbols.

Example:

```lisp
(
    (regions east1 west2)
    (zones
        (zone_a :status up)
        (zone_b :status up)))
```

```lisp
(cloud
    (regions east1 west2)
    (zones
        (zone_a :status up)
        (zone_b :status up)))
```

Detect:

- For each head symbol, count matching head symbols. If > 1, return false;

### object array

A list with an optional head symbol followed by 2 or more object lists with the same head symbol.

Example:

```lisp
(objects
    (object :name "foo")
    (object :name "bar")
    (object :name "baz"))
```

```lisp
((object :name "foo")
    (object :name "bar")
    (object :name "baz"))
```

## Merge flow

```lisp
Example 1
; outer is script expressions
; it is not a true s-exp, just a container of list expressions
(settings   ; list of objects
    (keymaps
        (key_a 0x44))
    (screen
        :width 5
        :height 10))
(revision)

; script to merge
(settings   ; list of objects
    (keymaps
        (key_a 0x47)
        (key_b 0x44))
    (screen
        :width 6
        :height 10
        :depth 33))
(revision :version v1) ; should replace base
```

Requirements for an object map merge:

- src0 must have unique head symbols (object map)
- src1 must have unique head symbols (object map)


Call `merge_list(src0, src1, dest)`
Determine if outer list is of either:

- object array
- object map
- object
- list only ; a list with

If object map:
    call `merge_object_map(src0, src1, dest)`
        for each list in src0:
            append empty list to dest
            search src1 for list with same head symbol
            if found:
                call `merge_list()`
            else:
                call `copy_list()`
If object array
