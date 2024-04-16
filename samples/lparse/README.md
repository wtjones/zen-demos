# lparse

S-expression parse example


## Expression structure

```
(this (is an) (s expression))
```

- list
    - atom
    - list
        - atom
        - atom
    - list
        - atom
        - atom

## Node types

### string

Starts and ends with double quotes.

### number-integer

Starts with a digit or +/- followed by at least one digit.

### number-decimal

TODO

### boolean

true or false

### symbol

* Not string/number/boolean.
* Can contain letters, digits, and certain special characters.
* The first character of a symbol cannot be a digit.
* Special characters allowed in symbols include +, -, *, /, _, !, ?, $, %, &, =, <, >, @, ^, ~, and :.



## Build/run

### Normal

```
cmake -D CMAKE_BUILD_TYPE:STRING=Debug -D CMAKE_C_FLAGS="-g3" -S . -Bbuild
cmake --build build --config Debug && ./build/lparse test/test1.lisp
```

### Sanitizer

`setarch` is used to avoid random [os-level sanitizer errors](https://github.com/google/sanitizers/issues/1724#issuecomment-2005714323)

```
cmake -D CMAKE_BUILD_TYPE:STRING=Debug -D CMAKE_C_FLAGS="-g3 -fsanitize=address" -S . -Bbuild

cmake --build build --config Debug && setarch `uname -m` -R ./build/lparse test/test1.lisp
```

## TODO

[ ] 32.32 fixed point


## References

https://www.cs.unm.edu/~luger/ai-final2/LISP/CH%2011_S-expressions,%20The%20Syntax%20of%20Lisp.pdf

https://github.com/UWCubeSat/minimee-electronics/blob/master/spaceduino/spaceduino.kicad_pcb
