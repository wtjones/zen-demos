# lparse

S-expression parse example

## Build/run

cmake -DCMAKE_BUILD_TYPE:STRING=Debug -S . -Bbuild
cmake --build build --config Debug && ./build/lparse

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
    
## Usage

```
cmake --build build --config Debug && ./build/lparse test/test1.lisp
```

## References

https://www.cs.unm.edu/~luger/ai-final2/LISP/CH%2011_S-expressions,%20The%20Syntax%20of%20Lisp.pdf

https://github.com/UWCubeSat/minimee-electronics/blob/master/spaceduino/spaceduino.kicad_pcb
