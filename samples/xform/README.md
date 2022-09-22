
```
cmake -DCMAKE_BUILD_TYPE:STRING=Debug -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/gcc -S . -Bbuild -G Ninja

cmake --build build --config Debug && ./build/xform
```