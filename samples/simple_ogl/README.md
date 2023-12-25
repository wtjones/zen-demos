Simple SDL2 + legacy OpenGL example



## Run

```
cmake -DCMAKE_BUILD_TYPE:STRING=Debug -DCMAKE_C_COMPILER:FILEPATH=/usr/bin/gcc -S . -Bbuild -G Ninja

cmake --build build --config Debug && ./build/simple_ogl
```

## References

https://marcelfischer.eu/blog/2019/sdl-opengl-cmake/
https://lazyfoo.net/tutorials/SDL/50_SDL_and_opengl_2/index.php
