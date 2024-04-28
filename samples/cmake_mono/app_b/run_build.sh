#! /bin/bash -x

cmake -S . -B build -DCMAKE_PREFIX_PATH=../lib_a/build
cmake --build build
