#! /bin/bash -x


SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
rm -rf ${SCRIPT_DIR}/build

cmake -S . -B build -DCMAKE_PREFIX_PATH=../lib_a/build
#cmake -S . -B build
cmake --build build
