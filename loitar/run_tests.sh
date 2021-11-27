#! /bin/bash

set -euo pipefail

main() {
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug -Bbuild
    env CTEST_OUTPUT_ON_FAILURE=TRUE cmake --build build -t testlib test
}

main

