#! /bin/bash

set -euo pipefail

main() {
    cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=Debug -Bbuild
    cmake --build build
}

main
