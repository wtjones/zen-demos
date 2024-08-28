#! /bin/bash

set -euo pipefail

main() {
    cmake --build build
    env CTEST_OUTPUT_ON_FAILURE=TRUE ctest --test-dir build --verbose
}

main

