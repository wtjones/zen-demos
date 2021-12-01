#! /bin/bash

set -euo pipefail

main() {
    env CTEST_OUTPUT_ON_FAILURE=TRUE cmake --build build -t testlib test
}

main

