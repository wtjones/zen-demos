#! /bin/bash

set -euo pipefail

if [[ -z ${1:-} ]]; then
	TEST_ARG=""
else
	TEST_ARG="-R ${1}"
fi

main() {
    cmake -S . -Bbuild -DENABLE_ASAN=ON
    cmake --build build
    env CTEST_OUTPUT_ON_FAILURE=TRUE ctest --test-dir build --verbose $TEST_ARG
}

main

