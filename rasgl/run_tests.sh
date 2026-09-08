#!/bin/bash

set -euo pipefail

DEBUG=${DEBUG:-0}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

if [[ -z ${1:-} ]]; then
	TEST_ARG=""
else
	TEST_ARG="-R ${1}"
fi

main() {
    cmake -S . -DDEBUG=$DEBUG -Bbuild -DENABLE_ASAN=ON
    cmake --build build
    env CTEST_OUTPUT_ON_FAILURE=TRUE ctest --test-dir build --verbose $TEST_ARG
}

main
