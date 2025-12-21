#!/bin/bash

OPTION_1=${1:--x}
EXP=${2:-(my-example (some-list (vals 1 2 3) "hi"))}

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cmake -S . -Bbuild
cmake --build build

${SCRIPT_DIR}/build/bin/larse "$OPTION_1" "$EXP"
