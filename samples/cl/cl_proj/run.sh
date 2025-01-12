#!/bin/bash

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
args=$(printf '"%s" ' "$@")

sbcl --noinform --non-interactive --load $SCRIPT_DIR/sample.asd --eval "(asdf:load-system :sample)" --eval "(sample:main $args)" --quit

