#!/bin/bash

sbcl --noinform --non-interactive \
    --load dsl.asd \
    --eval '(require :dsl)' \
    --eval '(asdf:test-system :dsl/tests)' \
    --quit


