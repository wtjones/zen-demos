#!/bin/bash

sbcl --noinform  \
    --load maze.asd \
    --eval '(asdf:load-system :maze)' \
    --eval '(maze:main)' --quit
