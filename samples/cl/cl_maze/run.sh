#!/bin/bash

if [ "$#" -eq 0 ]; then
  sbcl --noinform --non-interactive \
       --load maze.asd \
       --eval '(require :maze)' \
       --eval '(maze:main)' \
       --quit
else
  sbcl --noinform --non-interactive \
       --load maze.asd \
       --eval '(require :maze)' \
       --eval '(maze:main)' \
       --quit "$@"
fi









# #!/bin/bash


# args=$(printf '"%s" ' "$@")

# sbcl --noinform --non-interactive --load cl-maze.asd --eval '(asdf:load-system :cl-maze)' --eval "(cl-maze:main $args)" --quit
