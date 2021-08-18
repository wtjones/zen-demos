## Proposed Structure

```
/src
    /demo
        demo_main.c     // uses gfx
    /platform
        /posix
            /main.c     // calls demo_main()
            /video.c
        /dos
            /main.c
            /video.c
    /gfx
        gfx.c           // uses video

```