# Demos relating to scaling and rotation

## Python Demos

```
cd py_demos
pip3 install -r requirements.txt
```

`python3 -m sin_cos_fixed`

`python3 -m demos.mode_7`

`python3 -m demos.rotate_2d`

## Game Boy mode 7 demo

```
cd mode7-gb
make -B
```

Load the rom in `/build` or use `run.sh`

### Renderers

#### A

Default renderer. Uses a buffer to process the screen-space rotation.

#### B

Performs rotation and 'sub-tile' translation in the same pass. Runs slightly slower.

`make renderer_b -B`

## Samples

* [xform](samples/xform/README.md) Simple matrix transforms in C
