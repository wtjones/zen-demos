# Python visualizations relating to scaling and rotation

## Setup

```
$ cd py_demos
$ pip3 install -r requirements.txt
```

## Fixed math

### Run visualization

`$ python3 -m demos.sin_cos_fixed`

### Fixed math calculator

Mode 10: Float to fixed 8.8

```
$ python3 demos/fixed_math.py 10 1.6

mode 10: float to fixed 8.8...
float 1.6 as fixed: 0000000110011001
result: 409

```

Mode 20: Fixed to float

```
$ python3 demos/fixed_math.py 20 5000000

mode 20: int to fixed 16.16...
input: 5000000
input 5000000 as fixed: 00000000010011000100101101000000
result: 76.2939453125

```

## Mode 7 demo

`$ python3 -m demos.mode_7`

## 2D rotation demo

`$ python3 -m demos.rotate_2d`

