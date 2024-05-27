# CMake mono repo package example

A demonstration of dependency management between CMake projects that are peers in a mono repo.


## Overview

The possible approaches:

- Module mode of `find_package()`
- Config mode of `find_package()`
- FetchContent
- Direct reference

The current approach is to directly reference the sibling directory.


## Usage

### POSIX

```
pushd lib_a
./run_build.sh
popd
pushd app_b
./run_build.sh

```

### DOS

See RasGL in the repo root regarding DJGPP setup.

```
pushd app_b
./run_dos.sh
```

## TODO

- [ ] DOS build is CMake-like.
  - Proper find_package() usage is likley needed.


## GPT prompt

```
go install github.com/chand1012/git2gpt@latest
~/go/bin/git2gpt --ignore .gptignore . > out.txt
```