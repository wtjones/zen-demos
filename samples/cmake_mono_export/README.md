# CMake mono repo export target example

A demonstration of dependency management between CMake projects that are peers in a mono repo.


## Structure

```
/repo
    /app_b
        CMakeLists.txt
    /lib_a
        CMakeLists.txt
```

### Export

Library lib_a uses `EXPORT(TARGETS ...)` to create lib_aTarget.cmake

From [docs](https://cmake.org/cmake/help/latest/command/export.html#export):

> Export targets or packages for outside projects to use them directly from the current project's build tree, without installation.

### Import

Executible app_b uses [`IMPORTED` targets](https://cmake.org/cmake/help/latest/guide/importing-exporting/index.html#importing-libraries) to link the library.

## Usage

In /lib_a:

`./run_build.sh`

In /app_b:

```
./run_build.sh
./build/app_b
```

## Analysis

### Pros

- Simpler to get working than `find_package`

### Cons

- The build for `lib_a` is not automatically triggered.
- Doesn't seem as clean as `find_package`.  

## GPT prompt

```
go install github.com/chand1012/git2gpt@latest
~/go/bin/git2gpt --ignore .gptignore . > out.txt
```