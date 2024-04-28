# CMake mono repo package example

A demonstration of dependency management between CMake projects that are peers in a mono repo.


## Overview

The possible approaches:

- Module mode
- Config mode
- FetchContent

The current approach is to utilize config mode.


## TODO

[ ] Resolve error `undefined reference to 'parse_int'`

## GPT prompt

```
go install github.com/chand1012/git2gpt@latest
~/go/bin/git2gpt --ignore .gptignore . > out.txt
```