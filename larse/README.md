# Larse

Lisp-like language for scripting and configuration

## Features

- Parse s-expressions
    - Single s-expression
    - Script as an array of expressions
- Pretty-print expressions

See [TODO](TODO.md) for roadmap.

## Build

```
# Also contained in run_build.sh

cmake -S . -Bbuild
cmake --build build
```

## Shell Usage

### Expression example

```
./run_shell.sh -x "(super-symbol:x (4 5))"

```

## Library Usage

### Single expression

```
#include "larse/core/expression.h"
#include "larse/core/parse.h"
#include "larse/core/repr.h"

int main(int argc, const char** argv)
{
    const char* exp1_in = "(this :here\n"
                          "(is math)\n"
                          "(+ 1 2))";
    LarNode* node1;
    LarParseResult result = lar_parse_single(exp1_in, &node1);
    char* repr = lar_repr_expression(node1);
    printf("repr test:\n%s\n", repr);
    lar_free_expression(&node1);
    free(repr);
}
```

Output:

```
[List]
    this [Atom: symbol]
    :here [Atom: symbol]
    [List]
        is [Atom: symbol]
        math [Atom: symbol]
    [List]
        + [Atom: symbol]
        1 [Atom: integer]
        2 [Atom: integer]
```
## Development

`./run_tests.sh`
