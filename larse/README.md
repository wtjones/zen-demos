# Larse

Lisp-like language for scripting and configuration

## Features

- Parse s-expressions
  - Single s-expression
  - Script as an array of expressions
  - Single-line comments ';'
- Pretty-print expressions

See [TODO](TODO.md) for roadmap.

## Terms

### head symbol

The first symbol of a list.

Example:

```lisp
(baz (boof 1 2 3))    ; baz and boof are both head symbols of their respective lists
```

### property

A non-head symbol prefixed with `:` that is followed by a value.

Example:

```lisp
(foo :my_prop 12) ; my_prop is a property
```

> **Note**
>
> This is only a convention and not enforced.

### object

A list with a head symbol and properties.

```lisp
(foo :my_prop 12); foo is an object
```

> **Note**
>
> This is only a convention and not enforced.

### array

An explicit array is not defined. By convention, it is a list that:

- has a head symbol
- remaining elements are exclusively lists that share a common head element

Example:

```lisp
(objects
    (object :name "foo")
    (object :name "bar")
    (object :name "baz"))
```

### qualified path

An addressible path to an array or property.

```lisp
(settings
    :bork_limit 4.5
    :glagun_mode ultra_max
    :soylents (red blue green))
(trezelderfs
    (trezelderf :val 1)
    (trezelderf :val 1))
```

In this example, the qualified paths are:

- `(settings)`
- `(settings bork_limit)`
- `(settings glagun_mode)`
- `(settings soylents)`
- `(trezelderfs)`

## Build

```bash
# Also contained in run_build.sh

cmake -S . -Bbuild
cmake --build build
```

## Shell Usage

```text
Usage: larse [options] [scriptfile]
 When 'scriptfile' is given, it is loaded.
Informative output:
 -h, --help    - print this help and exit
 -d            - verbose output
 -v            - print the version
Actions:
 -x expressions - execute the expressions, then exit
 -m file-to-merge - apply a 2nd [scriptfile]
```

### Expression example

```bash
./run_shell.sh -x "(super-symbol:x (4 5))"

```

### Script file example

```bash
build/bin/larse tests/data/test1.lsp
```

## Library Usage

### Single expression

```c
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

```text
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

### Logging

Additional logs are enabled by specifying a log file.

```c
FILE* log_file = fopen("/tmp/larse.log", "w");
lar_log_configure(log_file);
```

## Development

`./run_tests.sh`

## DOS

### Build - Docker option

`./build_docker_dos.sh`
`./build_dos.sh`

Optional: Inspect container:

`docker run --rm -it larse-dos bash`

### Build - Local DJGPP option

Build or get DJGPP binaries from <https://github.com/andrewwutw/build-djgpp>.

Extract somewhere such as `/opt/djgpp`.

Set variable `DJGPP_PREFIX` in shell or profile:

```bash
export DJGPP_PREFIX="/opt/djgpp"
# if building with defaults:
export DJGPP_PREFIX="/usr/local/djgpp"

```

Optional: Set PATH or use provided shell script.

`PATH="${DJGPP_PREFIX}/bin:${PATH}"`

### DOSBox

[DOSBox-X](https://github.com/joncampbell123/dosbox-x/) is recommended due to the built-in 32-bit extender.

**Flatpak**

`run_dos.sh` will attempt to use flatpak with $pwd permissions. I'd prefer to wire this up in the shell profile, but had issues with the sandbox.

**Non-flatpak**

```bash
export DOSBOX_BIN="/usr/local/bin/dosbox-x"
```
