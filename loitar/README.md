# Loitar

An experiment in Lisp

## Build

`./run_build.sh`

## Usage

### Execute expression

`./build/bin/loitar x "(+ 2 2)"`

## Development

Run all tests

`./run_tests.sh`

Run a specific test

`cmake --build build && ./build/bin/testlib "*Atom*"`


## Nodes

### Proposed base node functionality

| Type          | Parent Type   | Example   | `name()`      | `symbol()`    |
| --            | --            | --        | --            | --            |
| AtomNode      | Node          | myvar     | AtomNode      | myvar         |
| StringNode    | AtomNode      | "hello"   | StringNode    | no value      |
| TrueNode      | AtomNode      | t         | TrueNode      | t             |
| NilNode       | AtomNode      | nil       | NilNode       | nil           |
| ListNode      | Node          | (1 2)     | ListNode      | no value      |
| ListNode      | Node          | ()        | ListNode      | nil           |


## Tech Debt

* Too much downcasting
* Paren matching not enforced: `(eq 1 1` parses