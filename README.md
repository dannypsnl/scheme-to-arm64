# scheme-to-arm64

A scheme to Arm64 compiler that keep simple for teaching purpose.

### Installation

```shell
git submodule update --init
cd bdwgc
./autogen.sh
./configure
make -j 8
make install
raco exe -o scmc main.rkt
raco distribute dist scmc
```

### Usage

```shell
scmc -e '(char=? #\c #\c)'
```

To learn more compiler options

```shell
scmc --help
```

### Known issues

The following problems are known in the current implementation and **not going to fix**!

1. nested vector works badly(the root cause is first 64bits of vector layout stores length of vector, however, it be converted to pointer encoded value rather than length stored)
   1. `#(1 #(2))` produces `#(2)`
   2. `#(3 #(2 4))` produces `#(2 #(2 #(2 ...`(a lots of `#(2 ...)` here)

### Develop

You're assume to familiar with arm64's assembly to contribute. A good start is:

- [Arm64 Assembly Language Notes](https://cit.dixie.edu/cs/2810/arm64-assembly.html)
- [johannst/notes: Arm64](https://johannst.github.io/notes/arch/arm64.html)
- [Hello Silicon: How to program on Apple Silicon Macs](https://github.com/below/HelloSilicon)

Compiler reference:

- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)
- Andy Keep - Writing a Nanopass Compiler
  - [video](https://youtu.be/Os7FE3J-U5Q)
  - [code](https://github.com/akeep/scheme-to-c)
- [Let's Build a Compiler series](https://generalproblem.net/lets_build_a_compiler/01-starting-out/)
