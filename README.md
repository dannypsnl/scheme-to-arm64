# scheme-compiler

A scheme to Arm64 compiler that keep simple for teaching purpose.

```shell
racket main.rkt -e '(char=? #\c #\c)'
```

To learn more compiler options

```shell
racket main.rkt --help
```

### Installation

```shell
git submodule update --init
cd bdwgc
./autogen.sh
./configure
make -j 8
make install
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

Compiler reference:

- [An Incremental Approach to Compiler Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf)
- [Andy Keep - Writing a Nanopass Compiler](https://youtu.be/Os7FE3J-U5Q)
