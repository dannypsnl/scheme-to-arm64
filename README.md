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

### Develop

You're assume to familiar with arm64's assembly to contribute. A good start is:

- [Arm64 Assembly Language Notes](https://cit.dixie.edu/cs/2810/arm64-assembly.html)
- [johannst/notes: Arm64](https://johannst.github.io/notes/arch/arm64.html)
