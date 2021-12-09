# scheme-compiler

A scheme to Arm64 compiler that keep simple for teaching purpose.

```shell
racket main.rkt -e '(char=? #\c #\c)'
```

### Develop

You're assume to familiar with arm64's assembly to contribute. A good start is:

- [Arm64 Assembly Language Notes](https://cit.dixie.edu/cs/2810/arm64-assembly.html)

#### Git Hooks

If you have [`direnv`](https://direnv.net/), you will get git hooks directly. Or you can use:

```shell
source .envrc
```

to get git hooks. In this repository, git hooks is installed for run tests before push since arm64 is unsupported on GitHub Action yet.
