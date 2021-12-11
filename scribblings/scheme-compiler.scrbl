#lang scribble/manual
@require[@for-label[racket/base]]

@title{scheme-compiler}
@author{linzizhuan}

A Scheme to Arm64 compiler. The following is usage

Run single expression

@codeblock|{
racket main.rkt -e '(< 1 2 3)'
}|

Dump disassemble result of the expression

@codeblock|{
racket main.rkt -e '(< 1 2 3)' -s
}|

Debug the expression

@codeblock|{
racket main.rkt -e '(< 1 2 3)' -d
}|

@section{Wait to implement}

Run a single file as script.
