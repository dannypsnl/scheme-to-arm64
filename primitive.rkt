#lang racket

(provide (all-defined-out))

(define primitive-functions
  '(+
    -
    *
    /
    ; comparison operators
    = < > <= >= char=?
    add1
    sub1
    integer?
    boolean?
    char?
    zero?))
