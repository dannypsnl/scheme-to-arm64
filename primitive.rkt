#lang racket

(provide (all-defined-out))

(define primitive-functions
  '(+
    -
    *
    /
    ; list and pair
    cons
    ; comparison operators
    = < > <= >= char=?
    add1
    sub1
    integer?
    boolean?
    char?
    zero?))
