#lang racket

(provide (all-defined-out))

(define primitive-functions
  '(+
    -
    *
    /
    add1 sub1
    zero?
    ; list and pair
    car cdr cons
    null?
    ; comparison operators
    = < > <= >= char=?
    ; type check
    integer?
    boolean?
    char?))
