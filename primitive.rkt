#lang racket

(provide (all-defined-out))

(define primitive-functions
  '(add1
    sub1
    integer?
    boolean?
    char?
    zero?))
(define (primitive-call? form)
  (member (car form)
          primitive-functions))
(define (primitive-op form) (car form))
(define (primitive-op-arg form index) (list-ref form index))
