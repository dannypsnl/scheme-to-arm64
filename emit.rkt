#lang racket/base

(provide (all-defined-out))

(define (emit . args)
  (apply printf args)
  (newline))
(define (label name) (emit "~a:" name))
(define (b.eq label) (emit "b.eq ~a" label))
(define (b label) (emit "b ~a" label))
