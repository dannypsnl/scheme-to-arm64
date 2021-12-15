#lang racket

(provide (all-defined-out))

(require syntax/parse/define)

(define (emit . args)
  (apply printf args)
  (newline))

(define-syntax-parser define-label
  [(_ label-names ...)
   #'(begin
       (define label-names (gensym 'LLB)) ...)])
