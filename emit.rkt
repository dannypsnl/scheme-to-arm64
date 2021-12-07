#lang racket

(provide (all-defined-out))

(require syntax/parse/define)

(define (emit . args)
  (apply printf args)
  (newline))
(define (b.eq label) (emit "b.eq ~a" label))
(define (b.lt label) (emit "b.lt ~a" label))
(define (b.le label) (emit "b.le ~a" label))
(define (b.gt label) (emit "b.gt ~a" label))
(define (b.ge label) (emit "b.ge ~a" label))
(define (b label) (emit "b ~a" label))

(define-syntax-parser label
  [(_ name emits ...)
   #'(begin
       (emit "~a:" name)
       emits ...)])

(define-syntax-parser define-label
  [(_ label-name)
   #'(begin
       (define label-name (gensym 'LLB)))])
