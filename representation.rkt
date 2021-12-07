#lang racket/base

(provide (all-defined-out))

(define (immediate-rep x)
  (cond
    [(integer? x) (bitwise-and (arithmetic-shift x fixnum-shift) #xffffffff)]
    [(char? x) (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-tag)]
    [(boolean? x)
     (if x
         (bitwise-ior (arithmetic-shift 1 bool-shift) bool-tag)
         bool-tag)]))

(define fixnum-shift 2)
(define fixnum-mask 3)

(define ptr-mask 7) ; mask for pointer type tag
(define ptr-mask-inv #xfffffff8) ; mask for pointer value

(define pair-tag 1)
(define vec-tag 2)
(define str-tag 3)
(define sym-tag 5)
(define closure-tag 6)

(define char-mask 255) ; character type mask
(define char-shift 8)
(define char-tag 7)

(define bool-mask 255)
(define bool-shift 8)
(define bool-tag 15)
