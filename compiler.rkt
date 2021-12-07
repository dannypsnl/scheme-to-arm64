#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(define (emit . args)
  (apply printf args)
  (newline))

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

(define (immediate-rep x)
  (cond
    [(integer? x) (bitwise-and (arithmetic-shift x fixnum-shift) #xffffffff)]
    [(char? x) (bitwise-ior (arithmetic-shift (char->integer x) char-shift) char-tag)]
    [(boolean? x)
     (if x
         (bitwise-ior (arithmetic-shift 1 bool-shift) bool-tag)
         bool-tag)]))

(define (compile-expr e)
  (emit "mov w0, #~a" (immediate-rep e)))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (compile-expr program)
  (emit "ret")
  )

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (system "gcc /tmp/scheme.s rts.c"))

(define (compile-and-run program)
  (begin (compile-to-binary program)
         (system "./a.out")))
