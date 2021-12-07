#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(require "emit.rkt"
         "representation.rkt"
         "primitive.rkt")

(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (emit-is-w0-equal-to val)
  (define-label if-true)
  (define-label end)
  (emit "cmp w0, #~a" (immediate-rep val))
  (emit "mov w0, #~a" (immediate-rep #f))
  (b.eq if-true)
  (b end)
  (label if-true)
  (emit "mov w0, #~a" (immediate-rep #t))
  (label end))
(define (compile-primitive-call form)
  (case (primitive-op form)
    [(+)
     (compile-expr (primitive-op-arg form 1))
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     (emit "mov w9, w0")
     (emit "add	w0, w8, w9")]
    [(-)
     (compile-expr (primitive-op-arg form 1))
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     (emit "mov w9, w0")
     (emit "sub w0, w8, w9")]
    [(*)
     (compile-expr (primitive-op-arg form 1))
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     (emit "mov w9, w0")
     (emit "lsr w9, w9, #~a" fixnum-shift)
     (emit "mul w0, w8, w9")]
    [(/)
     (compile-expr (primitive-op-arg form 1))
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     (emit "mov w9, w0")
     (emit "lsr w9, w9, #~a" fixnum-shift)
     (emit "sdiv w0, w8, w9")]
    [(= < > <= >=)
     (define-label if-true)
     (define-label end)
     (compile-expr (primitive-op-arg form 1))
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     (emit "mov w9, w0")
     (emit "cmp w8, w9")
     (emit "mov w0, #~a" (immediate-rep #f))
     (case (primitive-op form)
       [(=) (b.eq if-true)]
       [(<) (b.lt if-true)]
       [(>) (b.gt if-true)]
       [(<=) (b.le if-true)]
       [(>=) (b.ge if-true)])
     (b end)
     (label if-true)
     (emit "mov w0, #~a" (immediate-rep #t))
     (label end)]
    [(char=?)
     (define-label if-true)
     (define-label end)
     (compile-expr (primitive-op-arg form 1))
     ; remove tag bits
     (emit "lsr w0, w0, #~a" char-shift)
     (emit "mov w8, w0")
     (compile-expr (primitive-op-arg form 2))
     ; remove tag bits
     (emit "lsr w0, w0, #~a" char-shift)
     (emit "cmp w8, w0")
     (emit "mov w0, #~a" (immediate-rep #f))
     (b.eq if-true)
     (b end)
     (label if-true)
     (emit "mov w0, #~a" (immediate-rep #t))
     (label end)]
    [(add1)
     (compile-expr (primitive-op-arg form 1))
     (emit "add w0, w0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (primitive-op-arg form 1))
     (emit "sub w0, w0, #~a" (immediate-rep 1))]
    [(integer?)
     (compile-expr (primitive-op-arg form 1))
     (emit "and w0, w0, #~a" fixnum-mask)
     (emit-is-w0-equal-to 0)]
    [(boolean?)
     (compile-expr (primitive-op-arg form 1))
     (emit "and w0, w0, #~a" bool-mask)
     (emit-is-w0-equal-to bool-tag)]
    [(char?)
     (compile-expr (primitive-op-arg form 1))
     (emit "and w0, w0, #~a" char-mask)
     (emit-is-w0-equal-to char-tag)]
    [(zero?)
     (compile-expr (primitive-op-arg form 1))
     (emit-is-w0-equal-to 0)]))
(define (compile-expr e)
  (cond
    [(immediate? e)
     (emit "mov w0, #~a" (immediate-rep e))]
    [(primitive-call? e) (compile-primitive-call e)]))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (compile-expr program)
  (emit "ret"))

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (system "gcc /tmp/scheme.s rts.c"))

(define (compile-and-run program)
  (begin
    (compile-to-binary (read (open-input-string program)))
    (system "./a.out")))
