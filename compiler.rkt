#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(require "emit.rkt"
         "representation.rkt"
         "primitive.rkt")

(define wordsize 4)
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
(define (compile-primitive-call form stack-index)
  (define op (primitive-op form))
  (case op
    [(+ - * /)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (primitive-op-arg form 2) (- stack-index wordsize))
     (emit "ldr w8, [x29, #~a]" stack-index)
     (case op
       [(+) (emit "add w0, w8, w0")]
       [(-) (emit "sub w0, w8, w0")]
       [(*) (emit "lsr w0, w0, #~a" fixnum-shift)
            (emit "mul w0, w8, w0")]
       [(/) (emit "lsr w0, w0, #~a" fixnum-shift)
            (emit "sdiv w0, w8, w0")])]
    [(= < > <= >= char=?)
     (define-label if-true)
     (define-label end)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (when (eq? op 'char=?)
       (emit "lsr w0, w0, #~a" char-shift))
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (primitive-op-arg form 2) (- stack-index wordsize))
     (emit "ldr w8, [x29, #~a]" stack-index)
     (when (eq? op 'char=?)
       (emit "lsr w0, w0, #~a" char-shift))
     (emit "cmp w0, w8")
     (emit "mov w0, #~a" (immediate-rep #f))
     (case op
       [(=) (b.eq if-true)]
       [(<) (b.lt if-true)]
       [(>) (b.gt if-true)]
       [(<=) (b.le if-true)]
       [(>=) (b.ge if-true)]
       [(char=?) (b.eq if-true)])
     (b end)
     (label if-true)
     (emit "mov w0, #~a" (immediate-rep #t))
     (label end)]
    [(add1)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "add w0, w0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "sub w0, w0, #~a" (immediate-rep 1))]
    [(integer?)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "and w0, w0, #~a" fixnum-mask)
     (emit-is-w0-equal-to 0)]
    [(boolean?)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "and w0, w0, #~a" bool-mask)
     (emit-is-w0-equal-to bool-tag)]
    [(char?)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit "and w0, w0, #~a" char-mask)
     (emit-is-w0-equal-to char-tag)]
    [(zero?)
     (compile-expr (primitive-op-arg form 1) stack-index)
     (emit-is-w0-equal-to 0)]))
(define (compile-expr e stack-index)
  (cond
    [(immediate? e)
     (emit "mov w0, #~a" (immediate-rep e))]
    [(primitive-call? e) (compile-primitive-call e stack-index)]))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (emit "stp x29, x30, [sp, #-16]!")
  (emit "mov x29, sp")
  (compile-expr program (- wordsize))
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
