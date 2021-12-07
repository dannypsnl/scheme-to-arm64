#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(define (emit . args)
  (apply printf args)
  (newline))

(define (compile-expr e)
  (emit "mov w0, #~a" e))

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
