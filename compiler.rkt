#lang racket

(provide compile-program
         compile-to-binary)

(define (emit . args)
  (apply printf args)
  (newline))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (emit "mov w0, #42")
  (emit "ret")
  )

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (system "gcc /tmp/scheme.s rts.c"))
