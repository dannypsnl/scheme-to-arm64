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
  (define-label if-true end)
  (emit "cmp w0, #~a" (immediate-rep val))
  (b.eq if-true)
  (emit "mov w0, #~a" (immediate-rep #f))
  (b end)
  (label if-true)
  (emit "mov w0, #~a" (immediate-rep #t))
  (label end))
(define (compile-primitive-call op args stack-index env)
  (case op
    [(+ - * /)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (list-ref args 1) (- stack-index wordsize) env)
     (emit "ldr w8, [x29, #~a]" stack-index)
     (case op
       [(+) (emit "add w0, w8, w0")]
       [(-) (emit "sub w0, w8, w0")]
       [(*) (emit "lsr w0, w0, #~a" fixnum-shift)
            (emit "mul w0, w8, w0")]
       [(/) (emit "lsr w0, w0, #~a" fixnum-shift)
            (emit "sdiv w0, w8, w0")])]
    [(= < > <= >= char=?)
     (define-label if-true end)
     (compile-expr (list-ref args 0) stack-index env)
     (when (eq? op 'char=?)
       (emit "lsr w0, w0, #~a" char-shift))
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (list-ref args 1) (- stack-index wordsize) env)
     (emit "ldr w8, [x29, #~a]" stack-index)
     (when (eq? op 'char=?)
       (emit "lsr w0, w0, #~a" char-shift))
     (emit "cmp w0, w8")
     (case op
       [(=) (b.eq if-true)]
       [(<) (b.lt if-true)]
       [(>) (b.gt if-true)]
       [(<=) (b.le if-true)]
       [(>=) (b.ge if-true)]
       [(char=?) (b.eq if-true)])
     (emit "mov w0, #~a" (immediate-rep #f))
     (b end)
     (label if-true)
     (emit "mov w0, #~a" (immediate-rep #t))
     (label end)]
    [(add1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "add w0, w0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "sub w0, w0, #~a" (immediate-rep 1))]
    [(integer?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and w0, w0, #~a" fixnum-mask)
     (emit-is-w0-equal-to 0)]
    [(boolean?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and w0, w0, #~a" bool-mask)
     (emit-is-w0-equal-to bool-tag)]
    [(char?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and w0, w0, #~a" char-mask)
     (emit-is-w0-equal-to char-tag)]
    [(zero?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit-is-w0-equal-to 0)]))

(define (compile-let names exprs body stack-index env)
  (let* ([stack-offsets
          (map (lambda (x) (- stack-index (* x wordsize)))
               (range 0 (length names)))]
         [inner-si (- stack-index (* (length names) wordsize))]
         [inner-env (append (map cons names stack-offsets) env)])
    ; evaluate exprs and assign them to stack locations
    (for ([expr exprs]
          [offset stack-offsets])
      (compile-expr expr inner-si env)
      (emit "str w0, [x29, #~a]" offset))

    ; evaluate all body forms - this will leave the last one's output in %eax
    (for ([form body])
      (compile-expr form inner-si inner-env))))

(define (variable? x) (symbol? x))
(define (compile-var-load v stack-index env)
  (emit "ldr w0, [x29, #~a]" (cdr (assoc v env))))

(define (compile-if test t-body f-body stack-index env)
  (define-label if-true end)
  (compile-expr test stack-index env)
  (emit-is-w0-equal-to #t)
  (b.eq if-true)
  (compile-expr f-body stack-index env)
  (b end)
  (label if-true
         (compile-expr t-body stack-index env))
  (label end))

(define (compile-cond tests bodys stack-index env)
  (define-label end)
  (for ([test tests]
        [exprs bodys])
    (define-label body-tag next)
    (compile-expr test stack-index env)
    (emit-is-w0-equal-to #t)
    (b.ne next)
    (label body-tag
           (for ([expr exprs])
             (compile-expr expr stack-index env))
           (b end))
    (label next))
  (label end))

(define (compile-expr e stack-index env)
  (match e
    [(? immediate? e) (emit "mov w0, #~a" (immediate-rep e))]
    [(? variable? e) (compile-var-load e stack-index env)]
    [`(if ,test ,t-body) (compile-if test t-body #f stack-index env)]
    [`(if ,test ,t-body ,f-body) (compile-if test t-body f-body stack-index env)]
    [`(cond (,tests ,bodys ...) ...)
     (compile-cond tests bodys stack-index env)]
    [`(let ([,ids ,exprs] ...) ,bodys ...)
     (compile-let ids exprs bodys stack-index env)]
    [`(,op ,args ...)
     #:when (member op primitive-functions)
     (compile-primitive-call op args stack-index env)]
    [else (error "unkown expression ~a" e)]))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (emit "stp x29, x30, [sp, #-16]!")
  (emit "mov x29, sp")
  (compile-expr program (- wordsize) '())
  (emit "ret"))

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (system "gcc /tmp/scheme.s rts.c"))

(define (compile-and-run program)
  (begin
    (compile-to-binary program)
    (system "./a.out")))

(module+ test
  (require rackunit)

  (define (expr->asm e)
    (with-output-to-file "/tmp/test.s"
      #:exists 'replace
      (lambda () (compile-expr e (- wordsize) '())))
    (file->lines "/tmp/test.s"))

  (check-equal? (expr->asm '1)
                '("mov w0, #4"))
  (check-equal? (expr->asm '(add1 1))
                '("mov w0, #4"
                  "add w0, w0, #4"))
  )
