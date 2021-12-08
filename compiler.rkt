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
(define (compile-primitive-call form stack-index env)
  (define op (primitive-op form))
  (case op
    [(+ - * /)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (primitive-op-arg form 2) (- stack-index wordsize) env)
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
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (when (eq? op 'char=?)
       (emit "lsr w0, w0, #~a" char-shift))
     (emit "str w0, [x29, #~a]" stack-index)
     (compile-expr (primitive-op-arg form 2) (- stack-index wordsize) env)
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
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "add w0, w0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "sub w0, w0, #~a" (immediate-rep 1))]
    [(integer?)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "and w0, w0, #~a" fixnum-mask)
     (emit-is-w0-equal-to 0)]
    [(boolean?)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "and w0, w0, #~a" bool-mask)
     (emit-is-w0-equal-to bool-tag)]
    [(char?)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit "and w0, w0, #~a" char-mask)
     (emit-is-w0-equal-to char-tag)]
    [(zero?)
     (compile-expr (primitive-op-arg form 1) stack-index env)
     (emit-is-w0-equal-to 0)]))

(define (let? x) (eq? (car x) 'let))
(define (compile-let bindings body stack-index env)
  (let* ([stack-offsets
          (map (lambda (x) (- stack-index (* x wordsize)))
               (range 0 (length bindings)))]
         [names (map car bindings)]
         [exprs (map cadr bindings)]
         [inner-si (- stack-index (* (length bindings) wordsize))]
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

(define (if? x) (eq? (car x) 'if))
(define (compile-if test t-body f-body stack-index env)
  (define-label if-true if-false end)
  (compile-expr test stack-index env)
  (emit-is-w0-equal-to #t)
  (b.eq if-true)
  (b if-false)
  (label if-true
         (compile-expr t-body stack-index env)
         (b end))
  (label if-false
         (compile-expr f-body stack-index env)
         (b end))
  (label end))

(define (compile-expr e stack-index env)
  (cond
    [(immediate? e) (emit "mov w0, #~a" (immediate-rep e))]
    [(variable? e) (compile-var-load e stack-index env)]
    [(if? e) (compile-if (cadr e) (caddr e)
                         (if (null? (cdddr e)) #f (cadddr e))
                         stack-index env)]
    [(let? e) (compile-let (cadr e) (cddr e) stack-index env)]
    [(primitive-call? e) (compile-primitive-call e stack-index env)]))

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
