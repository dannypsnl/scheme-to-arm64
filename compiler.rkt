#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(require "emit.rkt"
         "representation.rkt"
         "primitive.rkt")

(define wordsize 4)
(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))

(define (emit-is-x0-equal-to val)
  (define-label if-true end)
  (emit "cmp x0, #~a" val)
  (b.eq if-true)
  (emit "mov x0, #~a" (immediate-rep #f))
  (b end)
  (label if-true)
  (emit "mov x0, #~a" (immediate-rep #t))
  (label end))
(define (compile-primitive-call op args stack-index env)
  (case op
    [(cons)
     ; store car/cdr to heap
     (compile-expr (list-ref args 0) stack-index env)
     (emit "str x0, [x28]")
     (compile-expr (list-ref args 1) stack-index env)
     (emit "str x0, [x28, #~a]" wordsize)
     ; save pointer and tag it, then increment heap ptr
     (emit "mov x0, x28")
     (emit "orr x0, x0, #~a" pair-tag)
     ; we used two wordsize from heap
     (emit "add x28, x28, #~a" (* 2 wordsize))]
    [(+ - * /)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "str x0, [x29, #~a]" stack-index)
     (compile-expr (list-ref args 1) (- stack-index wordsize) env)
     (emit "ldr x8, [x29, #~a]" stack-index)
     (case op
       [(+) (emit "add x0, x8, x0")]
       [(-) (emit "sub x0, x8, x0")]
       [(*) (emit "lsr x0, x0, #~a" fixnum-shift)
            (emit "mul x0, x8, x0")]
       [(/) (emit "lsr x0, x0, #~a" fixnum-shift)
            (emit "sdiv x0, x8, x0")])]
    [(= < > <= >= char=?)
     (define-label if-true end)
     (compile-expr (list-ref args 0) stack-index env)
     (when (eq? op 'char=?)
       (emit "lsr x0, x0, #~a" char-shift))
     (emit "str x0, [x29, #~a]" stack-index)
     (compile-expr (list-ref args 1) (- stack-index wordsize) env)
     (emit "ldr x8, [x29, #~a]" stack-index)
     (when (eq? op 'char=?)
       (emit "lsr x0, x0, #~a" char-shift))
     (emit "cmp x8, x0")
     (case op
       [(=) (b.eq if-true)]
       [(<) (b.lt if-true)]
       [(>) (b.gt if-true)]
       [(<=) (b.le if-true)]
       [(>=) (b.ge if-true)]
       [(char=?) (b.eq if-true)])
     (emit "mov x0, #~a" (immediate-rep #f))
     (b end)
     (label if-true)
     (emit "mov x0, #~a" (immediate-rep #t))
     (label end)]
    [(add1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "add x0, x0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "sub x0, x0, #~a" (immediate-rep 1))]
    [(integer?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and x0, x0, #~a" fixnum-mask)
     (emit-is-x0-equal-to 0)]
    [(boolean?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and x0, x0, #~a" bool-mask)
     (emit-is-x0-equal-to bool-tag)]
    [(char?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "and x0, x0, #~a" char-mask)
     (emit-is-x0-equal-to char-tag)]
    [(zero?)
     (compile-expr (list-ref args 0) stack-index env)
     (emit-is-x0-equal-to 0)]))

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
      (emit "str x0, [x29, #~a]" offset))

    ; evaluate all body forms - this will leave the last one's output in %eax
    (for ([form body])
      (compile-expr form inner-si inner-env))))

(define (variable? x) (symbol? x))
(define (compile-var-load v stack-index env)
  (emit "ldr x0, [x29, #~a]" (cdr (assoc v env))))

(define (compile-if test t-body f-body stack-index env)
  (define-label if-true end)
  (compile-expr test stack-index env)
  (emit-is-x0-equal-to (immediate-rep #t))
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
    (emit-is-x0-equal-to (immediate-rep #t))
    (b.ne next)
    (label body-tag
           (for ([expr exprs])
             (compile-expr expr stack-index env))
           (b end))
    (label next))
  (label end))

(define (compile-expr e stack-index env)
  (match e
    [(or 'null '()) (emit "mov x0, #~a" (immediate-rep null))]
    [(? immediate? e) (emit "mov x0, #~a" (immediate-rep e))]
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

  (emit "mov x28, x0")
  (compile-expr program (- wordsize) '())
  (emit "ret"))

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (system "gcc /tmp/scheme.s c/runtime.c c/representation.c"))

(define (compile-and-eval program)
  (compile-to-binary program)
  (match-define (list stdout stdin status stderr do)
    (process "./a.out"))
  (do 'wait)
  (read stdout))

(define (compile-and-run program)
  (compile-to-binary program)
  (system "./a.out"))

(module+ test
  (require rackunit)

  ; arithmetic
  (check-equal? (compile-and-eval '1) 1)
  (check-equal? (compile-and-eval '(add1 1)) 2)
  (check-equal? (compile-and-eval '(+ 1 1)) 2)
  ; conditional
  (check-equal? (compile-and-eval '(if #f 1)) #f)
  (check-equal? (compile-and-eval '(if #t 1)) 1)
  (check-equal? (compile-and-eval '(if #t 1 2)) 1)
  (check-equal? (compile-and-eval '(if #f 1 2)) 2)
  (check-equal? (compile-and-eval '(cond
                                     [(= (- 2 1) 1) 1]
                                     [#t 2]))
                1)
  ; type check
  (check-equal? (compile-and-eval '(char=? #\c #\a)) #f)
  (check-equal? (compile-and-eval '(char=? #\b #\b)) #t)
  (check-equal? (compile-and-eval '(char? #\c)) #t)
  (check-equal? (compile-and-eval '(char? 1)) #f)
  (check-equal? (compile-and-eval '(boolean? #f)) #t)
  (check-equal? (compile-and-eval '(boolean? 1)) #f)
  (check-equal? (compile-and-eval '(integer? 1)) #t)
  (check-equal? (compile-and-eval '(integer? #f)) #f)
  ; let
  (check-equal? (compile-and-eval '(let ([x 1]) x)) 1)
  ; comparsion
  (check-equal? (compile-and-eval '(= 1 1)) #t)
  (check-equal? (compile-and-eval '(<= (sub1 10) (* 9 (/ 4 2)))) #t)
  (check-equal? (compile-and-eval '(> 2 1)) #t)
  (check-equal? (compile-and-eval '(>= 2 2)) #t)
  (check-equal? (compile-and-eval '(>= 3 2)) #t)
  (check-equal? (compile-and-eval '(>= 2 3)) #f)
  (check-equal? (compile-and-eval '(< 2 1)) #f)
  (check-equal? (compile-and-eval '(<= 2 1)) #f)
  (check-equal? (compile-and-eval '(<= 2 2)) #t)
  (check-equal? (compile-and-eval '(zero? 0)) #t)
  (check-equal? (compile-and-eval '(zero? #\c)) #f)
  ; list and pair
  (check-equal? (compile-and-eval 'null) '())
  (check-equal? (compile-and-eval '(cons #\c 1)) (cons #\c 1))
  )
