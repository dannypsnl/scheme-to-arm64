#lang racket

(provide compile-program
         compile-to-binary
         compile-and-run)

(require "emit.rkt"
         "representation.rkt"
         "primitive.rkt")

(define wordsize 8)
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
     ; save pointer and tag it
     (emit "mov x0, x28")
     (emit "orr x0, x0, #~a" pair-tag)
     ; we used two wordsize from heap
     (emit "add x28, x28, #~a" (* 2 wordsize))]
    [(make-string make-vector)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "lsr x0, x0, #~a" fixnum-shift)
     ; store length into new structure
     (emit "str x0, [x28]")
     ; save pointer and tag it
     (emit "orr x1, x28, #~a" (case op [(make-string) str-tag] [(make-vector) vec-tag]))
     (emit "add x28, x28, #8")
     (when (> (length args) 1)
       (compile-expr (list-ref args 1) (- stack-index wordsize) env)
       (case op [(make-string) (emit "lsr w0, w0, #~a" char-shift)])
       (define k (case op [(make-string) 1] [(make-vector) wordsize]))
       (for ([i (range (list-ref args 0))])
         (emit "str x0, [x28, #~a]" (* k i)))
       (emit "add x28, x28, #~a" (* k (list-ref args 0))))
     (emit "mov x0, x1")]
    [(string-ref vector-ref)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "add x1, x0, #~a" (- wordsize (case op [(string-ref) str-tag] [(vector-ref) vec-tag])))
     (compile-expr (list-ref args 1) (- stack-index wordsize) env)
     ; get index, so now index is in x0
     ; x1 is current pointer, x1 <- x1 + x0>>shift is offset of value
     (emit "add x1, x1, x0, lsr #~a" (case op [(string-ref) fixnum-shift] [(vector-ref) (+ fixnum-shift 2)]))
     (emit "ldr x0, [x1]")
     (case op ; now we convert loaded char back to encoded char
       [(string-ref) (emit "lsl x0, x0, #~a" char-shift)
                     (emit "orr x0, x0, #~a" char-tag)])]
    [(string-length vector-length)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "sub x0, x0, #~a" (case op [(string-length) str-tag] [(vector-length) vec-tag]))
     (emit "ldr x0, [x0]")
     (emit "lsl x0, x0, #~a" fixnum-shift)]
    [(add1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "add x0, x0, #~a" (immediate-rep 1))]
    [(sub1)
     (compile-expr (list-ref args 0) stack-index env)
     (emit "sub x0, x0, #~a" (immediate-rep 1))]
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
    [(integer? boolean? char? string? vector? zero?)
     (compile-expr (list-ref args 0) stack-index env)
     (case op
       [(integer?) (emit "and x0, x0, #~a" fixnum-mask)
                   (emit-is-x0-equal-to 0)]
       [(boolean?) (emit "and x0, x0, #~a" bool-mask)
                   (emit-is-x0-equal-to bool-tag)]
       [(char?) (emit "and x0, x0, #~a" char-mask)
                (emit-is-x0-equal-to char-tag)]
       [(string?) (emit "and x0, x0, #~a" ptr-mask)
                  (emit-is-x0-equal-to str-tag)]
       [(vector?) (emit "and x0, x0, #~a" ptr-mask)
                  (emit-is-x0-equal-to vec-tag)]
       [(zero?) (emit-is-x0-equal-to 0)])]
    [(null? car cdr)
     (compile-expr (list-ref args 0) stack-index env)
     (case op
       [(car) (emit "ldr x0, [x0, #~a]" (- pair-tag))]
       [(cdr) (emit "ldr x0, [x0, #~a]" (- wordsize pair-tag))]
       [(null?) (emit-is-x0-equal-to pair-tag)])]))

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

    ; evaluate all body forms - this will leave the last one's output in x0
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
  (system "clang -target arm64-apple-darwin-macho /tmp/scheme.s c/runtime.c c/representation.c"))

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
  (check-equal? (compile-and-eval '(null? null)) #t)
  (check-equal? (compile-and-eval '(null? ())) #t)
  (check-equal? (compile-and-eval '(cons #\c 1)) (cons #\c 1))
  (check-equal? (compile-and-eval '(car (cons 1 2))) 1)
  (check-equal? (compile-and-eval '(cdr (cons 1 2))) 2)
  ; string
  (check-equal? (compile-and-eval '(make-string 5 #\c)) "ccccc")
  (check-equal? (compile-and-eval '(string-ref (make-string 2 #\q) 1)) #\q)
  (check-equal? (compile-and-eval '(string? (make-string 2 #\a))) #t)
  (check-equal? (compile-and-eval '(string-length (make-string 2 #\b))) 2)
  ; vector
  (check-equal? (compile-and-eval '(make-vector 2 #\c)) #(#\c #\c))
  (check-equal? (compile-and-eval '(vector-ref (make-vector 2 2) 0)) 2)
  (check-equal? (compile-and-eval '(vector? (make-vector 2 2))) #t)
  (check-equal? (compile-and-eval '(vector-length (make-vector 3 #\b))) 3)
  )
