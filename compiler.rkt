#lang racket

(provide compile-program
         compile-to-binary)

(require "emit.rkt"
         "representation.rkt"
         "primitive.rkt")

(define wordsize 8)
(define (immediate? x) (or (integer? x) (char? x) (boolean? x) (null? x)))

(struct Env (m parent) #:transparent)
(define env (make-parameter (Env (make-hash) #f)))
(define (make-env m [p (env)])
  (Env m p))
(define (lookup name)
  (define current-env (env))
  (if (Env-parent current-env)
      (hash-ref (Env-m current-env) name
                (lambda ()
                  (parameterize ([env (Env-parent current-env)])
                    (lookup name))))
      (hash-ref (Env-m current-env) name)))
(define (var-set! name offset)
  (hash-set! (Env-m (env)) name offset))

(define (emit-is-x0-equal-to val)
  (define-label if-true end)
  (emit "cmp x0, #~a" val)
  (b.eq if-true)
  (emit "mov x0, #~a" (immediate-rep #f))
  (b end)
  (label if-true)
  (emit "mov x0, #~a" (immediate-rep #t))
  (label end))
(define (compile-primitive-call op args stack-index)
  (case op
    [(cons)
     ; store car/cdr to heap
     (compile-expr (car args) stack-index)
     (emit "str x0, [sp, #~a]" stack-index)
     (compile-expr (cadr args) (- stack-index wordsize))
     (emit "ldr x1, [sp, #~a]" stack-index)
     (emit "stp x1, x0, [x28]")
     ; save pointer and tag it
     (emit "orr x0, x28, #~a" pair-tag)
     ; we used two wordsize from heap
     (emit "add x28, x28, #~a" (* 2 wordsize))]
    [(make-string make-vector)
     (compile-expr (car args) stack-index)
     (emit "lsr x0, x0, #~a" fixnum-shift)
     ; store length into new structure
     (emit "str x0, [x28]")
     ; save pointer and tag it
     (emit "orr x1, x28, #~a" (case op [(make-string) str-tag] [(make-vector) vec-tag]))
     (emit "add x28, x28, #8")
     (when (> (length args) 1)
       (compile-expr (cadr args) (- stack-index wordsize))
       (case op [(make-string) (emit "lsr w0, w0, #~a" char-shift)])
       (define k (case op [(make-string) 1] [(make-vector) wordsize]))
       (for ([i (range (car args))])
         (emit "str x0, [x28, #~a]" (* k i)))
       (emit "add x28, x28, #~a" (* k (car args))))
     (emit "mov x0, x1")]
    [(string-ref vector-ref)
     (compile-expr (car args) stack-index)
     (emit "add x1, x0, #~a" (- wordsize (case op [(string-ref) str-tag] [(vector-ref) vec-tag])))
     (compile-expr (cadr args) (- stack-index wordsize))
     ; get index, so now index is in x0
     ; x1 is current pointer, x1 <- x1 + x0>>shift is offset of value
     (emit "add x1, x1, x0, lsr #~a" (case op [(string-ref) fixnum-shift] [(vector-ref) (+ fixnum-shift 2)]))
     (emit "ldr x0, [x1]")
     (case op ; now we convert loaded char back to encoded char
       [(string-ref) (emit "lsl x0, x0, #~a" char-shift)
                     (emit "orr x0, x0, #~a" char-tag)])]
    [(string-length vector-length)
     (compile-expr (car args) stack-index)
     (emit "sub x0, x0, #~a" (case op [(string-length) str-tag] [(vector-length) vec-tag]))
     (emit "ldr x0, [x0]")
     (emit "lsl x0, x0, #~a" fixnum-shift)]
    [(add1 sub1)
     (compile-expr (car args) stack-index)
     (case op
       [(add1) (emit "add x0, x0, #~a" (immediate-rep 1))]
       [(sub1) (emit "sub x0, x0, #~a" (immediate-rep 1))])]
    [(+ - * /)
     (compile-expr (car args) stack-index)
     (emit "str x0, [sp, #~a]" stack-index)
     (for ([v (cdr args)])
       (compile-expr v (- stack-index wordsize))
       (emit "ldr x1, [sp, #~a]" stack-index)
       (case op
         [(+) (emit "add x1, x1, x0")]
         [(-) (emit "sub x1, x1, x0")]
         [(*) (emit "lsr x0, x0, #~a" fixnum-shift)
              (emit "mul x1, x1, x0")]
         [(/) (emit "lsr x0, x0, #~a" fixnum-shift)
              (emit "sdiv x1, x1, x0")])
       (emit "str x1, [sp, #~a]" stack-index))
     (emit "ldr x0, [sp, #~a]" stack-index)]
    [(or)
     (define-label if-true end)
     (for ([v args])
       (compile-expr v stack-index)
       (emit-is-x0-equal-to (immediate-rep #t))
       (b.eq if-true))
     (emit "mov x0, #~a" (immediate-rep #f))
     (b end)
     (label if-true
            (emit "mov x0, #~a" (immediate-rep #t)))
     (label end)]
    [(and)
     (define-label if-true end)
     (for ([v args])
       (compile-expr v stack-index)
       (emit-is-x0-equal-to (immediate-rep #f))
       (b.eq if-true))
     (emit "mov x0, #~a" (immediate-rep #t))
     (b end)
     (label if-true
            (emit "mov x0, #~a" (immediate-rep #f)))
     (label end)]
    [(= < > <= >= char=?)
     (define-label end)
     (for ([left args]
           [right (cdr args)])
       (compile-expr left stack-index)
       (when (eq? op 'char=?)
         (emit "lsr x0, x0, #~a" char-shift))
       (emit "mov x8, x0")
       (compile-expr right (- stack-index wordsize))
       (when (eq? op 'char=?)
         (emit "lsr x0, x0, #~a" char-shift))
       (emit "cmp x8, x0")
       (define-label if-true)
       (case op
         [(=) (b.eq if-true)]
         [(<) (b.lt if-true)]
         [(>) (b.gt if-true)]
         [(<=) (b.le if-true)]
         [(>=) (b.ge if-true)]
         [(char=?) (b.eq if-true)])
       (emit "mov x0, #~a" (immediate-rep #f))
       (b end)
       (label if-true))
     (emit "mov x0, #~a" (immediate-rep #t))
     (label end)]
    [(integer? boolean? char? string? vector? zero?)
     (compile-expr (car args) stack-index)
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
     (compile-expr (car args) stack-index)
     (case op
       [(car) (emit "ldr x0, [x0, #~a]" (- pair-tag))]
       [(cdr) (emit "ldr x0, [x0, #~a]" (- wordsize pair-tag))]
       [(null?) (emit-is-x0-equal-to pair-tag)])]))

(define (compile-cond tests bodys stack-index)
  (define-label end)
  (for ([test tests]
        [exprs bodys])
    (define-label body-tag next)
    (compile-expr test stack-index)
    (emit-is-x0-equal-to (immediate-rep #t))
    (b.ne next)
    (label body-tag
           (for ([expr exprs])
             (compile-expr expr stack-index))
           (b end))
    (label next))
  (label end))

(define (compile-expr e stack-index)
  (match e
    [(or 'null '()) (emit "mov x0, #~a" (immediate-rep null))]
    ['(void) (emit "mov x0, #~a" (immediate-rep (void)))]
    [(? immediate? e) (emit "mov x0, #~a" (immediate-rep e))]
    [(? symbol? e) (emit "ldr x0, [sp, #~a]" (lookup e))]
    [(or (vector vs ...) `(vector ,vs ...))
     (emit "mov x0, #~a" (length vs))
     (emit "str x0, [x28]")
     (emit "orr x1, x28, #~a" vec-tag)
     (emit "add x28, x28, #~a" wordsize)
     (for ([v vs] [i (range (length vs))])
       (compile-expr v stack-index)
       (emit "str x0, [x28, #~a]" (* wordsize i)))
     (emit "add x28, x28, #~a" (* wordsize (length vs)))
     (emit "mov x0, x1")]
    [`(if ,test ,t-body) (compile-expr `(if ,test ,t-body #f) stack-index)]
    [`(if ,test ,t-body ,f-body)
     (define-label if-true end)
     (compile-expr test stack-index)
     (emit-is-x0-equal-to (immediate-rep #t))
     (b.eq if-true)
     (compile-expr f-body stack-index)
     (b end)
     (label if-true
            (compile-expr t-body stack-index))
     (label end)]
    [`(cond (,tests ,bodys ...) ...)
     (compile-cond tests bodys stack-index)]
    [`(define ,name ,expr)
     (compile-expr expr stack-index)
     ;;; FIXME: stack-index have to grow at here
     (define var-offset (- stack-index wordsize))
     (emit "str x0, [sp, #~a]" var-offset)
     (parameterize ([env (Env-parent (env))])
       (var-set! name var-offset))]
    [`(let ([,names ,exprs] ...) ,bodys ...)
     (let* ([stack-offsets
             (map (lambda (x) (- stack-index (* x wordsize)))
                  (range 0 (length names)))]
            [inner-si (- stack-index (* (length names) wordsize))])
       ; evaluate exprs and assign them to stack locations
       (for ([expr exprs]
             [offset stack-offsets])
         (compile-expr expr inner-si)
         (emit "str x0, [sp, #~a]" offset))
       (for ([form bodys])
         (parameterize ([env (make-env (make-hash (map cons names stack-offsets)))])
           (compile-expr form inner-si))))]
    [`(,op ,args ...)
     #:when (member op primitive-functions)
     (compile-primitive-call op args stack-index)]
    [(or `(list ,lst ...)
         `(quote ,lst ...))
     (compile-expr (foldr (λ (v r) (list 'cons v r)) '() lst) stack-index)]
    ; note: keep for function call
    [else (error "unkown expression ~a" e)]))

(define (compile-program program)
  (emit ".section __TEXT,__text,regular,pure_instructions")
  (emit ".p2align 2")
  (emit ".globl _scheme_entry")
  (emit "_scheme_entry:")

  (emit "mov x28, x0")
  (compile-expr program (- wordsize))
  (emit "ret"))

(define (compile-to-binary program [debug? #f])
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (define cmd "clang -target arm64-apple-darwin-macho /tmp/scheme.s c/runtime.c c/representation.c")
  (system (if debug? (string-append cmd " -g") cmd)))

(define (compile-and-eval program)
  (compile-to-binary program)
  (match-define (list stdout stdin status stderr do)
    (process "./a.out"))
  (do 'wait)
  (read stdout))

(module+ test
  (require rackunit)

  ; arithmetic
  (check-equal? (compile-and-eval '1) 1)
  (check-equal? (compile-and-eval '(add1 1)) 2)
  (check-equal? (compile-and-eval '(+ 1 1)) 2)
  (check-equal? (compile-and-eval '(+ 1 2 3)) 6)
  (check-equal? (compile-and-eval '(- 1 2 3)) -4)
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
  ; let and define
  (check-equal? (compile-and-eval '(let ([x 1]) x)) 1)
  (check-equal? (compile-and-eval '(let ([x 1])
                                     (let ([y (* 2 x)])
                                       (cons x y))))
                '(1 . 2))
  (check-equal? (compile-and-eval '(let ()
                                     (define x 1)
                                     (define y 2)
                                     (cons x y)))
                '(1 . 2))
  ; logical
  (check-equal? (compile-and-eval '(and #t #t)) #t)
  (check-equal? (compile-and-eval '(and #f #t)) #f)
  (check-equal? (compile-and-eval '(and #t #f)) #f)
  (check-equal? (compile-and-eval '(and #t #t #t)) #t)
  (check-equal? (compile-and-eval '(and #t #f #t)) #f)
  (check-equal? (compile-and-eval '(or #t #f)) #t)
  (check-equal? (compile-and-eval '(or #t #f #t)) #t)
  (check-equal? (compile-and-eval '(or #f #t)) #t)
  (check-equal? (compile-and-eval '(or #f #f)) #f)
  ; comparsion
  (check-equal? (compile-and-eval '(= 1 1)) #t)
  (check-equal? (compile-and-eval '(<= (sub1 10) (* 9 (/ 4 2)))) #t)
  (check-equal? (compile-and-eval '(> 2 1)) #t)
  (check-equal? (compile-and-eval '(>= 2 2)) #t)
  (check-equal? (compile-and-eval '(>= 3 2)) #t)
  (check-equal? (compile-and-eval '(>= 2 3)) #f)
  (check-equal? (compile-and-eval '(< 1 2 3 4 5 6 7)) #t)
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
  (check-equal? (compile-and-eval '(cons 1 (cons 2 (cons 3 4)))) '(1 2 3 . 4))
  (check-equal? (compile-and-eval '(car (cons 1 2))) 1)
  (check-equal? (compile-and-eval '(cdr (cons 1 2))) 2)
  (check-equal? (compile-and-eval '(quote 1 2 3)) '(1 2 3))
  (check-equal? (compile-and-eval '(list 1 2 3)) '(1 2 3))
  (check-equal? (compile-and-eval '(list 1 (list 1 2 3) 3)) '(1 (1 2 3) 3))
  ; string
  (check-equal? (compile-and-eval '(make-string 5 #\c)) "ccccc")
  (check-equal? (compile-and-eval '(string-ref (make-string 2 #\q) 1)) #\q)
  (check-equal? (compile-and-eval '(string? (make-string 2 #\a))) #t)
  (check-equal? (compile-and-eval '(string-length (make-string 2 #\b))) 2)
  ; vector
  (check-equal? (compile-and-eval '#(1 2 3)) #(1 2 3))
  (check-equal? (compile-and-eval '(vector 1 2 3)) #(1 2 3))
  (check-equal? (compile-and-eval '(vector)) #())
  (check-equal? (compile-and-eval '(make-vector 2 #\c)) #(#\c #\c))
  (check-equal? (compile-and-eval '(vector-ref (make-vector 2 2) 0)) 2)
  (check-equal? (compile-and-eval '(vector? (make-vector 2 2))) #t)
  (check-equal? (compile-and-eval '(vector-length (make-vector 3 #\b))) 3)
  )
