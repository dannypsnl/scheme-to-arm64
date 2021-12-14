#lang racket

(provide compile-program
         compile-to-binary)

(require "env.rkt"
         "emit.rkt"
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
(define (compile-primitive-call op args stack-index)
  (case op
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
    ))

(define (compile-expr e stack-index)
  (match e
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
    [`(let ([,names ,exprs] ...) ,bodys ...)
     (let ([stack-offsets
            (map (lambda (x) (- stack-index (* x wordsize)))
                 (range 0 (length names)))]
           [inner-si (- stack-index (* (length names) wordsize))])
       ; evaluate exprs and assign them to stack locations
       (for ([expr exprs]
             [offset stack-offsets])
         (compile-expr expr inner-si)
         (emit "str x0, [sp, #~a]" offset))
       (parameterize ([env (make-env (make-hash (map cons names stack-offsets)))])
         (for ([body bodys])
           (define maybe-offset (compile-expr body inner-si))
           (when (number? maybe-offset)
             (set! inner-si maybe-offset)))))]
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
  ; list and pair
  (check-equal? (compile-and-eval '(null? ())) #t)
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
