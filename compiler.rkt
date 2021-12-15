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
  ; list and pair
  (check-equal? (compile-and-eval '(null? ())) #t)
  )
