#lang nanopass

(provide compile-program
         compile-to-binary)

(require "env.rkt"
         "representation.rkt"
         "runtime.rkt"
         "lang/scheme.rkt"
         "lang/arm64.rkt"
         syntax/parse/define)

(define-syntax-parser define-label
  [(_ label-names ...)
   #'(begin
       (define label-names (gensym 'LLB)) ...)])

(define wordsize 8)
(define functions '())

(define-pass compile-scm : (scm/Final Expr) (e si) -> (arm64 Instruction) ()
  (definitions
    (define stack-index si)
    (define (Expr-on-offset e offset)
      (set! stack-index (- stack-index offset))
      (define r (Expr e))
      (set! stack-index (+ stack-index offset))
      r))
  (emit-is-x0-equal-to : Expr (e) -> Instruction ()
                       [,c (define-label if-true end)
                           (list
                            `(cmp x0 ,c)
                            `(b.eq ,if-true)
                            `(mov x0 ,(immediate-rep #f))
                            `(b ,end)
                            `(label ,if-true)
                            `(mov x0 ,(immediate-rep #t))
                            `(label ,end))])
  (Expr : Expr (e) -> Instruction ()
        [,name (guard (eq? name 'null))
               `(mov x0 ,(immediate-rep null))]
        [,name `(ldr x0 [sp ,(lookup name)])]
        [,c `(mov x0 ,(immediate-rep c))]
        [,s (list `(mov x0 ,(+ wordsize (string-length s)))
                  `(call _GC_malloc)
                  `(mov x27 x0)
                  `(mov x0 ,(string-length s))
                  `(str x0 [x27 0])
                  `(orr x1 x27 ,str-tag)
                  `(add x27 x27 8)
                  (for/list ([c (string->list s)]
                             [i (range (string-length s))])
                    (list `(mov w0 ,(char->integer c))
                          `(str w0 [x27 ,i])))
                  `(mov x0 x1))]
        [,v (match-define (vector vs ...) v)
            (list `(mov x0 ,(* (add1 (length vs)) wordsize))
                  `(call _GC_malloc)
                  `(mov x27 x0)
                  `(mov x0 ,(length vs))
                  `(str x0 [x27 0])
                  `(orr x1 x27 ,vec-tag)
                  (for/list ([v vs]
                             [i (range (length vs))])
                    (list (Expr v)
                          `(str x0 [x27 ,(* (add1 i) wordsize)])))
                  `(mov x0 x1))]
        [(define ,name ,e)
         (define var-offset stack-index)
         (var-set! name var-offset)
         (define r (Expr e))
         (set! stack-index (- stack-index wordsize))
         (list r `(str x0 [sp ,var-offset]))]
        [(begin ,e* ... ,e)
         (parameterize ([env (make-env (make-hash))])
           (for/list ([e (append e* (list e))])
             (Expr e)))]
        [(cond [,e ,body] ...)
         (define-label end)
         (append
          (for/list ([e e]
                     [body body])
            (define-label body-tag next)
            (list
             (Expr e)
             (emit-is-x0-equal-to (immediate-rep #t))
             `(b.ne ,next)
             `(label ,body-tag)
             (Expr body)
             `(b ,end)
             `(label ,next)))
          (list `(label ,end)))]
        [(if ,e0 ,e1 ,e2)
         (define-label if-true end)
         (list (Expr e0)
               (emit-is-x0-equal-to (immediate-rep #t))
               `(b.eq ,if-true)
               (Expr e2)
               `(b ,end)
               `(label ,if-true)
               (Expr e1)
               `(label ,end))]
        [(make-closure (lifted-lambda ,name (,name* ...) ,body) ,e1)
         #| first we generate lifted lambda definition block
            notice that we cannot append this block directly on to _scheme_entry!
         |#
         (define lifted-lambda-name name)
         (set! functions
               (cons
                (list `(comment ,(format "~a" (unparse-scm/Final e)))
                      `(global-label ,lifted-lambda-name)
                      (parameterize ([env (make-env (make-hash))])
                        (append
                         (for/list ([name name*])
                           (define var-offset stack-index)
                           (var-set! name var-offset)
                           (set! stack-index (- stack-index wordsize))
                           `(str x0 [sp ,var-offset]))
                         (list (Expr body))))
                      `(ret))
                functions))
         #| here we make a closure,
            1. `e1` must be a env(encoded as vector), thus, we can directly compile it
            2. then we should get pointer to the block we generated for lifted-lambda
            3. call `__scheme_make_closure` function with pointer and env
         |#
         (list (Expr e1)
               `(mov x1 x0)
               `(ldr-fn-ptr x0 ,lifted-lambda-name)
               `(call __scheme_make_closure))]
        [(prim ,op ,e1 ...)
         (case op
           [(vector) (define vs e1)
                     (list `(mov x0 ,(* (add1 (length vs)) wordsize))
                           `(call _GC_malloc)
                           `(mov x27 x0)
                           `(mov x0 ,(length vs))
                           `(str x0 [x27 0])
                           `(orr x1 x27 ,vec-tag)
                           (for/list ([v vs]
                                      [i (range (length vs))])
                             (list (Expr v)
                                   `(str x0 [x27 ,(* (add1 i) wordsize)])))
                           `(mov x0 x1))]
           [(cons) (match-define (list e-car e-cdr) e1)
                   (list (Expr-on-offset e-cdr wordsize)
                         `(mov x1 x0)
                         (Expr e-car)
                         `(call __scheme_cons))]
           [(add1 sub1) (list (Expr (car e1))
                              (case op
                                [(add1) `(add x0 x0 ,(immediate-rep 1))]
                                [(sub1) `(sub x0 x0 ,(immediate-rep 1))]))]
           [(+ - * /) (list
                       (Expr (car e1))
                       `(str x0 [sp ,stack-index])
                       (for/list ([v (cdr e1)])
                         (set! stack-index (- stack-index wordsize))
                         (define e (Expr v))
                         (set! stack-index (+ stack-index wordsize))
                         (list e
                               `(ldr x1 [sp ,stack-index])
                               (case op
                                 [(+) `(add x1 x1 x0)]
                                 [(-) `(sub x1 x1 x0)]
                                 [(*) (list `(lsr x0 x0 ,fixnum-shift)
                                            `(mul x1 x1 x0))]
                                 [(/) (list `(lsr x0 x0 ,fixnum-shift)
                                            `(sdiv x1 x1 x0))])
                               `(str x1 [sp ,stack-index])))
                       `(ldr x0 [sp ,stack-index]))]
           [(= < > <= >= char=?)
            (define-label end)
            (append
             (for/list ([left e1]
                        [right (cdr e1)])
               (define-label if-true)
               (list
                (Expr left)
                (if (eq? op 'char=?) `(lsr x0 x0 ,char-shift) '())
                `(mov x8 x0)
                (Expr right)
                (if (eq? op 'char=?) `(lsr x0 x0 ,char-shift) '())
                `(cmp x8 x0)
                (case op
                  [(=) `(b.eq ,if-true)]
                  [(<) `(b.lt ,if-true)]
                  [(>) `(b.gt ,if-true)]
                  [(<=) `(b.le ,if-true)]
                  [(>=) `(b.ge ,if-true)]
                  [(char=?) `(b.eq ,if-true)])
                `(mov x0 ,(immediate-rep #f))
                `(b ,end)
                `(label ,if-true)))
             (list `(mov x0 ,(immediate-rep #t))
                   `(label ,end)))]
           [(integer? boolean? char? string? vector? zero? null? car cdr)
            (list
             (Expr (car e1))
             (case op
               [(integer?) (list `(and x0 x0 ,fixnum-mask)
                                 (emit-is-x0-equal-to 0))]
               [(boolean?) (list `(and x0 x0 ,bool-mask)
                                 (emit-is-x0-equal-to bool-tag))]
               [(char?) (list `(and x0 x0 ,char-mask)
                              (emit-is-x0-equal-to char-tag))]
               [(string?) (list `(and x0 x0 ,ptr-mask)
                                (emit-is-x0-equal-to str-tag))]
               [(vector?) (list `(and x0 x0 ,ptr-mask)
                                (emit-is-x0-equal-to vec-tag))]
               [(zero?) (emit-is-x0-equal-to 0)]
               [(car) `(ldr x0 [x0 ,(- pair-tag)])]
               [(cdr) `(ldr x0 [x0 ,(- wordsize pair-tag)])]
               [(null?) (emit-is-x0-equal-to pair-tag)]))]
           [(void) `(mov x0 ,(immediate-rep (void)))]
           [(and or)
            (define-label if-true end)
            (list
             (for/list ([v e1])
               (list (Expr v)
                     (case op
                       [(and) (emit-is-x0-equal-to (immediate-rep #f))]
                       [(or) (emit-is-x0-equal-to (immediate-rep #t))])
                     `(b.eq ,if-true)))
             (case op
               [(and) `(mov x0 ,(immediate-rep #t))]
               [(or) `(mov x0 ,(immediate-rep #f))])
             `(b ,end)
             `(label ,if-true)
             (case op
               [(and) `(mov x0 ,(immediate-rep #f))]
               [(or) `(mov x0 ,(immediate-rep #t))])
             `(label ,end))]
           [(make-string make-vector)
            (append
             (match e1
               [`(,len) (list `(mov x1 0)
                              (Expr len))]
               [`(,len ,fill-by) (list (Expr fill-by)
                                       `(mov x1 x0)
                                       (Expr len))])
             (list `(call ,(case op [(make-string) '__scheme_make_string] [(make-vector) '__scheme_make_vector]))))]
           [(string-ref vector-ref)
            (set! stack-index (- stack-index wordsize))
            (define e (Expr (cadr e1)))
            (set! stack-index (+ stack-index wordsize))
            (list
             (Expr (car e1))
             `(add x1 x0 ,(- wordsize (case op [(string-ref) str-tag] [(vector-ref) vec-tag])))
             e
             ; get index, so now index is in x0
             ; x1 is current pointer, x1 <- x1 + x0>>shift is offset of value
             `(add x1 x1 x0 lsr ,(case op [(string-ref) fixnum-shift] [(vector-ref) (+ fixnum-shift 2)]))
             `(ldr x0 [x1 0])
             (case op ; now we convert loaded char back to encoded char
               [(string-ref) (list `(lsl x0 x0 ,char-shift)
                                   `(orr x0 x0 ,char-tag))]
               [else '()]))]
           [(string-length vector-length)
            (list (Expr (car e1))
                  `(sub x0 x0 ,(case op [(string-length) str-tag] [(vector-length) vec-tag]))
                  `(ldr x0 [x0 0])
                  `(lsl x0 x0 ,fixnum-shift))])]
        [(,e0 ,e1 ...)
         `(comment "todo function call")
         (list (Expr e0) ; compile a function
               ; now assume we get a closure
               `(sub x0 x0 ,closure-tag)
               ; FIXME: decode x0 first
               `(ldr x9 [x0 ,0]) ; function pointer
               `(ldr x10 [x0 ,8]) ; env
               (for/list ([arg e1])
                 (Expr arg))
               `(closure-call x9))
         ])
  (Expr e))

(define (compile-expr scm-exp stack-index)
  (flatten-arm64 (compile-scm scm-exp stack-index)))

(define (compile-program e)
  (emit-program (compile-expr (E e) (- wordsize))
                (flatten-arm64 functions)))

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program))))

(define (compile-and-eval program)
  (compile-to-binary program)
  (println runtime)
  (parameterize ([current-directory runtime])
    (match-define (list stdout stdin status stderr do)
      (process "zig build run"))
    (do 'wait)
    (read stderr)))

(module+ test
  (require rackunit)

  ; arithmetic
  (check-equal? (compile-and-eval '1) 1)
  (check-equal? (compile-and-eval '(add1 1)) 2)
  (check-equal? (compile-and-eval '(+ 1 1)) 2)
  (check-equal? (compile-and-eval '(* 1 2)) 2)
  (check-equal? (compile-and-eval '(+ 1 2 3)) 6)
  (check-equal? (compile-and-eval '(- 1 2 3)) -4)
  ; conditional
  ; (check-equal? (compile-and-eval '(if #f 1)) eof) ; since we will get a #<void> which prints nothing
  (check-equal? (compile-and-eval '(if #t 1)) 1)
  (check-equal? (compile-and-eval '(if #t 1 2)) 1)
  (check-equal? (compile-and-eval '(if #f 1 2)) 2)
  (check-equal? (compile-and-eval '(cond
                                     [(= (- 2 1) 1) 1]
                                     [#t 2]))
                1)
  (check-equal? (compile-and-eval '(cond [#t (define x 2)
                                             x]))
                2)
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
  (check-equal? (compile-and-eval '(cons #\c 1)) (cons #\c 1))
  (check-equal? (compile-and-eval '(cons 1 (cons 2 (cons 3 4)))) '(1 2 3 . 4))
  (check-equal? (compile-and-eval '(cons (cons 2 3) 1)) '((2 . 3) . 1))
  (check-equal? (compile-and-eval '(car (cons 1 2))) 1)
  (check-equal? (compile-and-eval '(cdr (cons 1 2))) 2)
  (check-equal? (compile-and-eval '(quote 1 2 3)) '(1 2 3))
  (check-equal? (compile-and-eval '(list 1 2 3)) '(1 2 3))
  ; (check-equal? (compile-and-eval '(list 1 (list 1 2 3) 3)) '(1 (1 2 3) 3))
  ; string
  (check-equal? (compile-and-eval '"abcde") "abcde")
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
