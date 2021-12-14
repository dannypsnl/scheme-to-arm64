#lang nanopass

(require "env.rkt"
         "representation.rkt"
         "lang/scheme.rkt"
         "lang/arm64.rkt"
         syntax/parse/define)

(define-syntax-parser define-label
  [(_ label-names ...)
   #'(begin
       (define label-names (gensym 'LLB)) ...)])

(define wordsize 8)

(define-pass compile-scm : (scm/Final Expr) (e si) -> (arm64 Instruction) ()
  (definitions (define stack-index si))
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
        [,name (case name
                 [(null) `(mov x0 ,(immediate-rep null))]
                 [else `(ldr x0 [sp ,(lookup name)])])]
        [,c `(mov x0 ,(immediate-rep c))]
        [(define ,name ,e)
         (define ret
           (list (Expr e)
                 `(str x0 [sp ,stack-index])))
         (var-set! name stack-index)
         (set! stack-index (- stack-index wordsize))
         ret]
        [(begin ,e* ... ,e)
         (parameterize ([env (make-env (make-hash))])
           (for/list ([e (append e* (list e))])
             (Expr e)))]
        [(let ([,name* ,e*] ...) ,body)
         `(comment "ignore")]
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
        [(,e0 ,e1 ...)
         (define op e0)
         (case op
           [(cons) (set! stack-index (- stack-index wordsize))
                   (define e (Expr (cadr e1)))
                   (set! stack-index (+ stack-index wordsize))
                   (list
                    ; store car/cdr to heap
                    (Expr (car e1))
                    `(str x0 [sp ,stack-index])
                    e
                    `(ldr x1 [sp ,stack-index])
                    `(stp x1 x0 [x28])
                    ; save pointer and tag it
                    `(orr x0 x28 ,pair-tag)
                    ; we used two wordsize from heap
                    `(add x28 x28 ,(* 2 wordsize)))]
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
                                 [(*) `(lsr x0 x0 ,fixnum-shift)
                                      `(mul x1 x1 x0)]
                                 [(/) `(lsr x0 x0 ,fixnum-shift)
                                      `(sdiv x1 x1 x0)])
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
           [else `(comment "todo function call")])])
  (Expr e))
(define-pass convert : (arm64 Instruction) (i) -> (arm64 Program) ()
  (if (list? i)
      `(,(flatten i) ...)
      `(,i)))
(define (compile-expr scm-exp stack-index)
  (convert (compile-scm scm-exp stack-index)))

(define (compile-program e)
  (emit-program (compile-expr (E e) (- wordsize))))

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
  (check-equal? (compile-and-eval '(if #f 1)) eof) ; since we will get a #<void> which prints nothing
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
  ; (check-equal? (compile-and-eval '(let ([x 1]) x)) 1)
  ; (check-equal? (compile-and-eval '(let ([x 1])
  ;                                    (let ([y (* 2 x)])
  ;                                      (cons x y))))
  ;               '(1 . 2))
  ; (check-equal? (compile-and-eval '(let ()
  ;                                    (define x 1)
  ;                                    (define y 2)
  ;                                    (cons x y)))
  ;               '(1 . 2))
  ; logical
  ; (check-equal? (compile-and-eval '(and #t #t)) #t)
  ; (check-equal? (compile-and-eval '(and #f #t)) #f)
  ; (check-equal? (compile-and-eval '(and #t #f)) #f)
  ; (check-equal? (compile-and-eval '(and #t #t #t)) #t)
  ; (check-equal? (compile-and-eval '(and #t #f #t)) #f)
  ; (check-equal? (compile-and-eval '(or #t #f)) #t)
  ; (check-equal? (compile-and-eval '(or #t #f #t)) #t)
  ; (check-equal? (compile-and-eval '(or #f #t)) #t)
  ; (check-equal? (compile-and-eval '(or #f #f)) #f)
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
  ; (check-equal? (compile-and-eval '(null? ())) #t)
  (check-equal? (compile-and-eval '(cons #\c 1)) (cons #\c 1))
  (check-equal? (compile-and-eval '(cons 1 (cons 2 (cons 3 4)))) '(1 2 3 . 4))
  (check-equal? (compile-and-eval '(car (cons 1 2))) 1)
  (check-equal? (compile-and-eval '(cdr (cons 1 2))) 2)
  ; (check-equal? (compile-and-eval '(quote 1 2 3)) '(1 2 3))
  ; (check-equal? (compile-and-eval '(list 1 2 3)) '(1 2 3))
  ; (check-equal? (compile-and-eval '(list 1 (list 1 2 3) 3)) '(1 (1 2 3) 3))
  ; string
  ; (check-equal? (compile-and-eval '(make-string 5 #\c)) "ccccc")
  ; (check-equal? (compile-and-eval '(string-ref (make-string 2 #\q) 1)) #\q)
  ; (check-equal? (compile-and-eval '(string? (make-string 2 #\a))) #t)
  ; (check-equal? (compile-and-eval '(string-length (make-string 2 #\b))) 2)
  ; vector
  ; (check-equal? (compile-and-eval '#(1 2 3)) #(1 2 3))
  ; (check-equal? (compile-and-eval '(vector 1 2 3)) #(1 2 3))
  ; (check-equal? (compile-and-eval '(vector)) #())
  ; (check-equal? (compile-and-eval '(make-vector 2 #\c)) #(#\c #\c))
  ; (check-equal? (compile-and-eval '(vector-ref (make-vector 2 2) 0)) 2)
  ; (check-equal? (compile-and-eval '(vector? (make-vector 2 2))) #t)
  ; (check-equal? (compile-and-eval '(vector-length (make-vector 3 #\b))) 3)
  )
