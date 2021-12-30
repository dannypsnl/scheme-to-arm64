#lang nanopass

(provide E
         scm/Final)

(define (constant? x)
  (or (integer? x) (char? x) (boolean? x)))
(define-language scm
  (terminals (symbol [name])
             (constant [c])
             (vector [v])
             (string [s]))
  (Expr [e body]
        c
        v
        s
        name
        (define name e)
        (begin e* ... e)
        (lambda (name* ...) body* ... body)
        (let ([name* e*] ...) body* ... body)
        (if e0 e1)
        (if e0 e1 e2)
        (cond [e body* ... body] ...)
        (e0 e1 ...)))

(define-language scm/L1
  (extends scm)
  (Expr [e body]
        (- (lambda (name* ...) body* ... body)
           (let ([name* e*] ...) body* ... body)
           (cond [e body* ... body] ...))
        (+ (lambda (name* ...) body)
           (cond [e body] ...))))
(define-pass wrap-begin : (scm Expr) (expr) -> (scm/L1 Expr) ()
  [Expr : Expr (expr) -> Expr ()
        [(lambda (,name* ...) ,[body*] ... ,[body])
         `(lambda (,name* ...) (begin ,body* ... ,body))]
        [(let ([,name* ,[e*]] ...) ,[body*] ... ,[body])
         `(begin (define ,name* ,e*) ...
                 ,body* ... ,body)]
        [(cond [,[e] ,[body*] ... ,[body]] ...)
         `(cond [,e (begin ,body* ... ,body)] ...)]])

(define-language scm/L2
  (extends scm/L1)
  (Expr [e body]
        (- (if e0 e1))))
(define-pass remove-if : (scm/L1 Expr) (e) -> (scm/L2 Expr) ()
  [Expr : Expr (e) -> Expr ()
        [(if ,[e0] ,[e1])
         `(if ,e0 ,e1 (void))]])

(define-language scm/L3 (extends scm/L2))
(define-pass normalize-data : (scm/L2 Expr) (e) -> (scm/L3 Expr) ()
  [Expr : Expr (e) -> Expr ()
        [(,[e0] ,[e1] ...)
         (case e0
           [(list quote) (foldr (λ (v r) `(cons ,v ,r)) `null e1)]
           [(vector) `,(apply vector e1)]
           [else `(,e0 ,e1 ...)])]])

(define primitive-functions
  '(+
    -
    *
    /
    add1 sub1
    zero?
    ; list and pair
    car cdr cons
    null?
    ; logical
    and or
    ; comparison operators
    = < > <= >= char=?
    ; type check
    integer?
    boolean?
    char?
    ; string
    make-string string-ref string? string-length
    ; vector
    vector make-vector vector-ref vector? vector-length))
(define (primitive? x) (member x primitive-functions))
(define-language scm/L4
  (extends scm/L3)
  (terminals (+ (primitive [op])))
  (Expr [e body]
        (+ (prim op e1 ...))))
(define-pass explicit-prim-call : (scm/L3 Expr) (e) -> (scm/L4 Expr) ()
  [Expr : Expr (e) -> Expr ()
        [(,[e0] ,[e1] ...)
         (cond
           [(member e0 primitive-functions)
            `(prim ,e0 ,e1 ...)]
           [else `(,e0 ,e1 ...)])]])

(define-language scm/L5
  (extends scm/L4)
  (Expr [e body]
        (- (lambda (name* ...) body))
        (+ (lifted-lambda (name* ...) body)
           ; make-closure stores function and environment
           (make-closure e0 e1)
           (make-env name ...))))
(define-pass freevars : (scm/L4 Expr) (e) -> * ()
  (Expr : Expr (e) -> * ()
        [,name (set name)]
        [(lambda (,name* ...) ,body)
         (set-subtract (freevars body)
                       (list->set name*))]
        [(define ,name ,e)
         (freevars e)]
        [(begin ,e* ... ,e)
         (apply set-union (map freevars (append e* (list e))))]
        [(if ,e0 ,e1 ,e2)
         (set-union  (freevars e0)
                     (freevars e1)
                     (freevars e2))]
        [(cond [,e ,body] ...)
         (apply set-union
                (append (map freevars e)
                        (map freevars body)))]
        [(prim ,op ,e* ...)
         (apply set-union (map freevars e*))]
        [(,e0 ,e1 ...)
         (apply set-union (map freevars (cons e0 e1)))]
        [else (set)]))
(define-pass replace-free : (scm/L5 Expr) (e $env fvs) -> (scm/L5 Expr) ()
  (Expr : Expr (e) -> Expr ()
        [,name (guard (set-member? fvs name))
               `(prim vector-ref $env ,(index-of (set->list fvs) name))]))
(define-pass closure-conversion : (scm/L4 Expr) (e) -> (scm/L5 Expr) ()
  (Expr : Expr (e) -> Expr ()
        [(lambda (,name* ...) ,[body])
         (define $env (gensym 'env))
         (define fvs (freevars e))
         ; convert free-vars in body by using reference to $env
         (if (set-empty? fvs)
             `(lifted-lambda (,name* ...) ,body)
             `(make-closure (lifted-lambda (,name* ... ,$env) ,(replace-free body $env fvs))
                            (prim vector ,(set->list fvs) ...)))]))

(define-language scm/Final (extends scm/L5))
(define-pass final : (scm/L5 Expr) (e) -> (scm/Final Expr) ()
  [Expr : Expr (e) -> Expr ()])

(define-parser parse-scm scm)
(define (E x)
  (foldl (lambda (f e)
           (f e))
         (parse-scm x)
         (list wrap-begin
               remove-if
               normalize-data
               explicit-prim-call
               closure-conversion
               final)))

(module+ test
  (require rackunit)

  (define-parser pL4 scm/L4)
  (check-equal? (freevars (pL4 '(lambda (x) (if x y z))))
                (set 'y 'z))
  (check-equal? (freevars (pL4 '(lambda (x) (prim cons x y))))
                (set 'y))
  (check-equal? (freevars (pL4 '(lambda (x) #(x y))))
                (set 'y))
  )
