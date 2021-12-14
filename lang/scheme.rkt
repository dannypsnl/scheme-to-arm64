#lang nanopass

(provide E
         scm/Final)

(define (constant? x)
  (or (integer? x) (char? x) (boolean? x)))
(define-language scm
  (terminals (symbol [name])
             (constant [c]))
  (Expr [e body]
        c
        name
        (define name e)
        (begin e* ... e)
        (let ([name* e*] ...) body* ... body)
        (if e0 e1)
        (if e0 e1 e2)
        (cond [e body* ... body] ...)
        (e0 e1 ...)))

(define-language scm/L1
  (extends scm)
  (Expr [e body]
        (- (let ([name* e*] ...) body* ... body)
           (cond [e body* ... body] ...))
        (+ (let ([name* e*] ...) body)
           (cond [e body] ...))))
(define-pass wrap-begin : (scm Expr) (expr) -> (scm/L1 Expr) ()
  [Expr : Expr (expr) -> Expr ()
        [(let ([,name* ,e*] ...) ,[body*] ... ,[body])
         `(let ([,name* ,e*] ...) (begin ,body* ... ,body))]
        [(cond [,[e] ,[body*] ... ,[body]] ...)
         `(cond [,e (begin ,body* ... ,body)] ...)]])

(define-language scm/L2
  (extends scm/L1)
  (Expr [e body]
        (- (if e0 e1))))
(define-pass remove-if : (scm/L1 Expr) (e) -> (scm/L2 Expr) ()
  [Expr : Expr (e) -> Expr ()
        [(if ,e0 ,e1)
         `(if ,e0 ,e1 (void))]])

(define-language scm/Final (extends scm/L2))
(define-pass final : (scm/L2 Expr) (e) -> (scm/Final Expr) ()
  [Expr : Expr (e) -> Expr ()])

(define-parser parse-scm scm)
(define (E x)
  ((compose final
            remove-if
            wrap-begin)
   (parse-scm x)))