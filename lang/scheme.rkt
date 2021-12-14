#lang nanopass

(provide E
         scm/Final)

(define (constant? x)
  (or (integer? x) (char? x) (boolean? x)))
(define-language scm
  (terminals (symbol [name])
             (constant [c]))
  (Expr (e body)
        c
        name
        (define name e)
        (begin e* ... e)
        (let ([name* e*] ...) body* ... body)
        (e0 e1 ...)))
(define-language scm/L1
  (extends scm)
  (Expr (e body)
        (- (let ([name* e*] ...) body* ... body))
        (+ (let ([name* e*] ...) body))))

(define-pass wrap-begin : (scm Expr) (e) -> (scm/L1 Expr) ()
  [Expr : Expr (e) -> Expr ()
        [(let ([,name* ,e*] ...) ,body* ... ,body)
         `(let ([,name* ,e*] ...) (begin ,body* ... ,body))]])

(define-language scm/Final (extends scm/L1))
(define-pass final : (scm/L1 Expr) (e) -> (scm/Final Expr) ()
  [Expr : Expr (e) -> Expr ()])

(define-parser parse-scm scm)
(define (E x)
  ((compose final
            wrap-begin)
   (parse-scm x)))
