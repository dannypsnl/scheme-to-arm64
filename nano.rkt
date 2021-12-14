#lang nanopass

(require "env.rkt"
         "representation.rkt")

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

(define arm64-regs '(sp
                     x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10
                     x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
                     x21 x22 x23 x24 x25 x26 x27 x28 x29 x30))
(define (arm64-reg? x)
  (and (symbol? x) (member x arm64-regs)))

(define-language arm64
  (entry Program)
  (terminals (string [comment-string])
             (arm64-reg [reg src dst])
             (symbol [label])
             (integer [shift imme-value])
             (list [instructions]))
  (Instruction [inst]
               instructions ; don't directly use this, this is stands for expression that generates several instructions
               (L label)
               (comment comment-string)
               (str src [dst shift])
               (ldr dst [src shift])
               (add dst src1 src2)
               (mov dst imme-value)
               (mov dst src)
               (b label))
  (Program [p]
           (inst* ...)))

(define-pass compile-scm : (scm/Final Expr) (e si) -> (arm64 Instruction) ()
  (definitions
    (define stack-index si))
  (Expr : Expr (e) -> Instruction ()
        [,name `(ldr x0 [sp ,(lookup name)])]
        [,c `(mov x0 ,(immediate-rep c))]
        [(define ,name ,e)
         (define ret
           (list (Expr e)
                 `(str x0 [sp ,stack-index])))
         (var-set! name stack-index)
         (set! stack-index (- stack-index 8))
         ret]
        [(begin ,e* ... ,e)
         (parameterize ([env (make-env (make-hash))])
           (for/list ([e (append e* (list e))])
             (Expr e)))]
        [(let ([,name* ,e*] ...) ,body)
         `(comment "ignore")]
        [(,e0 ,e1 ...)
         (match e0
           ['+  (list (Expr (car e1))
                      `(str x0 [sp ,stack-index])
                      (for/list ([v (cdr e1)])
                        (list (Expr v)
                              `(ldr x1 [sp ,stack-index])
                              `(add x1 x1 x0)
                              `(str x1 [sp ,stack-index])))
                      `(ldr x0 [sp ,stack-index]))]
           [else `(comment "ignore")])]))
(define-pass convert : (arm64 Instruction) (i) -> (arm64 Program) ()
  (if (list? i)
      `((L scheme_entry)
        ,(flatten i) ...)
      `((L scheme_entry) ,i)))
(define (compile-expr scm-exp stack-index)
  (convert (compile-scm scm-exp stack-index)))

(define-parser parse-scm scm)
(define (E x)
  ((compose final
            wrap-begin)
   (parse-scm x)))
(compile-expr (E '1) (- 8))
(compile-expr (E '#\c) (- 8))
(compile-expr (E '#t) (- 8))
(compile-expr (E '#f) (- 8))
(compile-expr (E '(begin (define x 1)
                         (define y 2)
                         (+ x y)))
              (- 8))
