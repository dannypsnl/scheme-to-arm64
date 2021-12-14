#lang nanopass

(require "env.rkt"
         "representation.rkt"
         "lang/scheme.rkt"
         "lang/arm64.rkt")

(define wordsize 8)

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
         (set! stack-index (- stack-index wordsize))
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

(define (compile-and-run program [debug? #f])
  (compile-to-binary program debug?)
  (if debug?
      (system "lldb ./a.out")
      (system "./a.out"))
  (void))

(compile-and-run '(begin (define x 1)
                         (define y 2)
                         (+ 1 2)))
