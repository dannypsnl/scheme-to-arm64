#lang racket/base

(provide env
         make-env
         lookup
         var-set!)

(struct Env (m parent)
  #:mutable
  #:transparent)
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
