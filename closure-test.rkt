#lang racket
(require "compiler.rkt")

(compile-to-binary
 '(let ([make-adder (lambda (n)
                      (lambda (m)
                        (+ n m)))])
    (let ([add1 (make-adder 1)])
      (add1 3))))

(parameterize ([current-directory "./zig"])
  (match-define (list stdout stdin status stderr do)
    (process "zig build run"))
  (do 'wait)
  (read stderr))
