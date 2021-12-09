#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt")

  (define (run args)
    (match args
      [(list file)
       (define p (open-input-file file))
       (let loop ([s (read file p)])
         (when (not (eof-object? s))
           (displayln s)
           (loop (read file p))))]
      [_ (printf "please provide a file~n")]))

  (define expression (make-parameter #f))
  (define show-asm (make-parameter #f))

  (command-line
   #:program "scheme"
   #:usage-help
   "scheme is a scheme compiler"
   #:once-each
   [("-e" "--expr") e "run single scheme expression" (expression (read (open-input-string e)))]
   [("-s") "show asm" (show-asm #t)]
   #:args args
   (define program (expression))
   (if program
       (begin
         (when (show-asm)
           (compile-program program))
         (displayln (compile-and-run program)))
       (run args))))
