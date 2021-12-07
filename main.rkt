#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt")

  (define expression (make-parameter #f))
  (define generate-asm (make-parameter #f))

  (command-line
   #:program "scheme"
   #:usage-help
   "scheme is a scheme compiler"
   #:once-each
   [("-e" "--expr") e "run single scheme expression"
                    (expression (read (open-input-string e)))]
   [("-s") "generate asm to file"
           (generate-asm #t)]
   #:args args
   (define program (expression))
   (if program
       (begin
         (when (generate-asm)
           (with-output-to-file "debug.s"
             #:exists 'replace
             (lambda () (compile-program program))))
         (compile-and-run program)
         (void))
       (run args))))

(define (run args)
  (match args
    [(list file)
     (define p (open-input-file file))
     (let loop ([s (read file p)])
       (when (not (eof-object? s))
         (displayln s)
         (loop (read file p))))]
    [_ (printf "please provide a file~n")]))
