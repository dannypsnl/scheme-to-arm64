#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt")

  (define expression (make-parameter #f))

  (command-line
   #:program "scheme"
   #:usage-help
   "scheme is a scheme compiler"
   #:once-each
   [("-e" "--expr") e "run single scheme expression"
                    (expression (read (open-input-string e)))]
   #:args args
   (if (expression)
       (void (compile-and-run (expression)))
       (match args
         [(list file)
          (define p (open-input-file file))
          (let loop ([s (read file p)])
            (when (not (eof-object? s))
              (displayln s)
              (loop (read file p))))]
         [_ (printf "please provide a file~n")]))))
