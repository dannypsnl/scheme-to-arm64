#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt")

  (define (compile-and-run program debug?)
    (compile-to-binary program debug?)
    (if debug?
        (system "lldb ./a.out")
        (system "./a.out"))
    (void))

  (define expression (make-parameter #f))
  (define show-asm (make-parameter #f))
  (define debug (make-parameter #f))

  (define/match (run args)
    [((list file))
     (define p (open-input-file file))
     (let loop ([s (read p)])
       (when (not (eof-object? s))
         (displayln s)
         (loop (read file p))))]
    [(_) (printf "please provide a file~n")])

  (command-line
   #:program "scheme"
   #:usage-help
   "scheme is a scheme compiler"
   #:once-each
   [("-e" "--expr") e "run single scheme expression" (expression (read (open-input-string e)))]
   [("-s") "show asm" (show-asm #t)]
   [("-d" "--debug") "generate debug information" (debug #t)]
   #:args args
   (define program (expression))
   (if program
       (begin
         (when (show-asm)
           (compile-program program))
         (compile-and-run program (debug)))
       (run args))))
