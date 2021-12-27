#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt")

  (define (compile-and-run program debug?)
    ; generate /tmp/scheme.s
    (compile-to-binary program)
    (parameterize ([current-directory "./zig"])
      (if debug?
          (begin (system "zig build")
                 (system "lldb ./zig-out/bin/zig"))
          (system "zig build run")))
    (void))

  (define expression (make-parameter #f))
  (define show-asm (make-parameter #f))
  (define debug (make-parameter #f))

  (define/match (run args)
    [((list file))
     (for ([s (sequence->list (in-port read (open-input-file file)))])
       (displayln s))]
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
