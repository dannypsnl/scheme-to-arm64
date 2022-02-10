#lang racket

(module+ main
  (require racket/cmdline
           "compiler.rkt"
           "runtime.rkt")

  (define (compile-and-run program debug?)
    ; generate /tmp/scheme.s
    (compile-to-binary program)
    (parameterize ([current-directory runtime])
      (if debug?
          (begin (system "zig build")
                 (system "lldb ./zig-out/bin/zig"))
          (system "zig build run")))
    (void))

  (define expression (make-parameter #f))
  (define show-asm (make-parameter #f))
  (define debug (make-parameter #f))

  (command-line
   #:program "scheme"
   #:usage-help
   "scheme is a scheme compiler"
   #:once-each
   [("-e" "--expr") e "run single scheme expression" (expression (read (open-input-string e)))]
   [("-s") "show asm" (show-asm #t)]
   [("-d" "--debug") "generate debug information" (debug #t)]
   #:args ()
   (define program (expression))
   (unless program
     (error 'expression-missing "please provide expression to run"))
   (when (show-asm)
     (compile-program program))
   (compile-and-run program (debug))))
