#lang nanopass

(provide arm64
         emit-program)

(require "../emit.rkt")

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

(define-pass emit-instruction : (arm64 Instruction) (i) -> * ()
  [Instruction : Instruction (i) -> * ()
               [(L ,label) (emit "~a:" label)]
               [(comment ,comment-string) (emit "// ~a" comment-string)]
               [(str ,src [,dst ,shift]) (emit "str ~a, [~a, ~a]" src dst shift)]
               [(ldr ,dst [,src ,shift]) (emit "ldr ~a, [~a, ~a]" dst src shift)]
               [(add ,dst ,src1 ,src2) (emit "add ~a, ~a, ~a" dst src1 src2)]
               [(mov ,dst ,imme-value) (emit "mov ~a, #~a" dst imme-value)]
               [(mov ,dst ,src) (emit "mov ~a, ~a" dst src)]
               [(b ,label) (emit "b ~a" label)]
               [else (emit "// ignored ~a" i)]])
(define-pass emit-program : (arm64 Program) (p) -> * ()
  [Program : Program (p) -> * ()
           [(,inst* ...)
            (emit ".section __TEXT,__text,regular,pure_instructions")
            (emit ".p2align 2")
            (emit ".globl _scheme_entry")
            (emit "_scheme_entry:")

            (emit "mov x28, x0")
            (map emit-instruction inst*)
            (emit "ret")]])
