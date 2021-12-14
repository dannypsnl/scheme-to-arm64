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
             (symbol [label-name])
             (integer [shift imme-value])
             (list [instructions]))
  (Value [v]
         reg
         imme-value)
  (Instruction [inst]
               instructions ; don't directly use this, this is stands for expression that generates several instructions
               (label label-name)
               (comment comment-string)
               (stp src1 src2 [dst])
               (str src [dst shift])
               (ldr dst [src shift])
               (lsr dst src imme-value)
               (add dst src v)
               (sub dst src v)
               (mul dst src v)
               (sdiv dst src v)
               (and dst src v)
               (orr dst src v)
               (mov dst v)
               (cmp reg v)
               (b label-name)
               (b.eq label-name)
               (b.ne label-name)
               (b.lt label-name)
               (b.le label-name)
               (b.gt label-name)
               (b.ge label-name)
               )
  (Program [p]
           (inst* ...)))

(define-pass emit-instruction : (arm64 Instruction) (i) -> * ()
  [Value : Value (v) -> * ()
         [,reg reg]
         [,imme-value (format "#~a" imme-value)]]
  [Instruction : Instruction (i) -> * ()
               [(label ,label-name) (emit "~a:" label-name)]
               [(comment ,comment-string) (emit "// ~a" comment-string)]
               [(stp ,src1 ,src2 [,dst]) (emit "stp ~a, ~a, [~a]" src1 src2 dst)]
               [(str ,src [,dst ,shift]) (emit "str ~a, [~a, ~a]" src dst shift)]
               [(ldr ,dst [,src ,shift]) (emit "ldr ~a, [~a, ~a]" dst src shift)]
               [(lsr ,dst ,src ,imme-value) (emit "lsr ~a, ~a, ~a" dst src imme-value)]
               [(add ,dst ,src ,v) (emit "add ~a, ~a, ~a" dst src (Value v))]
               [(sub ,dst ,src ,v) (emit "sub ~a, ~a, ~a" dst src (Value v))]
               [(mul ,dst ,src ,v) (emit "mul ~a, ~a, ~a" dst src (Value v))]
               [(sdiv ,dst ,src ,v) (emit "sdiv ~a, ~a, ~a" dst src (Value v))]
               [(and ,dst ,src ,v) (emit "and ~a, ~a, ~a" dst src (Value v))]
               [(orr ,dst ,src ,v) (emit "orr ~a, ~a, ~a" dst src (Value v))]
               [(mov ,dst ,v) (emit "mov ~a, ~a" dst (Value v))]
               [(cmp ,reg ,v) (emit "cmp ~a, ~a" reg (Value v))]
               [(b ,label-name) (emit "b ~a" label-name)]
               [(b.eq ,label-name) (emit "b.eq ~a" label-name)]
               [(b.ne ,label-name) (emit "b.ne ~a" label-name)]
               [(b.lt ,label-name) (emit "b.lt ~a" label-name)]
               [(b.le ,label-name) (emit "b.le ~a" label-name)]
               [(b.gt ,label-name) (emit "b.gt ~a" label-name)]
               [(b.ge ,label-name) (emit "b.ge ~a" label-name)]
               [else (emit "// ignored ~a" i)]
               ])
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
