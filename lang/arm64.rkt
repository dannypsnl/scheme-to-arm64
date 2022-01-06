#lang nanopass

(provide arm64
         emit-program
         flatten-arm64)

(define ops '(lsr lsl))
(define (op? x) (member x ops))
(define arm64-regs '(sp
                     w0 ; we use this to let converted result fit into char
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
             (op [op])
             (integer [shift imme-value])
             (list [instructions]))
  (Value [v]
         reg
         imme-value)
  (Instruction [inst]
               instructions ; don't directly use this, this is stands for expression that generates several instructions
               (global-label label-name)
               (label label-name)
               (comment comment-string)
               (stp src1 src2 [dst shift])
               (ldp src1 src2 [dst shift])
               (str src [dst shift])
               (ldr-fn-ptr reg label-name)
               (ldr dst [src shift])
               (lsr dst src imme-value)
               (lsl dst src imme-value)
               (add dst src0 src1 op v)
               (add dst src v)
               (sub dst src v)
               (mul dst src v)
               (sdiv dst src v)
               (and dst src v)
               (orr dst src v)
               (mov dst v)
               (cmp reg v)
               (closure-call reg)
               (call label-name)
               (ret)
               (b label-name)
               (b.eq label-name)
               (b.ne label-name)
               (b.lt label-name)
               (b.le label-name)
               (b.gt label-name)
               (b.ge label-name))
  (Program [p]
           (inst* ...)))

(define-language arm64/Final
  (extends arm64)
  (Instruction [inst]
               (- instructions)))
(define-pass final-arm64 : (arm64 Instruction) (i) -> (arm64/Final Instruction) ())
(define-pass flatten-arm64 : (arm64 Instruction) (i) -> (arm64/Final Program) ()
  (if (list? i)
      `(,(map final-arm64 (flatten i))...)
      `(,(final-arm64 i))))

(define-pass emit-instruction : (arm64/Final Instruction) (i) -> * ()
  [Value : Value (v) -> * ()
         [,reg reg]
         [,imme-value (format "#~a" imme-value)]]
  [Instruction : Instruction (i) -> * ()
               [(global-label ,label-name) (emit ".global ~a" label-name)
                                           (emit "~a:" label-name)]
               [(label ,label-name) (emit "~a:" label-name)]
               [(comment ,comment-string) (emit "// ~a" comment-string)]
               [(stp ,src1 ,src2 [,dst ,shift]) (emit "stp ~a, ~a, [~a, ~a]" src1 src2 dst shift)]
               [(ldp ,dst1 ,dst2 [,src ,shift]) (emit "ldp ~a, ~a, [~a, ~a]" dst1 dst2 src shift)]
               [(str ,src [,dst ,shift]) (emit "str ~a, [~a, ~a]" src dst shift)]
               [(ldr-fn-ptr ,reg ,label-name)
                ; pesudo command: `ldr x0, =_start` moves absolute address of `_start` to `x0`!
                (emit "ldr ~a, =~a" reg label-name)]
               [(ldr ,dst [,src ,shift]) (emit "ldr ~a, [~a, ~a]" dst src shift)]
               [(lsr ,dst ,src ,imme-value) (emit "lsr ~a, ~a, ~a" dst src imme-value)]
               [(lsl ,dst ,src ,imme-value) (emit "lsl ~a, ~a, ~a" dst src imme-value)]
               [(add ,dst ,src ,v) (emit "add ~a, ~a, ~a" dst src (Value v))]
               [(add ,dst ,src0 ,src1 ,op ,v) (emit "add ~a, ~a, ~a, ~a ~a" dst src0 src1 op (Value v))]
               [(sub ,dst ,src ,v) (emit "sub ~a, ~a, ~a" dst src (Value v))]
               [(mul ,dst ,src ,v) (emit "mul ~a, ~a, ~a" dst src (Value v))]
               [(sdiv ,dst ,src ,v) (emit "sdiv ~a, ~a, ~a" dst src (Value v))]
               [(and ,dst ,src ,v) (emit "and ~a, ~a, ~a" dst src (Value v))]
               [(orr ,dst ,src ,v) (emit "orr ~a, ~a, ~a" dst src (Value v))]
               [(mov ,dst ,v) (emit "mov ~a, ~a" dst (Value v))]
               [(cmp ,reg ,v) (emit "cmp ~a, ~a" reg (Value v))]
               [(closure-call ,reg) (emit "stp x29, x30, [sp, 8]")
                                    (emit "bl ~a" reg)
                                    (emit "ldp x29, x30, [sp, 8]")]
               [(call ,label-name) (emit "stp x29, x30, [sp, 8]")
                                   (emit "bl ~a" label-name)
                                   (emit "ldp x29, x30, [sp, 8]")]
               [(ret) (emit "ret")]
               [(b ,label-name) (emit "b ~a" label-name)]
               [(b.eq ,label-name) (emit "b.eq ~a" label-name)]
               [(b.ne ,label-name) (emit "b.ne ~a" label-name)]
               [(b.lt ,label-name) (emit "b.lt ~a" label-name)]
               [(b.le ,label-name) (emit "b.le ~a" label-name)]
               [(b.gt ,label-name) (emit "b.gt ~a" label-name)]
               [(b.ge ,label-name) (emit "b.ge ~a" label-name)]])
(define-pass emit-program : (arm64/Final Program) (p funcs) -> * ()
  (Program : Program (p) -> * ()
           [(,inst* ...)
            (emit ".global _scheme_entry")
            (emit "_scheme_entry:")
            (map emit-instruction inst*)
            (emit "ret")])
  (F : Program (funcs) -> * ()
     [(,inst* ...)
      (map emit-instruction inst*)])
  (emit ".text")
  (emit ".p2align 2")
  (F funcs)
  (Program p))

(define (emit . args)
  (apply printf args)
  (newline))
