#lang nanopass

(provide arm64
         emit
         program%
         block%
         current-block)

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
  (entry Instruction)
  (terminals (string [comment-string])
             (arm64-reg [reg src dst])
             (symbol [label-name])
             (op [op])
             (integer [shift imme-value])
             (void [void]))
  (Value [v]
         reg
         imme-value)
  (Instruction [inst]
               void
               (comment comment-string)
               (stp src1 src2 [dst shift])
               (ldp src1 src2 [dst shift])
               (str src [dst shift])
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
               (b label-name)
               (bl label-name)
               (b.eq label-name)
               (b.ne label-name)
               (b.lt label-name)
               (b.le label-name)
               (b.gt label-name)
               (b.ge label-name)))

(define-pass emit-instruction : (arm64 Instruction) (i) -> * ()
  [Value : Value (v) -> * ()
         [,reg reg]
         [,imme-value (format "#~a" imme-value)]]
  [Instruction : Instruction (i) -> * ()
               [,void (error 'unreachable)]
               [(comment ,comment-string) (emit-to-asm "// ~a" comment-string)]
               [(stp ,src1 ,src2 [,dst ,shift]) (emit-to-asm "stp ~a, ~a, [~a, ~a]" src1 src2 dst shift)]
               [(ldp ,dst1 ,dst2 [,src ,shift]) (emit-to-asm "ldp ~a, ~a, [~a, ~a]" dst1 dst2 src shift)]
               [(str ,src [,dst ,shift]) (emit-to-asm "str ~a, [~a, ~a]" src dst shift)]
               [(ldr ,dst [,src ,shift]) (emit-to-asm "ldr ~a, [~a, ~a]" dst src shift)]
               [(lsr ,dst ,src ,imme-value) (emit-to-asm "lsr ~a, ~a, ~a" dst src imme-value)]
               [(lsl ,dst ,src ,imme-value) (emit-to-asm "lsl ~a, ~a, ~a" dst src imme-value)]
               [(add ,dst ,src ,v) (emit-to-asm "add ~a, ~a, ~a" dst src (Value v))]
               [(add ,dst ,src0 ,src1 ,op ,v) (emit-to-asm "add ~a, ~a, ~a, ~a ~a" dst src0 src1 op (Value v))]
               [(sub ,dst ,src ,v) (emit-to-asm "sub ~a, ~a, ~a" dst src (Value v))]
               [(mul ,dst ,src ,v) (emit-to-asm "mul ~a, ~a, ~a" dst src (Value v))]
               [(sdiv ,dst ,src ,v) (emit-to-asm "sdiv ~a, ~a, ~a" dst src (Value v))]
               [(and ,dst ,src ,v) (emit-to-asm "and ~a, ~a, ~a" dst src (Value v))]
               [(orr ,dst ,src ,v) (emit-to-asm "orr ~a, ~a, ~a" dst src (Value v))]
               [(mov ,dst ,v) (emit-to-asm "mov ~a, ~a" dst (Value v))]
               [(cmp ,reg ,v) (emit-to-asm "cmp ~a, ~a" reg (Value v))]
               [(b ,label-name) (emit-to-asm "b ~a" label-name)]
               [(bl ,label-name) (emit-to-asm "bl ~a" label-name)]
               [(b.eq ,label-name) (emit-to-asm "b.eq ~a" label-name)]
               [(b.ne ,label-name) (emit-to-asm "b.ne ~a" label-name)]
               [(b.lt ,label-name) (emit-to-asm "b.lt ~a" label-name)]
               [(b.le ,label-name) (emit-to-asm "b.le ~a" label-name)]
               [(b.gt ,label-name) (emit-to-asm "b.gt ~a" label-name)]
               [(b.ge ,label-name) (emit-to-asm "b.ge ~a" label-name)]])

(define (emit-to-asm . args)
  (apply printf args)
  (newline))

(define (emit inst)
  (send (current-block) emit inst))
(define current-block (make-parameter #f))

(define program%
  (class object%
    (define blocks '())

    (define/public (append-block b)
      (set! blocks (append blocks (list b))))
    (define/public (emit-asm)
      (emit-to-asm ".section __TEXT,__text,regular,pure_instructions")
      (emit-to-asm ".p2align 2")
      (emit-to-asm ".globl _scheme_entry")
      (for ([block blocks])
        (send block emit-asm))
      (emit-to-asm "ret"))

    (super-new)))

(define block%
  (class object%
    (init-field name)
    (define instructions '())

    (define/public (emit i)
      (set! instructions (append instructions (list i))))
    (define/public (emit-asm)
      (emit-to-asm "~a:" name)
      (for ([i instructions])
        (emit-instruction i)))

    (super-new)))
