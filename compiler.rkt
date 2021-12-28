#lang nanopass

(provide compile-program
         compile-to-binary)

(require "env.rkt"
         "representation.rkt"
         "lang/scheme.rkt"
         "lang/arm64.rkt"
         syntax/parse/define)

(define-syntax-parser define-label
  [(_ label-names ...)
   #'(begin
       (define label-names (gensym 'LLB)) ...)])

(define wordsize 8)

(define-pass compile-scm : (scm/Final Expr) (e si) -> (arm64 Instruction) ()
  (definitions
    (define stack-index si)
    (define (Expr-on-offset e offset)
      (set! stack-index (- stack-index offset))
      (Expr e)
      (set! stack-index (+ stack-index offset))))
  (emit-is-x0-equal-to : Expr (e) -> Instruction ()
                       [,c (define-label if-true end)
                           (emit `(cmp x0 ,c))
                           (emit `(b.eq ,if-true))
                           (emit `(mov x0 ,(immediate-rep #f)))
                           (emit `(b ,end))
                           (define if-true-block (new-block if-true))
                           (parameterize ([current-block if-true-block])
                             (emit `(mov x0 ,(immediate-rep #t))))
                           (define end-block (new-block end))
                           (current-block end-block)])
  (Expr : Expr (e) -> Instruction ()
        [,name (guard (eq? name 'null))
               (emit `(mov x0 ,(immediate-rep null)))]
        [,name (emit `(ldr x0 [sp ,(lookup name)]))]
        [,c (emit `(mov x0 ,(immediate-rep c)))]
        [,v (match-define (vector vs ...) v)
            (emit `(mov x0 ,(* (length vs) wordsize)))
            (emit `(stp x29 x30 [sp 8]))
            (emit `(bl _GC_malloc))
            (emit `(ldp x29 x30 [sp 8]))
            (emit `(mov x27 x0))
            (emit `(mov x0 ,(length vs)))
            (emit `(str x0 [x27 0]))
            (emit `(orr x1 x27 ,vec-tag))
            (for ([v vs]
                  [i (range (length vs))])
              (Expr v)
              (emit `(str x0 [x27 ,(* (add1 i) wordsize)])))
            (emit `(mov x0 x1))]
        [(define ,name ,e)
         (define var-offset stack-index)
         (var-set! name var-offset)
         (Expr e)
         (set! stack-index (- stack-index wordsize))
         (emit `(str x0 [sp ,var-offset]))]
        [(begin ,e* ... ,e)
         (parameterize ([env (make-env (make-hash))])
           (for ([e (append e* (list e))])
             (Expr e)))]
        [(cond [,e ,body] ...)
         (define-label end)
         (for ([e e]
               [body body])
           (define-label body-tag next)
           (Expr e)
           (emit-is-x0-equal-to (immediate-rep #t))
           (emit `(b.ne ,next))
           (define body-block (new-block body-tag))
           (parameterize ([current-block body-block])
             (Expr body)
             (emit `(b ,end)))
           (current-block (new-block next)))
         (define end-block (new-block end))
         (current-block end-block)]
        [(if ,e0 ,e1 ,e2)
         (define-label if-true end)
         (Expr e0)
         (emit-is-x0-equal-to (immediate-rep #t))
         (emit `(b.eq ,if-true))
         (Expr e2)
         (emit `(b ,end))
         (define if-true-block (new-block if-true))
         (parameterize ([current-block if-true-block])
           (Expr e1))
         (define end-block (new-block end))
         (current-block end-block)]
        [(prim ,op ,e1 ...)
         (case op
           [(cons) (match-define (list e-car e-cdr) e1)
                   (Expr-on-offset e-cdr wordsize)
                   (emit `(mov x1 x0))
                   (Expr e-car)
                   (emit `(stp x29 x30 [sp 8]))
                   (emit `(bl __scheme_cons))
                   (emit `(ldp x29 x30 [sp 8]))]
           [(add1 sub1) (Expr (car e1))
                        (case op
                          [(add1) (emit `(add x0 x0 ,(immediate-rep 1)))]
                          [(sub1) (emit `(sub x0 x0 ,(immediate-rep 1)))])]
           [(+ - * /) (Expr (car e1))
                      (emit `(str x0 [sp ,stack-index]))
                      (for ([v (cdr e1)])
                        (set! stack-index (- stack-index wordsize))
                        (Expr v)
                        (set! stack-index (+ stack-index wordsize))
                        (emit `(ldr x1 [sp ,stack-index]))
                        (case op
                          [(+) (emit `(add x1 x1 x0))]
                          [(-) (emit `(sub x1 x1 x0))]
                          [(*) (emit `(lsr x0 x0 ,fixnum-shift))
                               (emit `(mul x1 x1 x0))]
                          [(/) (emit `(lsr x0 x0 ,fixnum-shift))
                               (emit `(sdiv x1 x1 x0))])
                        (emit `(str x1 [sp ,stack-index])))
                      (emit `(ldr x0 [sp ,stack-index]))]
           [(= < > <= >= char=?)
            (define-label end)
            (for ([left e1]
                  [right (cdr e1)])
              (define-label if-true)
              (Expr left)
              (when (eq? op 'char=?)
                (emit `(lsr x0 x0 ,char-shift)))
              `(mov x8 x0)
              (Expr right)
              (when (eq? op 'char=?)
                (emit `(lsr x0 x0 ,char-shift)))
              (emit `(cmp x8 x0))
              (emit (case op
                      [(=) `(b.eq ,if-true)]
                      [(<) `(b.lt ,if-true)]
                      [(>) `(b.gt ,if-true)]
                      [(<=) `(b.le ,if-true)]
                      [(>=) `(b.ge ,if-true)]
                      [(char=?) `(b.eq ,if-true)]))
              (emit `(mov x0 ,(immediate-rep #f)))
              (emit `(b ,end))
              (define if-true-block (new-block if-true))
              (current-block if-true-block))
            (emit `(mov x0 ,(immediate-rep #t)))
            (define end-block (new-block end))
            (current-block end-block)]
           [(integer? boolean? char? string? vector? zero? null? car cdr)
            (list
             (Expr (car e1))
             (case op
               [(integer?) (emit `(and x0 x0 ,fixnum-mask))
                           (emit-is-x0-equal-to 0)]
               [(boolean?) (emit `(and x0 x0 ,bool-mask))
                           (emit-is-x0-equal-to bool-tag)]
               [(char?) (emit `(and x0 x0 ,char-mask))
                        (emit-is-x0-equal-to char-tag)]
               [(string?) (emit `(and x0 x0 ,ptr-mask))
                          (emit-is-x0-equal-to str-tag)]
               [(vector?) (emit `(and x0 x0 ,ptr-mask))
                          (emit-is-x0-equal-to vec-tag)]
               [(zero?) (emit-is-x0-equal-to 0)]
               [(car) (emit `(ldr x0 [x0 ,(- pair-tag)]))]
               [(cdr) (emit `(ldr x0 [x0 ,(- wordsize pair-tag)]))]
               [(null?) (emit-is-x0-equal-to pair-tag)]))]
           [(void) (emit `(mov x0 ,(immediate-rep (void))))]
           [(and or)
            (define-label if-true end)
            (for ([v e1])
              (Expr v)
              (case op
                [(and) (emit-is-x0-equal-to (immediate-rep #f))]
                [(or) (emit-is-x0-equal-to (immediate-rep #t))])
              (emit `(b.eq ,if-true)))
            (emit (case op
                    [(and) `(mov x0 ,(immediate-rep #t))]
                    [(or) `(mov x0 ,(immediate-rep #f))]))
            (emit `(b ,end))
            (define if-true-block (new-block if-true))
            (parameterize ([current-block if-true-block])
              (emit (case op
                      [(and) `(mov x0 ,(immediate-rep #f))]
                      [(or) `(mov x0 ,(immediate-rep #t))])))
            (define end-block (new-block end))
            (current-block end-block)]
           [(make-string make-vector)
            (match e1
              [`(,len) (emit `(mov x1 0))
                       (Expr len)]
              [`(,len ,fill-by) (Expr fill-by)
                                (emit `(mov x1 x0))
                                (Expr len)])
            (emit `(stp x29 x30 [sp 8]))
            (emit `(bl ,(case op [(make-string) '__scheme_make_string] [(make-vector) '__scheme_make_vector])))
            (emit `(ldp x29 x30 [sp 8]))]
           [(string-ref vector-ref)
            (Expr-on-offset (cadr e1) wordsize)
            (Expr (car e1))
            (emit `(add x1 x0 ,(- wordsize (case op [(string-ref) str-tag] [(vector-ref) vec-tag]))))
            ; get index, so now index is in x0
            ; x1 is current pointer, x1 <- x1 + x0>>shift is offset of value
            `(add x1 x1 x0 lsr ,(case op [(string-ref) fixnum-shift] [(vector-ref) (+ fixnum-shift 2)]))
            `(ldr x0 [x1 0])
            (case op ; now we convert loaded char back to encoded char
              [(string-ref) (emit `(lsl x0 x0 ,char-shift))
                            (emit `(orr x0 x0 ,char-tag))]
              [else (void)])]
           [(string-length vector-length)
            (Expr (car e1))
            (emit `(sub x0 x0 ,(case op [(string-length) str-tag] [(vector-length) vec-tag])))
            (emit `(ldr x0 [x0 0]))
            (emit `(lsl x0 x0 ,fixnum-shift))])]
        [(,e0 ,e1 ...)
         `(comment "todo: function call")])
  (Expr e))

(define current-program (new program%))
(define (new-block name)
  (define b (new block% [name name]))
  (send current-program append-block b)
  b)

(define (compile-program scm-exp)
  (define scheme-entry-block (new block% [name '_scheme_entry]))
  (send current-program append-block scheme-entry-block)
  (parameterize ([current-block scheme-entry-block])
    (compile-scm (E scm-exp) (- wordsize)))
  (send current-program emit-asm))

(define (compile-to-binary program)
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program))))

(define (compile-and-eval program)
  (compile-to-binary program)
  (parameterize ([current-directory "./zig"])
    (match-define (list stdout stdin status stderr do)
      (process "zig build run"))
    (do 'wait)
    (read stderr)))

(module+ test
  (require rackunit)

  ; arithmetic
  (check-equal? (compile-and-eval '1) 1)
  (check-equal? (compile-and-eval '(add1 1)) 2)
  (check-equal? (compile-and-eval '(+ 1 1)) 2)
  (check-equal? (compile-and-eval '(* 1 2)) 2)
  (check-equal? (compile-and-eval '(+ 1 2 3)) 6)
  (check-equal? (compile-and-eval '(- 1 2 3)) -4)
  ; conditional
  ; (check-equal? (compile-and-eval '(if #f 1)) eof) ; since we will get a #<void> which prints nothing
  (check-equal? (compile-and-eval '(if #t 1)) 1)
  (check-equal? (compile-and-eval '(if #t 1 2)) 1)
  (check-equal? (compile-and-eval '(if #f 1 2)) 2)
  (check-equal? (compile-and-eval '(cond
                                     [(= (- 2 1) 1) 1]
                                     [#t 2]))
                1)
  (check-equal? (compile-and-eval '(cond [#t (define x 2)
                                             x]))
                2)
  ; type check
  (check-equal? (compile-and-eval '(char=? #\c #\a)) #f)
  (check-equal? (compile-and-eval '(char=? #\b #\b)) #t)
  (check-equal? (compile-and-eval '(char? #\c)) #t)
  (check-equal? (compile-and-eval '(char? 1)) #f)
  (check-equal? (compile-and-eval '(boolean? #f)) #t)
  (check-equal? (compile-and-eval '(boolean? 1)) #f)
  (check-equal? (compile-and-eval '(integer? 1)) #t)
  (check-equal? (compile-and-eval '(integer? #f)) #f)
  ; let and define
  (check-equal? (compile-and-eval '(let ([x 1]) x)) 1)
  (check-equal? (compile-and-eval '(let ([x 1])
                                     (let ([y (* 2 x)])
                                       (cons x y))))
                '(1 . 2))
  (check-equal? (compile-and-eval '(let ()
                                     (define x 1)
                                     (define y 2)
                                     (cons x y)))
                '(1 . 2))
  ; logical
  (check-equal? (compile-and-eval '(and #t #t)) #t)
  (check-equal? (compile-and-eval '(and #f #t)) #f)
  (check-equal? (compile-and-eval '(and #t #f)) #f)
  (check-equal? (compile-and-eval '(and #t #t #t)) #t)
  (check-equal? (compile-and-eval '(and #t #f #t)) #f)
  (check-equal? (compile-and-eval '(or #t #f)) #t)
  (check-equal? (compile-and-eval '(or #t #f #t)) #t)
  (check-equal? (compile-and-eval '(or #f #t)) #t)
  (check-equal? (compile-and-eval '(or #f #f)) #f)
  ; comparsion
  (check-equal? (compile-and-eval '(= 1 1)) #t)
  (check-equal? (compile-and-eval '(<= (sub1 10) (* 9 (/ 4 2)))) #t)
  (check-equal? (compile-and-eval '(> 2 1)) #t)
  (check-equal? (compile-and-eval '(>= 2 2)) #t)
  (check-equal? (compile-and-eval '(>= 3 2)) #t)
  (check-equal? (compile-and-eval '(>= 2 3)) #f)
  (check-equal? (compile-and-eval '(< 1 2 3 4 5 6 7)) #t)
  (check-equal? (compile-and-eval '(< 2 1)) #f)
  (check-equal? (compile-and-eval '(<= 2 1)) #f)
  (check-equal? (compile-and-eval '(<= 2 2)) #t)
  (check-equal? (compile-and-eval '(zero? 0)) #t)
  (check-equal? (compile-and-eval '(zero? #\c)) #f)
  ; list and pair
  (check-equal? (compile-and-eval 'null) '())
  (check-equal? (compile-and-eval '(null? null)) #t)
  (check-equal? (compile-and-eval '(cons #\c 1)) (cons #\c 1))
  (check-equal? (compile-and-eval '(cons 1 (cons 2 (cons 3 4)))) '(1 2 3 . 4))
  (check-equal? (compile-and-eval '(cons (cons 2 3) 1)) '((2 . 3) . 1))
  (check-equal? (compile-and-eval '(car (cons 1 2))) 1)
  (check-equal? (compile-and-eval '(cdr (cons 1 2))) 2)
  (check-equal? (compile-and-eval '(quote 1 2 3)) '(1 2 3))
  (check-equal? (compile-and-eval '(list 1 2 3)) '(1 2 3))
  ; (check-equal? (compile-and-eval '(list 1 (list 1 2 3) 3)) '(1 (1 2 3) 3))
  ; string
  (check-equal? (compile-and-eval '(make-string 5 #\c)) "ccccc")
  (check-equal? (compile-and-eval '(string-ref (make-string 2 #\q) 1)) #\q)
  (check-equal? (compile-and-eval '(string? (make-string 2 #\a))) #t)
  (check-equal? (compile-and-eval '(string-length (make-string 2 #\b))) 2)
  ; vector
  (check-equal? (compile-and-eval '#(1 2 3)) #(1 2 3))
  (check-equal? (compile-and-eval '(vector 1 2 3)) #(1 2 3))
  (check-equal? (compile-and-eval '(vector)) #())
  (check-equal? (compile-and-eval '(make-vector 2 #\c)) #(#\c #\c))
  (check-equal? (compile-and-eval '(vector-ref (make-vector 2 2) 0)) 2)
  (check-equal? (compile-and-eval '(vector? (make-vector 2 2))) #t)
  (check-equal? (compile-and-eval '(vector-length (make-vector 3 #\b))) 3)
  )
