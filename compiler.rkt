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
      (define r (Expr e))
      (set! stack-index (+ stack-index offset))
      r))
  (emit-is-x0-equal-to : Expr (e) -> Instruction ()
                       [,c (define-label if-true end)
                           (list
                            `(cmp x0 ,c)
                            `(b.eq ,if-true)
                            `(mov x0 ,(immediate-rep #f))
                            `(b ,end)
                            `(label ,if-true)
                            `(mov x0 ,(immediate-rep #t))
                            `(label ,end))])
  (Expr : Expr (e) -> Instruction ()
        [,name (guard (eq? name 'null))
               `(mov x0 ,(immediate-rep null))]
        [,name `(ldr x0 [sp ,(lookup name)])]
        [,c `(mov x0 ,(immediate-rep c))]
        [,v (match-define (vector vs ...) v)
            (list `(mov x0 ,(* (length vs) wordsize))
                  `(stp x29 x30 [sp 8])
                  `(bl _GC_malloc)
                  `(ldp x29 x30 [sp 8])
                  `(mov x27 x0)
                  `(mov x0 ,(length vs))
                  `(str x0 [x27 0])
                  `(orr x1 x27 ,vec-tag)
                  (for/list ([v vs]
                             [i (range (length vs))])
                    (list (Expr v)
                          `(str x0 [x27 ,(* (add1 i) wordsize)])))
                  `(mov x0 x1))]
        [(define ,name ,e)
         (define var-offset stack-index)
         (var-set! name var-offset)
         (define r (Expr e))
         (set! stack-index (- stack-index wordsize))
         (list r `(str x0 [sp ,var-offset]))]
        [(begin ,e* ... ,e)
         (parameterize ([env (make-env (make-hash))])
           (for/list ([e (append e* (list e))])
             (Expr e)))]
        [(cond [,e ,body] ...)
         (define-label end)
         (append
          (for/list ([e e]
                     [body body])
            (define-label body-tag next)
            (list
             (Expr e)
             (emit-is-x0-equal-to (immediate-rep #t))
             `(b.ne ,next)
             `(label ,body-tag)
             (Expr body)
             `(b ,end)
             `(label ,next)))
          (list `(label ,end)))]
        [(if ,e0 ,e1 ,e2)
         (define-label if-true end)
         (list (Expr e0)
               (emit-is-x0-equal-to (immediate-rep #t))
               `(b.eq ,if-true)
               (Expr e2)
               `(b ,end)
               `(label ,if-true)
               (Expr e1)
               `(label ,end))]
        [(prim ,op ,e1 ...)
         (case op
           [(cons) (match-define (list e-car e-cdr) e1)
                   (list (Expr-on-offset e-cdr wordsize)
                         `(mov x1 x0)
                         (Expr e-car)
                         `(stp x29 x30 [sp 8])
                         `(bl __scheme_cons)
                         `(ldp x29 x30 [sp 8]))]
           [(add1 sub1) (list (Expr (car e1))
                              (case op
                                [(add1) `(add x0 x0 ,(immediate-rep 1))]
                                [(sub1) `(sub x0 x0 ,(immediate-rep 1))]))]
           [(+ - * /) (list
                       (Expr (car e1))
                       `(str x0 [sp ,stack-index])
                       (for/list ([v (cdr e1)])
                         (set! stack-index (- stack-index wordsize))
                         (define e (Expr v))
                         (set! stack-index (+ stack-index wordsize))
                         (list e
                               `(ldr x1 [sp ,stack-index])
                               (case op
                                 [(+) `(add x1 x1 x0)]
                                 [(-) `(sub x1 x1 x0)]
                                 [(*) (list `(lsr x0 x0 ,fixnum-shift)
                                            `(mul x1 x1 x0))]
                                 [(/) (list `(lsr x0 x0 ,fixnum-shift)
                                            `(sdiv x1 x1 x0))])
                               `(str x1 [sp ,stack-index])))
                       `(ldr x0 [sp ,stack-index]))]
           [(= < > <= >= char=?)
            (define-label end)
            (append
             (for/list ([left e1]
                        [right (cdr e1)])
               (define-label if-true)
               (list
                (Expr left)
                (if (eq? op 'char=?) `(lsr x0 x0 ,char-shift) '())
                `(mov x8 x0)
                (Expr right)
                (if (eq? op 'char=?) `(lsr x0 x0 ,char-shift) '())
                `(cmp x8 x0)
                (case op
                  [(=) `(b.eq ,if-true)]
                  [(<) `(b.lt ,if-true)]
                  [(>) `(b.gt ,if-true)]
                  [(<=) `(b.le ,if-true)]
                  [(>=) `(b.ge ,if-true)]
                  [(char=?) `(b.eq ,if-true)])
                `(mov x0 ,(immediate-rep #f))
                `(b ,end)
                `(label ,if-true)))
             (list `(mov x0 ,(immediate-rep #t))
                   `(label ,end)))]
           [(integer? boolean? char? string? vector? zero? null? car cdr)
            (list
             (Expr (car e1))
             (case op
               [(integer?) (list `(and x0 x0 ,fixnum-mask)
                                 (emit-is-x0-equal-to 0))]
               [(boolean?) (list `(and x0 x0 ,bool-mask)
                                 (emit-is-x0-equal-to bool-tag))]
               [(char?) (list `(and x0 x0 ,char-mask)
                              (emit-is-x0-equal-to char-tag))]
               [(string?) (list `(and x0 x0 ,ptr-mask)
                                (emit-is-x0-equal-to str-tag))]
               [(vector?) (list `(and x0 x0 ,ptr-mask)
                                (emit-is-x0-equal-to vec-tag))]
               [(zero?) (emit-is-x0-equal-to 0)]
               [(car) `(ldr x0 [x0 ,(- pair-tag)])]
               [(cdr) `(ldr x0 [x0 ,(- wordsize pair-tag)])]
               [(null?) (emit-is-x0-equal-to pair-tag)]))]
           [(void) `(mov x0 ,(immediate-rep (void)))]
           [(and or)
            (define-label if-true end)
            (list
             (for/list ([v e1])
               (list (Expr v)
                     (case op
                       [(and) (emit-is-x0-equal-to (immediate-rep #f))]
                       [(or) (emit-is-x0-equal-to (immediate-rep #t))])
                     `(b.eq ,if-true)))
             (case op
               [(and) `(mov x0 ,(immediate-rep #t))]
               [(or) `(mov x0 ,(immediate-rep #f))])
             `(b ,end)
             `(label ,if-true)
             (case op
               [(and) `(mov x0 ,(immediate-rep #f))]
               [(or) `(mov x0 ,(immediate-rep #t))])
             `(label ,end))]
           [(make-string make-vector)
            (define len (car e1))
            (list (Expr len)
                  `(lsr x0 x0 ,fixnum-shift)
                  `(stp x29 x30 [sp 8])
                  `(bl _GC_malloc)
                  `(ldp x29 x30 [sp 8])
                  `(mov x27 x0)
                  ; save pointer and tag it
                  `(orr x1 x27 ,(case op [(make-string) str-tag] [(make-vector) vec-tag]))
                  (Expr len)
                  `(lsr x0 x0 ,fixnum-shift)
                  ; store length
                  `(str x0 [x27 0])
                  `(add x27 x27 ,wordsize)
                  ; middle
                  (match e1
                    [`(,len) (list)]
                    [`(,len ,fit-by)
                     (define-label loop)
                     (list
                      ; move len to x3
                      `(mov x3 x0)
                      ; set counter x2 by len
                      `(mov x2 ,0)
                      `(label ,loop)
                      (Expr fit-by)
                      (if (equal? op 'make-string) `(lsr w0 w0 ,char-shift) '())
                      `(str x0 [x27 0])
                      ; increase pointer with shift
                      `(add x27 x27 ,(case op [(make-string) 1] [(make-vector) wordsize]))
                      `(cmp x2 x3)
                      `(add x2 x2 ,1)
                      `(b.lt ,loop))])
                  ; middle
                  `(mov x0 x1))]
           [(string-ref vector-ref)
            (set! stack-index (- stack-index wordsize))
            (define e (Expr (cadr e1)))
            (set! stack-index (+ stack-index wordsize))
            (list
             (Expr (car e1))
             `(add x1 x0 ,(- wordsize (case op [(string-ref) str-tag] [(vector-ref) vec-tag])))
             e
             ; get index, so now index is in x0
             ; x1 is current pointer, x1 <- x1 + x0>>shift is offset of value
             `(add x1 x1 x0 lsr ,(case op [(string-ref) fixnum-shift] [(vector-ref) (+ fixnum-shift 2)]))
             `(ldr x0 [x1 0])
             (case op ; now we convert loaded char back to encoded char
               [(string-ref) (list `(lsl x0 x0 ,char-shift)
                                   `(orr x0 x0 ,char-tag))]
               [else '()]))]
           [(string-length vector-length)
            (list (Expr (car e1))
                  `(sub x0 x0 ,(case op [(string-length) str-tag] [(vector-length) vec-tag]))
                  `(ldr x0 [x0 0])
                  `(lsl x0 x0 ,fixnum-shift))])]
        [(,e0 ,e1 ...)
         `(comment "todo function call")])
  (Expr e))

(define (compile-expr scm-exp stack-index)
  (flatten-arm64 (compile-scm scm-exp stack-index)))

(define (compile-program e)
  (emit-program (compile-expr (E e) (- wordsize))))

(define (compile-to-binary program [debug? #f])
  (with-output-to-file "/tmp/scheme.s"
    #:exists 'replace
    (lambda () (compile-program program)))
  (with-output-to-file "/tmp/scheme.c"
    #:exists 'replace
    (lambda ()
      (displayln "
extern void show(long);
extern int printf(const char * restrict format, ... );
extern long scheme_entry();

int main() {
  show(scheme_entry());
  printf(\"\\n\");
}")))
  (define cmd "clang -target arm64-apple-darwin-macho -lschemeruntime -lgc /tmp/scheme.c /tmp/scheme.s")
  (system (if debug? (string-append cmd " -g") cmd)))

(define (compile-and-eval program)
  (compile-to-binary program)
  (match-define (list stdout stdin status stderr do)
    (process "./a.out"))
  (do 'wait)
  (read stdout))

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
