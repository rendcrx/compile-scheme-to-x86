(load "tests-driver.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define fixtag 0)
(define fixshift 2)
(define fixmask #x03)
(define bool-bits 6)
(define boolmask #x3F)
(define bool_f #x2F)
(define bool_t #x6F)
(define wordsize 4) ; base byte
(define fixnum-bits (- (* wordsize 8) fixshift))
(define fixlower (- (expt 2 (- fixnum-bits 1))))
(define fixupper (sub1 (expt 2 (- fixnum-bits 1))))
(define null #x3f)
(define fixcharshift 8)
(define fixcharmask #xff)
(define fixchartag #x0f)

(define (fixnum? x)
  (and (integer? x) (exact? x) (<= fixlower x fixupper)))

(define (immediate? x)
  (or (fixnum? x)
      (boolean? x)
      (null? x)
      (char? x)))

(define (immediate-rep x)
  (cond
    [(fixnum? x) (ash x fixshift)]
    [(boolean? x) (if x bool_t bool_f)]
    [(null? x) null]
    [(char? x) (+ 15 (ash (char->integer x) fixcharshift))]
    ))

(define-syntax define-primitive
  (syntax-rules ()
    [(_ (prim-name arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*emitter*
		(lambda (arg* ...) b b* ...)))]))

(define (primitive? x)
  (and (symbol? x) (getprop x '*is-prim*)))

(define (primitive-emitter x)
  (or (getprop x '*emitter*) (error 'primitive-emitter "error")))

(define (primcall? expr)
  (and (pair? expr) (primitive? (car expr))))

(define (primitive-arg-count x)
  (or (getprop x '*arg-count*) (error 'primitive-arg-count "error")))

(define (check-primcall-args prim args)
  (let ([len (length args)])
    (unless (= len (primitive-arg-count prim)) (error 'check-primcall-args "error"))))

(define (emit-primcall expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(define-primitive (fxadd1 arg)
  (emit-expr arg)
  (emit "\taddl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 arg)
  (emit-expr arg)
  (emit "\tsubl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum arg)
  (emit-expr arg)
  (emit "\tshrl $~s, %eax" fixcharshift )
  (emit "\tsall $~s, %eax" fixshift))

(define-primitive (fixnum->char arg)
  (emit-expr arg)
  (emit "\tshll $~s, %eax" (- fixcharshift fixshift))
  (emit "\torl $~s, %eax" fixchartag))

(define (change-al-to-bool)
  (emit "\tmovzbl %al, %eax")
  (emit "\tsall $~s, %eax" bool-bits)
  (emit "\torl $~s, %eax" bool_f))

(define-primitive (fixnum? arg)
  (emit-expr arg)
  (emit "\tandl $~s, %eax" fixmask)
  (emit "\tcmpl $~s, %eax" fixtag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxzero? arg)
  (emit-expr arg)
  (emit "\tcmpl $0, %eax")
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (null? arg)
  (emit-expr arg)
  (emit "\tcmpl $~s, %eax" null)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (boolean? arg)
  (emit-expr arg)
  (emit "andl $~s, %eax" boolmask)
  (emit "cmpl $~s, %eax" bool_f)
  (emit "sete %al")
  (change-al-to-bool))

(define-primitive (char? arg)
  (emit-expr arg)
  (emit "\tandl $~s, %eax" fixcharmask)
  (emit "\tcmpl $~s, %eax" fixchartag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (not arg)
  (emit-expr arg)
  (emit "\tcmpl $~s, %eax" bool_f)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxlognot arg)
  (emit-expr arg)
  (emit "\tshrl $~s, %eax" fixshift)
  (emit "\tnotl %eax")
  (emit "\tshll $~s, %eax" fixshift))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
	(set! count (add1 count))
	L))))

(define (if? expr)
  (and (list? expr) (= (length expr) 4) (eq? (car expr) 'if)))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if expr)
  (let ([alt-label (unique-label)]
	[end-label (unique-label)])
    (emit-expr (if-test expr))
    (emit "\tcmpl $~s, %eax" bool_f)
    (emit "\tje ~a" alt-label)
    (emit-expr (if-conseq expr))
    (emit "\tjmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr (if-altern expr))
    (emit "~a:" end-label)))

(define-syntax and
  (syntax-rules ()
    [(_) #t]
    [(_ e) e]
    [(_ e1 e2 ...)
     (if e1 (and e2 ...) #f)]))

(define-syntax or
  (syntax-rules ()
    [(_) #f]
    [(_ e) e]
    [(_ e1 e2 ...)
     (if e1 e1 (or e2 ...))]))

(define (emit-immediate arg)
  (emit "\tmovl $~a, %eax" (immediate-rep arg)))

(define (emit-expr expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr) (emit-if expr)]
    [(primcall? expr) (emit-primcall expr)]
    [else (error 'emit-expr "error")]))

(define (emit-function-header arg)
  (emit "\t.text")
  (emit "\t.globl ~a" arg)
  (emit "\t.type ~a, @function" arg)
  (emit "~a:" arg))

(define (emit-program expr)
  (emit-function-header "scheme_entry")
  (emit-expr expr)
  (emit "\tret"))
