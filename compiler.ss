(load "tests-driver.scm")
(load "tests-1.5-req.scm")
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
    [(_ (prim-name si arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*emitter*
		(lambda (si arg* ...) b b* ...)))]))

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

(define (emit-primcall si expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(define-primitive (fxadd1 si arg)
  (emit-expr si arg)
  (emit "\taddl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si arg)
  (emit-expr si arg)
  (emit "\tsubl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum si arg)
  (emit-expr si arg)
  (emit "\tshrl $~s, %eax" fixcharshift )
  (emit "\tsall $~s, %eax" fixshift))

(define-primitive (fixnum->char si arg)
  (emit-expr si arg)
  (emit "\tshll $~s, %eax" (- fixcharshift fixshift))
  (emit "\torl $~s, %eax" fixchartag))

(define (change-al-to-bool)
  (emit "\tmovzbl %al, %eax")
  (emit "\tsall $~s, %eax" bool-bits)
  (emit "\torl $~s, %eax" bool_f))

(define-primitive (fixnum? si arg)
  (emit-expr si arg)
  (emit "\tandl $~s, %eax" fixmask)
  (emit "\tcmpl $~s, %eax" fixtag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxzero? si arg)
  (emit-expr si arg)
  (emit "\tcmpl $0, %eax")
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (null? si arg)
  (emit-expr si arg)
  (emit "\tcmpl $~s, %eax" null)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (boolean? si arg)
  (emit-expr si arg)
  (emit "andl $~s, %eax" boolmask)
  (emit "cmpl $~s, %eax" bool_f)
  (emit "sete %al")
  (change-al-to-bool))

(define-primitive (char? si arg)
  (emit-expr si arg)
  (emit "\tandl $~s, %eax" fixcharmask)
  (emit "\tcmpl $~s, %eax" fixchartag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (not si arg)
  (emit-expr si arg)
  (emit "\tcmpl $~s, %eax" bool_f)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxlognot si arg)
  (emit-expr si arg)
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

(define (emit-if si expr)
  (let ([alt-label (unique-label)]
	[end-label (unique-label)])
    (emit-expr si (if-test expr))
    (emit "\tcmpl $~s, %eax" bool_f)
    (emit "\tje ~a" alt-label)
    (emit-expr si (if-conseq expr))
    (emit "\tjmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si (if-altern expr))
    (emit "~a:" end-label)))

(define (emit-immediate arg)
  (emit "\tmovl $~a, %eax" (immediate-rep arg)))

(define (emit-expr si expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(if? expr) (emit-if si expr)]
    [(primcall? expr) (emit-primcall si expr)]
    [else (error 'emit-expr "error")]))

(define (emit-function-header arg)
  (emit "\t.text")
  (emit "\t.globl ~a" arg)
  (emit "\t.type ~a, @function" arg)
  (emit "~a:" arg))

;; si point to the first empty space of stack top
(define-primitive (fx+ si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\taddl ~s(%esp), %eax" si))

(define-primitive (fx- si arg1 arg2)
  (emit-expr si arg2)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg1)
  (emit "\tsubl ~s(%esp), %eax" si))

(define-primitive (fx* si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tsarl $~s, %eax" fixshift)
  (emit "\tmull ~s(%esp)" si))

(define-primitive (fxlogor si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\torl ~s(%esp), %eax" si))

(define-primitive (fxlogand si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tandl ~s(%esp), %eax" si))

(define-primitive (fx= si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fx< si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetg %al")
  (change-al-to-bool))

(define-primitive (fx<= si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetge %al")
  (change-al-to-bool))

(define-primitive (fx> si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetl %al")
  (change-al-to-bool))

(define-primitive (fx>= si arg1 arg2)
  (emit-expr si arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetle %al")
  (change-al-to-bool))

(define (emit-program expr)
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) expr)
  (emit "\tret")
  (emit-function-header "scheme_entry")
  (emit "\tmovl %esp, %ecx")     ; store %esp
  (emit "\tmovl 4(%esp), %esp")  ; get and update %esp
  (emit "\tcall L_scheme_entry") ; call procedure
  (emit "\tmovl %ecx, %esp")     ; restore %esp
  (emit "\tret"))
