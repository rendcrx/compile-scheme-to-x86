(load "tests-driver.scm")
(load "tests-1.9.1-req.scm")
(load "tests-1.8-req.scm")
(load "tests-1.7-req.scm")
(load "tests-1.6-opt.scm")
(load "tests-1.6-req.scm")
(load "tests-1.5-req.scm")
(load "tests-1.4-req.scm")
(load "tests-1.3-req.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define lazy #f)
(define lazy-string "")
(define (start-lazy)
  (string-append lazy-string "\n")
  (set! lazy #t))
(define (end-lazy) (set! lazy #f))
(define old-emit emit)
(define (emit . args)
  (if lazy
      (set! lazy-string (string-append lazy-string (apply format args) "\n"))
      (apply old-emit args)))

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
(define pairtag #x1)

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
    [(_ (prim-name si env arg* ...) b b* ...)
     (begin
       (putprop 'prim-name '*is-prim* #t)
       (putprop 'prim-name '*arg-count*
		(length '(arg* ...)))
       (putprop 'prim-name '*emitter*
		(lambda (si env arg* ...) b b* ...)))]))

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

(define (emit-primcall si env expr)
  (let ([prim (car expr)] [args (cdr expr)])
    (check-primcall-args prim args)
    (apply (primitive-emitter prim) si env args)))

(define-primitive (fxadd1 si env arg)
  (emit-expr si env arg)
  (emit "\taddl $~s, %eax" (immediate-rep 1)))

(define-primitive (fxsub1 si env arg)
  (emit-expr si env arg)
  (emit "\tsubl $~s, %eax" (immediate-rep 1)))

(define-primitive (char->fixnum si env arg)
  (emit-expr si env arg)
  (emit "\tshrl $~s, %eax" fixcharshift )
  (emit "\tsall $~s, %eax" fixshift))

(define-primitive (fixnum->char si env arg)
  (emit-expr si env arg)
  (emit "\tshll $~s, %eax" (- fixcharshift fixshift))
  (emit "\torl $~s, %eax" fixchartag))

(define (change-al-to-bool)
  (emit "\tmovzbl %al, %eax")
  (emit "\tsall $~s, %eax" bool-bits)
  (emit "\torl $~s, %eax" bool_f))

(define-primitive (fixnum? si env arg)
  (emit-expr si env arg)
  (emit "\tandl $~s, %eax" fixmask)
  (emit "\tcmpl $~s, %eax" fixtag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxzero? si env arg)
  (emit-expr si env arg)
  (emit "\tcmpl $0, %eax")
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (null? si env arg)
  (emit-expr si env arg)
  (emit "\tcmpl $~s, %eax" null)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (eq? si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (boolean? si env arg)
  (emit-expr si env arg)
  (emit "andl $~s, %eax" boolmask)
  (emit "cmpl $~s, %eax" bool_f)
  (emit "sete %al")
  (change-al-to-bool))

(define-primitive (char? si env arg)
  (emit-expr si env arg)
  (emit "\tandl $~s, %eax" fixcharmask)
  (emit "\tcmpl $~s, %eax" fixchartag)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (not si env arg)
  (emit-expr si env arg)
  (emit "\tcmpl $~s, %eax" bool_f)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fxlognot si env arg)
  (emit-expr si env arg)
  (emit "\tshrl $~s, %eax" fixshift)
  (emit "\tnotl %eax")
  (emit "\tshll $~s, %eax" fixshift))

(define unique-label
  (let ([count 0])
    (lambda ()
      (let ([L (format "L_~s" count)])
	(set! count (add1 count))
	L))))

(define (unique-labels l)
  (if (null? l)
      '()
      (cons (unique-label)
	    (unique-labels (cdr l)))))

(define (if? expr)
  (and (list? expr) (= (length expr) 4) (eq? (car expr) 'if)))

(define (if-test expr)
  (cadr expr))

(define (if-conseq expr)
  (caddr expr))

(define (if-altern expr)
  (cadddr expr))

(define (emit-if si env expr)
  (let ([alt-label (unique-label)]
	[end-label (unique-label)])
    (emit-expr si env (if-test expr))
    (emit "\tcmpl $~s, %eax" bool_f)
    (emit "\tje ~a" alt-label)
    (emit-expr si env (if-conseq expr))
    (emit "\tjmp ~a" end-label)
    (emit "~a:" alt-label)
    (emit-expr si env (if-altern expr))
    (emit "~a:" end-label)))

;; si point to the first empty space of stack top
(define-primitive (fx+ si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\taddl ~s(%esp), %eax" si))

(define-primitive (fx- si env arg1 arg2)
  (emit-expr si env arg2)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg1)
  (emit "\tsubl ~s(%esp), %eax" si))

(define-primitive (fx* si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tsarl $~s, %eax" fixshift)
  (emit "\tmull ~s(%esp)" si))

(define-primitive (fxlogor si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\torl ~s(%esp), %eax" si))

(define-primitive (fxlogand si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tandl ~s(%esp), %eax" si))

(define-primitive (fx= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsete %al")
  (change-al-to-bool))

(define-primitive (fx< si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetg %al")
  (change-al-to-bool))

(define-primitive (fx<= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetge %al")
  (change-al-to-bool))

(define-primitive (fx> si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetl %al")
  (change-al-to-bool))

(define-primitive (fx>= si env arg1 arg2)
  (emit-expr si env arg1)
  (emit "\tmovl %eax, ~s(%esp)" si)
  (emit-expr (- si wordsize) env arg2)
  (emit "\tcmpl ~s(%esp), %eax" si)
  (emit "\tsetle %al")
  (change-al-to-bool))

(define (variable? x)
  (not (list? x)))

(define (emit-variable-ref env expr)
  (cond
    [(null? env) (error 'emit-variable-ref "env error")]
    [else
      (let ([item (car env)])
	(let ([name (car item)]
	      [si (cdr item)])
	  (if (eq? name expr)
	      (emit "\tmovl ~s(%esp), %eax" si)
	      (emit-variable-ref (cdr env) expr))))]))

(define (let? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'let)))

(define (let-bindings x)
  (cadr x))

(define (first x)
  (car x))

(define (rest x)
  (cdr x))

(define (empty? x)
  (null? x))

(define (lhs x)
  (car x))

(define (rhs x)
  (cadr x))

(define (emit-stack-save si)
  (emit "\tmovl %eax, ~s(%esp)" si))

(define (next-stack-index si)
  (- si wordsize))

(define (extend-env name si env)
  (cons (cons name si)
	env))

(define (let-body x)
  (let ([body-exprs (cddr x)])
    (if (null? (cdr body-exprs))
	(car body-exprs)
	(cons 'begin body-exprs))))

(define (emit-let si env expr)
  (define (process-let bindings si new-env)
    (cond
      [(empty? bindings)
       (emit-expr si new-env (let-body expr))]
      [else
	(let ([b (first bindings)])
	  (emit-expr si env (rhs b))
	  (emit-stack-save si)
	  (process-let (rest bindings)
		       (next-stack-index si)
		       (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (let*? x)
  (and (list? x) (not (null? x)) (eq? (car x) 'let*)))

(define (emit-let* si env expr)
  (define (process-let* bindings si new-env)
    (cond
      [(empty? bindings)
       (emit-expr si new-env (let-body expr))]
      [else
	(let ([b (first bindings)])
	  (emit-expr si new-env (rhs b))
	  (emit-stack-save si)
	  (process-let* (rest bindings)
			(next-stack-index si)
			(extend-env (lhs b) si new-env)))]))
  (process-let* (let-bindings expr) si env))

(define (emit-immediate arg)
  (emit "\tmovl $~a, %eax" (immediate-rep arg)))

(define (letrec? expr)
  (and (list? expr) (not (null? expr)) (eq? (car expr) 'letrec)))

(define (letrec-bindings x)
  (cadr x))

;; don't care outside environment
(define (make-initial-env vars labels)
  (if (and (null? vars) (null? labels))
      '()
      (cons (cons (car vars)
		  (car labels))
	    (make-initial-env (cdr vars) (cdr labels)))))

(define (lambda-formals x)
  (cadr x))

(define (lambda-body x)
  (caddr x))

(define (emit-lambda env)
  (lambda (expr label)
    (start-lazy)
    (emit-function-header label)
    (let ([fmls (lambda-formals expr)]
	  [body (lambda-body expr)])
      (let f ([fmls fmls] [si (- wordsize)] [env env])
	(cond
	  [(empty? fmls)
	   (emit-tail-expr si env body)]
	  [else
	    (f (rest fmls)
	       (- si wordsize)
	       (extend-env (first fmls) si env))])))
    (end-lazy)))

(define (emit-adjust-base x)
  (emit "\taddl $~s, %esp" x))

(define (call-args x)
  (cdr x))

(define (call-target x)
  (car x))

(define (lookup name env)
  (if (null? env)
      (error 'lookup "error")
      (let ([item (car env)])
	(let ([env-name (car item)]
	      [env-label (cdr item)])
	  (if (eq? name env-name)
	      env-label
	      (lookup name (cdr env)))))))

(define (emit-call si label)
  (emit "\tcall ~a" label))

(define (app? expr)
  (and (list? expr) (not (null? expr))))

;; function call
(define (emit-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (first args))
      (emit "\tmovl %eax, ~s(%esp)" si)
      (emit-arguments (- si wordsize) (rest args))))
  (emit-arguments (- si wordsize) (call-args expr))
  (emit-adjust-base (+ si wordsize))
  (emit-call si (lookup (call-target expr) env))
  (emit-adjust-base (- (+ si wordsize))))

(define (letrec-body x)
  (caddr x))

(define (emit-letrec si expr)
  (let* ([bindings (letrec-bindings expr)]
	 [lvars (map lhs bindings)]
	 [lambdas (map rhs bindings)]
	 [labels (unique-labels lvars)]
	 [env (make-initial-env lvars labels)])
    (for-each (emit-lambda env) lambdas labels)
    (emit-expr si env (letrec-body expr))))

(define (emit-tail-immediate arg)
  (emit "\tmovl $~a, %eax" (immediate-rep arg))
  (emit "\tret"))

(define (emit-tail-variable-ref env expr)
  (cond
    [(null? env) (error 'emit-tail-variable-ref "env error")]
    [else
      (let ([item (car env)])
	(let ([name (car item)]
	      [si (cdr item)])
	  (if (eq? name expr)
	      (begin (emit "\tmovl ~s(%esp), %eax" si)
		     (emit "\tret"))
	      (emit-tail-variable-ref (cdr env) expr))))]))

(define (emit-tail-if si env expr)
  (let ([alt-label (unique-label)]
	[end-label (unique-label)])
    (emit-expr si env (if-test expr))
    (emit "\tcmpl $~s, %eax" bool_f)
    (emit "\tje ~a" alt-label)
    (emit-tail-expr si env (if-conseq expr))
    (emit "~a:" alt-label)
    (emit-tail-expr si env (if-altern expr))))

(define (emit-tail-let si env expr)
  (define (process-let bindings si new-env)
    (cond
      [(empty? bindings)
       (emit-tail-expr si new-env (let-body expr))]
      [else
	(let ([b (first bindings)])
	  (emit-expr si env (rhs b))
	  (emit-stack-save si)
	  (process-let (rest bindings)
		       (next-stack-index si)
		       (extend-env (lhs b) si new-env)))]))
  (process-let (let-bindings expr) si env))

(define (emit-tail-primcall si env expr)
  (emit-primcall si env expr)
  (emit "\tret"))

(define (last args)
  (if (null? (cdr args))
      (car args)
      (last (cdr args))))

(define (prev args)
  (if (null? (cdr args))
      '()
      (cons (car args)
	    (prev (cdr args)))))

(define (emit-tail-app si env expr)
  (define (emit-arguments si args)
    (unless (empty? args)
      (emit-expr si env (last args))
      (emit "\tmovl %eax, ~s(%esp)" (- (* (length args) wordsize)))
      (emit-arguments si (prev args))))
  (define (change-to-safe si args)
    (let* ([len (length args)]
	   [pos (- (+ 4 (* len wordsize)))])
      (if (< si pos)
	  si
	  pos)))
  (emit-arguments (change-to-safe si (call-args expr)) (call-args expr))
  (emit "\tjmp ~a" (lookup (call-target expr) env)))

(define (emit-tail-expr si env expr)
  (cond
    [(immediate? expr) (emit-tail-immediate expr)]
    [(variable? expr) (emit-tail-variable-ref env expr)]
    [(if? expr) (emit-tail-if si env expr)]
    [(let? expr) (emit-tail-let si env expr)]
    [(primcall? expr) (emit-tail-primcall si env expr)]
    [(app? expr) (emit-tail-app si env expr)]
    [else (error 'emit-tail-expr "error")]))

(define (pair? x)
  (and (list? x) (= (length x) 2) (eq? (car x) 'pair?)))

(define (emit-pair si env x)
  (emit-expr si env (cadr x))
  (emit "\tandl $7, %eax")
  (emit "\tcmpl $1, %eax")
  (emit "\tsete %al")
  (change-al-to-bool))

; (define (emit-pair si env x)
;   (if (eq? (car x) 'car)
;       (emit-car si env x)
;       (emit-cdr si env x)))

(define (emit-car si env x)
  (emit-expr si env (cadr x))
  (emit "\tmovl -1(%eax), %eax"))

(define (emit-cdr si env x)
  (emit-expr si env (cadr x))
  (emit "\tmovl 3(%eax), %eax"))

(define (cons? x)
  (and (list? x) (eq? (length x) 3) (eq? (car x) 'cons)))

(define (cons-car x)
  (cadr x))

(define (cons-cdr x)
  (caddr x))

(define (emit-cons si env x)
  (let ([car (cons-car x)]
	[cdr (cons-cdr x)])
    (emit "\tmovl %ebp, ~s(%esp)" si)
    (emit "\taddl $8, %ebp")
    (emit-expr (- si wordsize) env car)
    (emit "\tmovl ~s(%esp), %ebx" si)
    (emit "\tmovl %eax, (%ebx)")
    (emit-expr (- si wordsize) env cdr)
    (emit "\tmovl ~s(%esp), %ebx" si)
    (emit "\tmovl %eax, 4(%ebx)")
    (emit "\tmovl %ebx, %eax")
    (emit "\torl $1, %eax")))

(define (car? x)
  (and (list? x) (= 2 (length x)) (eq? (car x) 'car)))

(define (cdr? x)
  (and (list? x) (= 2 (length x)) (eq? (car x) 'cdr)))

(define (begin? x)
  (and (list? x) (<= 2 (length x)) (eq? (car x) 'begin)))

(define (emit-begin si env x)
  (let ([expr (cdr x)])
    (if (= (length expr) 1)
	(emit-expr si env (car expr))
	(begin
	  (emit-expr si env (car expr))
	  (emit-begin si env expr)))))

(define (set-car!? x)
  (and (list? x) (= 3 (length x)) (eq? (car x) 'set-car!)))

(define (emit-set-car! si env x)
  (let ([name (cadr x)]
	[new (caddr x)])
    (if (symbol? name)
	(let ([offset (lookup name env)])
	  (emit-expr si env new)
	  (emit "\tmovl ~s(%esp), %ebx" offset)
	  (emit "\tshr $1, %ebx")
	  (emit "\tshl $1, %ebx")
	  (emit "\tmovl %eax, (%ebx)"))
	(begin
	  (emit "\tmovl %eax, %ebx")
	  (emit "\tshr $1, %ebx")
	  (emit "\tshl $1, %ebx")
	  (emit-expr si env new)
	  (emit "\tmovl %eax, (%ebx)")))))

(define (set-cdr!? x)
  (and (list? x) (= 3 (length x)) (eq? (car x) 'set-cdr!)))

(define (emit-set-cdr! si env x)
  (let ([name (cadr x)]
	[new (caddr x)])
    (let ([offset (lookup name env)])
      (emit-expr si env new)
      (emit "\tmovl ~s(%esp), %ebx" offset)
      (emit "\tshr $1, %ebx")
      (emit "\tshl $1, %ebx")
      (emit "\tmovl %eax, 4(%ebx)"))))

(define (emit-expr si env expr)
  (cond
    [(immediate? expr) (emit-immediate expr)]
    [(variable? expr) (emit-variable-ref env expr)]
    [(if? expr) (emit-if si env expr)]
    [(let? expr) (emit-let si env expr)]
    [(let*? expr) (emit-let* si env expr)]
    [(cons? expr) (emit-cons si env expr)]
    [(pair? expr) (emit-pair si env expr)]
    [(car? expr) (emit-car si env expr)]
    [(cdr? expr) (emit-cdr si env expr)]
    [(set-car!? expr) (emit-set-car! si env expr)]
    [(set-cdr!? expr) (emit-set-cdr! si env expr)]
    [(begin? expr) (emit-begin si env expr)]
    [(primcall? expr) (emit-primcall si env expr)]
    [(letrec? expr) (emit-letrec si expr)]
    [(app? expr) (emit-app si env expr)]
    [else (error 'emit-expr "error")]))

(define (emit-function-header arg)
  (emit "\t.globl ~a" arg)
  (emit "\t.type ~a, @function" arg)
  (emit "~a:" arg))

(define (emit-program expr)
  (emit "\t.text")
  (emit-function-header "L_scheme_entry")
  (emit-expr (- wordsize) '() expr)
  (emit "\tret")
  (emit-function-header "scheme_entry")
  (emit "\tmovl 4(%esp), %ecx")
  (emit "\tmovl %ebx, 4(%ecx)")
  (emit "\tmovl %esi, 16(%ecx)")
  (emit "\tmovl %edi, 20(%ecx)")
  (emit "\tmovl %ebp, 24(%ecx)")
  (emit "\tmovl %esp, 28(%ecx)")
  (emit "\tmovl 12(%esp), %ebp")
  (emit "\tmovl 8(%esp), %esp")
  (emit "\tcall L_scheme_entry")
  (emit "\tmovl 4(%ecx), %ebx")
  (emit "\tmovl 16(%ecx), %esi")
  (emit "\tmovl 20(%ecx), %edi")
  (emit "\tmovl 24(%ecx), %ebp")
  (emit "\tmovl 28(%ecx), %esp")
  (emit "\tret")
  (emit lazy-string))
