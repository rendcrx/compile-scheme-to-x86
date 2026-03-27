(load "tests-driver.scm")
(load "tests-1.2-req.scm")
(load "tests-1.1-req.scm")

(define fixshift 2)
(define fixmask #x03)
(define bool_f #x2F)
(define bool_t #x6F)
(define wordsize 4) ; byte
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

(define (emit-program x)
  (unless (immediate? x)
    (error 'emit-program "the input must be integer"))
  (emit "\t.text")
  (emit "\t.globl scheme_entry")
  (emit "\t.type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "\tmovl $~s, %eax" (immediate-rep x))
  (emit "\tret"))
