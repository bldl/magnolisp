#lang racket

#|
|#

(require magnolisp/ast-repr magnolisp/ast-view
         racket/generic rackunit)

(define-view A ([#:field a]))
(define-view AB ([#:field a] [#:field b]))

(define (get-c obj) (HasA-a obj))
(define (set-c obj v) (set-HasA-a obj v))

(define-view C ([#:access c get-c set-c]))

(define-ast HasA (A C) ([#:none a]))
(define-ast HasAB (A AB) ([#:none a] [#:none b]))
(define-ast HasBA (A AB) ([#:none b] [#:none a]))

(check-eqv? 7 (C-c (HasA 7)))

(check-true (A=? (HasA 4) (HasAB 4 5)))
(check-true (A=? (HasA 4) (HasBA 6 4)))
(check-true (A=? (HasAB 4 7) (HasBA 6 4)))

(check-equal? '(4 7) (match (HasAB 4 7) [(AB a b) (list a b)] [_ #f]))
(check-equal? '(4 7) (match (HasBA 7 4) [(AB a b) (list a b)] [_ #f]))

(define-generics D-impl
  (get-d D-impl)
  (set-d D-impl v))

(struct DoesD (d)
        #:methods gen:D-impl
        [(define (get-d x) 1)
         (define (set-d x v) (void))])

(define-view D ([#:field a] [#:access d get-d set-d]))

(define-ast Weird (A AB D) ([#:none a] [#:none b])
  #:struct-options
  (#:methods gen:D-impl 
             [(define (get-d D-impl)
                (* 2 (Weird-b D-impl)))
              (define (set-d D-impl v)
                (set-Weird-b D-impl (/ v 2)))]))

(check-eqv? 4 (D-d (Weird 1 2)))
(check-eqv? 3/2 (Weird-b (D-copy (Weird 1 2) 7 3)))
(check-equal? (Weird 1 2) (D-copy (Weird 10 10) 1 4))
(check-equal? (Weird 1 2) (set-D-a (Weird 6 2) 1))
(check-equal? (Weird 1 2) (set-D-d (Weird 1 6) 4))
(check-equal? '(1 4) (match (Weird 1 2) [(D a d) (list a d)] [_ #f]))

(define-view Ast ([#:field annos]))
(define (get-type ast)
  (hash-ref (Ast-annos ast) 'type #f))
(define (set-type ast t)
  (set-Ast-annos ast (hash-set (Ast-annos ast) 'type t)))
(define-view Expr ([#:access type get-type set-type]))
(define-ast Lit (Ast Expr) ([#:none annos] [#:none dat]))

(check-eq? 'int (Expr-type (Lit #hasheq((type . int)) 5)))

(let ((e (Lit #hasheq() 6)))
  (check-eq? 'int (Expr-type (set-Expr-type e 'int))))

(define-view Num (#:fields num)
  #:generics-options
  (#:defaults ([number? 
                (define (Num-num x) x)
                (define (set-Num-num x v) v)
                (define (Num-copy x v) v)])))

(define-ast HasNum (Num) ([#:none num]))

(check-false (Num? "string"))
(check-true (Num? 5))
(check-false (Num? (HasA 4)))
(check-true (Num? (HasNum 7)))

(check-eqv? 5 (Num-num 5))
(check-eqv? 5 (set-Num-num 7 5))
(check-eqv? 5 (Num-copy 7 5))

(check-eqv? 15 (match (cons 7 (HasNum 8))
                 [(cons (Num x) (Num y)) (+ x y)]
                 [_ #f]))

(define-view HasX (#:fields x))
(define-ast FunnyCopy ([HasX (#:copy (lambda (fc x)
                                       (FunnyCopy 5)))])
  ([#:none x]))
(check-eqv? 5 (HasX-x (HasX-copy (FunnyCopy 1) 7)))

(define-ast FunnyOverride ([HasX ([#:field x])]) ([#:none x]))
(check-eqv? 6 (HasX-x (FunnyOverride 6)))

(define-ast SevenX ([HasX ([#:access x (lambda (obj) 7) (lambda (obj x) obj)])])
  ([#:none x]))
(check-eqv? 7 (HasX-x (set-HasX-x (SevenX 9) 8)))

(define (AC-getter obj)
  (AC-c obj))
(define (AC-setter obj b)
  (struct-copy AC obj [c b]))
(define-ast AC (A [AB ([#:access b AC-getter AC-setter])])
  ([#:none a] [#:none c]))
(check-match (A-copy (AB-copy (AC 1 2) 4 5) 6) (AC 6 5))
