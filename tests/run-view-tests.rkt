#lang racket

#|
|#

(require (relative-in magnolisp
                      "ast-repr.rkt" "ast-view.rkt"
                      "strategy.rkt" "util.rkt")
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

(let ()
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
  (check-eqv? 3/2 (Weird-b (copy-D (Weird 1 2) 7 3)))
  (check-equal? (Weird 1 2) (copy-D (Weird 10 10) 1 4))
  (check-equal? (Weird 1 2) (set-D-a (Weird 6 2) 1))
  (check-equal? (Weird 1 2) (set-D-d (Weird 1 6) 4))
  (check-equal? '(1 4) (match (Weird 1 2) [(D a d) (list a d)] [_ #f])))

(let ()
  (define-view Ast ([#:field annos]))
  (define (get-type ast)
    (hash-ref (Ast-annos ast) 'type #f))
  (define (set-type ast t)
    (set-Ast-annos ast (hash-set (Ast-annos ast) 'type t)))
  (define-view Expr ([#:access type get-type set-type]))
  (define-ast Lit (Ast Expr) ([#:none annos] [#:none dat]))
  
  (check-eq? 'int (Expr-type (Lit #hasheq((type . int)) 5)))
  
  (let ((e (Lit #hasheq() 6)))
    (check-eq? 'int (Expr-type (set-Expr-type e 'int)))))

(let ()
  (define-view Num (#:fields num))

  (define-ast HasNum (Num) ([#:none num]))

  (check-false (Num? "string"))
  (check-false (Num? (HasA 4)))
  (check-true (Num? (HasNum 7)))

  (check-eqv? 5 (Num-num (HasNum 5)))
  (check-true (Num=? (HasNum 5) (set-Num-num (HasNum 7) 5)))
  (check-true (Num=? (HasNum 5) (copy-Num (HasNum 7) 5)))
  
  (check-eqv? 15 (match (cons (HasNum 7) (HasNum 8))
                   [(cons (Num x) (Num y)) (+ x y)]
                   [_ #f])))

(let ()
  (define-view HasX (#:fields x))
  (define-ast FunnyCopy ([HasX (#:copy (lambda (fc x)
                                         (FunnyCopy 5)))])
    ([#:none x]))
  (check-eqv? 5 (HasX-x (copy-HasX (FunnyCopy 1) 7)))
  
  (define-ast FunnyOverride ([HasX ([#:field x])]) ([#:none x]))
  (check-eqv? 6 (HasX-x (FunnyOverride 6)))
  
  (define-ast SevenX ([HasX ([#:access x (lambda (obj) 7) (lambda (obj x) obj)])])
    ([#:none x]))
  (check-eqv? 7 (HasX-x (set-HasX-x (SevenX 9) 8))))

(let ()
  (define (AC-getter obj)
    (AC-c obj))
  (define (AC-setter obj b)
    (struct-copy AC obj [c b]))
  (define-ast AC (A [AB ([#:access b AC-getter AC-setter])])
    ([#:none a] [#:none c]))
  (check-match (copy-A (copy-AB (AC 1 2) 4 5) 6) (AC 6 5)))

;;; 
;;; renamed view fields
;;; 

(let () ;; "manual" renaming with `#:access`
  (define-ast Var () ([#:none id]))
  (define-view Assign ([#:field lv] [#:field rv]))
  (define-ast AssignStat (Assign) ([#:just lv] [#:just rv]))
  (define (get-e obj) (AssignExpr-e obj))
  (define (set-e obj e) (set-AssignExpr-e obj e))
  (define-ast AssignExpr ([Assign
                           ([#:access rv get-e set-e])
                           ]) ([#:just lv] [#:just e]))
  (check-true (Assign=? (AssignStat (Var 'x) (Var 'y))
                        (AssignExpr (Var 'x) (Var 'y))))
  (check-false (Assign=? (AssignStat (Var 'x) (Var 'y))
                         (AssignExpr (Var 'y) (Var 'y)))))

(let ()
  (define-ast Var () ([#:none id]))
  (define-view Assign ([#:field lv] [#:field rv]))
  (define-ast AssignStat (Assign) ([#:just lv] [#:just rv]))
  (define-ast AssignExpr ([Assign ([#:field rv #:use e])])
    ([#:just lv] [#:just e]))
  (check-true (Assign=? (AssignStat (Var 'x) (Var 'y))
                        (AssignExpr (Var 'x) (Var 'y))))
  (check-false (Assign=? (AssignStat (Var 'x) (Var 'y))
                         (AssignExpr (Var 'y) (Var 'y)))))

(let ()
  (define-ast Var () ([#:none id]))
  (define-view Assign ([#:field lv] [#:field rv #:use e]))
  (define-ast AssignStat (Assign) ([#:just lv] [#:just e]))
  (define-ast AssignExpr (Assign) ([#:just lv] [#:just e]))
  (check-eq? 'x (Var-id (Assign-lv (AssignStat (Var 'x) (Var 'y)))))
  (check-eq? 'x (Var-id (Assign-lv (AssignExpr (Var 'x) (Var 'y)))))
  (check-eq? 'x (Var-id (AssignStat-lv (AssignStat (Var 'x) (Var 'y)))))
  (check-eq? 'x (Var-id (AssignExpr-lv (AssignExpr (Var 'x) (Var 'y)))))
  (check-eq? 'y (Var-id (Assign-rv (AssignStat (Var 'x) (Var 'y)))))
  (check-eq? 'y (Var-id (Assign-rv (AssignExpr (Var 'x) (Var 'y)))))
  (check-eq? 'y (Var-id (AssignStat-e (AssignStat (Var 'x) (Var 'y)))))
  (check-eq? 'y (Var-id (AssignExpr-e (AssignExpr (Var 'x) (Var 'y)))))
  (check-true (Assign=? (AssignStat (Var 'x) (Var 'y))
                        (AssignExpr (Var 'x) (Var 'y))))
  (check-false (Assign=? (AssignStat (Var 'x) (Var 'y))
                         (AssignExpr (Var 'y) (Var 'y))))
  (check-equal?
   (AssignStat (Var 'x) (Var 'y))
   (copy-Assign (AssignStat (Var 'z) (Var 'z)) (Var 'x) (Var 'y)))
  (check-match
   (copy-Assign (AssignExpr (Var 'z) (Var 'z)) (Var 'x) (Var 'y))
   (Assign (Var 'x) (Var 'y))))

;;; 
;;; view-based traversals
;;; 

;; An empty view.
(let ()
  (define-view V () #:traversable)
  (define-ast A (V) ())
  (let ((ast (A)))
    (check-equal? (V-term-fields ast) '())
    (let ((x (set-V-term-fields ast '())))
      (check-equal? ast x))))

;; A view without sub-terms.
(let ()
  (define-view V ([#:field #:none v]) #:traversable)
  (define-ast A (V) ([#:none v] [#:many e]))
  (check-equal? (V-term-fields (A 7 '())) '())
  (let ((ast (A 7 '())))
    (define x (set-V-term-fields ast '()))
    (check-equal? ast x)))

;; A view with one sub-term field.
(let ()
  (define-view V ([#:field #:just v]) #:traversable)
  (define-ast A (V) ([#:just v] [#:many e]))
  (let ((ast (A 7 (list (A 10 '()))))) ;; not actually storing a term
    (check-equal? (V-term-fields ast) '(7))
    (define x (set-V-term-fields ast '(8)))
    (check-equal? x (set-A-v ast 8))
    (check-true (V=? x (set-A-v ast 8)))))
  
;; A view with many sub-terms in a field.
(let ()
  (define-view V ([#:field #:many e]) #:traversable)
  (define-ast A (V) ([#:just v] [#:many e]))
  (let ((ast (A 7 (list 8 9))))
    (check-equal? (V-term-fields ast) '((8 9)))
    (define x (set-V-term-fields ast '((9 10))))
    (check-equal? x (set-A-e ast '(9 10)))
    (check-equal? (A-v x) (A-v ast))))

;; A view with an #:access term field.
(let ()
  (define (get-b x) (V-a x))
  (define (set-b x b) (set-V-a x b))
  (define-view V ([#:field #:none a]
                  [#:access #:just b get-b set-b]) #:traversable)
  (define-ast A (V) ([#:none a] [#:none c]))
  (define-ast Nil () ())
  (define nil (Nil))
  (define sub (A nil 7))
  (define ast (A sub 8))
  (check-eq? (A-a ast) (V-b ast))
  (check-equal? (V-term-fields ast) (list sub))
  (define ast0 (set-V-term-fields ast (list nil)))
  (check-eq? nil (V-a ast0))
  (check-eq? nil (V-b ast0))
  (check-eq? nil (A-a ast0))
  (void))

;; A view with multiple #:field term fields.
(let ()
  (define-view V ([#:field #:just a]
                  [#:field #:just b]
                  [#:field #:many c]) #:traversable)
  (define-view W () #:traversable)
  (define-ast Nil () ())
  (define nil (Nil))
  (define-ast A (V W) ([#:just a] [#:just d]
                       [#:many c] [#:just b]))
  (define ast (A nil nil null nil))
  (check-true (null? (W-term-fields ast)))
  (check-equal? (V-term-fields ast) `(,nil ,nil ()))
  (define ast2 (set-V-term-fields ast (list ast ast (list ast ast))))
  (check-true (= (length (A-c ast2)) 2))
  (check-eq? (V-a ast2) (car (V-c ast2)))
  (let ((lst (V-term-fields ast2)))
    (check-equal? ast2 (set-V-term-fields ast lst)))
  (void))

;; A view with a mixture of term fields.
(let ()
  (define (get-b x) (A-b x))
  (define (set-b x v) (set-A-b x v))
  (define-view V ([#:field #:just a]
                  [#:access #:many b get-b set-b]
                  [#:field #:many c]) #:traversable)
  (define-ast A (V) ([#:just a] [#:many b] [#:many c]))
  (define-ast Nil () ())
  (define nil (Nil))
  (define ast0 (A nil null null))
  (check-equal? (V-term-fields ast0) `(,nil () ()))
  (define ast1 (set-V-term-fields ast0 (list ast0 (list nil) (list ast0))))
  (let* ((ast2 (set-V-a ast1 ast0))
         (ast2 (set-V-b ast2 (list nil)))
         (ast2 (set-V-c ast2 (list ast0))))
    (check-equal? ast2 ast1))
  (let* ((ast2 (set-A-a ast1 ast0))
         (ast2 (set-A-b ast2 (list nil)))
         (ast2 (set-A-c ast2 (list ast0))))
    (check-equal? ast2 ast1))
  (void))

;; A view with more than one term field, and also a non-term one.
(let ()
  (define-view V ([#:field #:none a]
                  [#:field #:just b]
                  [#:field #:many c]) #:traversable)
  (define-ast A (V) ([#:none a] [#:just b] [#:many c]))
  (define-ast Nil () ())
  (define nil (Nil))
  (define ast0 (A 'a nil null))
  (check-equal? (V-term-fields ast0) `(,nil ()))
  (define ast1 (set-V-term-fields ast0 (list ast0 (list ast0))))
  (let* ((ast2 ast0)
         (ast2 (set-V-a ast2 'b))
         (ast2 (set-V-b ast2 nil)) 
         (ast2 (set-V-c ast2 null))
         (ast3 (set-A-a ast0 'b)))
    (check-equal? ast2 ast3))
  (void))

;; View-based traversal.
(let ()
  (define-view V ([#:field #:many c]) #:traversable)
  (define-ast A (V) ([#:none a] [#:many b] [#:many c]))
  (define (f ast)
    (set-A-a ast (add1 (A-a ast))))
  (define ast0 (A 0 null null))
  (define ast1 (A 0 (list ast0) (list ast0)))
  (define ast7 (A 7 null null))
  (define ast77 (A 7 null (list ast7)))
  (define ast2 (A 7 (list ast0 ast1) (list ast7 ast77)))
  (define V-all (make-view-term-rewrite-all V))
  (let ((rw (topdown-rewriter f #:rewrite-all V-all))
        (bad? #f))
    (define (chk ast)
      (define a (A-a ast))
      (unless (or (= a 0) (= a 8))
        (set! bad? #t)))
    ((topdown-visitor chk) (rw ast2))
    (check-false bad?)))

;; View-based + concrete traversal.
(let ()
  (define (get-ans ast) (A-ans ast))
  (define (set-ans ast lst) (set-A-ans ast lst))
  (define-view V ([#:access #:many ans get-ans set-ans])
    #:traversable)
  (define-ast A (V) ([#:none ans] [#:none val] [#:many lst]))
  (define V-all (make-view-term-rewrite-all V))
  (define both-all (combined-rewrite-all V-all term-rewrite-all))
  (define V-visit (make-view-term-rewrite-all V))
  (define both-visit (combined-visit-all V-visit term-visit-all))
  (define (count ast)
    (define n 0)
    (define (inc x)
      (set! n (+ n (A-val x))))
    ((topdown-visitor inc #:visit-all both-all) ast)
    n)
  (define (f ast) (set-A-val ast (add1 (A-val ast))))
  (define ast0 (A null 0 null))
  (check-eqv? 0 (count ast0))
  (define ast1 (A (list ast0) 1 (list ast0)))
  (check-eqv? 1 (count ast1))
  (define ast1c ((topdown-rewriter f #:rewrite-all term-rewrite-all) ast1))
  (check-eqv? 3 (count ast1c))
  (define ast1v ((topdown-rewriter f #:rewrite-all V-all) ast1))
  (check-eqv? 3 (count ast1v))
  (define ast1b ((topdown-rewriter f #:rewrite-all both-all) ast1))
  (check-eqv? 4 (count ast1b))
  (define ast2 (A (list ast1) 1 (list ast0)))
  (define ast2b ((topdown-rewriter f #:rewrite-all both-all) ast2))
  (check-eqv? 7 (count ast2b)))

;;; 
;;; implied views
;;; 
(let ()
  (define-view R ([#:field r]))
  (define-view V ([#:field v]) #:also (R))
  (define-ast A (V) ([#:none a] [#:none r] [#:none v]))
  (define a0 (A 1 2 3))
  (check-pred A? a0)
  (check-pred V? a0)
  (check-pred R? a0)
  (check-eqv? 1 (A-a a0))
  (check-eqv? 3 (V-v a0))
  (check-eqv? 2 (A-r a0))
  (check-eqv? 2 (R-r a0))
  (check-true (V=? a0 (A 3 4 3)))
  (check-true (R=? a0 (A 0 2 7)))
  (check-eqv? 9 (R-r (set-R-r a0 9)))
  (check-eqv? 9 (R-r (copy-V (copy-R a0 9) 8))))

;;; 
;;; partial views
;;; 
(let ()
  (define-view V ([#:field v]) #:partial)
  (define-ast A (V) ([#:none a] [#:none r] [#:none v]))
  (define a0 (A 1 2 3))
  (check-pred A? a0)
  (check-eqv? 1 (procedure-arity V?))
  (check-false (V? 42))
  (check-pred V? a0)

  (define (B-in-V? b)
    (> (B-v b) 5))
  (define-ast B ([V (#:predicate B-in-V?)]) ([#:none v]))
  (check-eqv? 0 (B-v (B 0)))
  (check-pred V? (B 6))
  (check-pred (negate V?) (B 5))
  (check-true (matches? (B 8) (V _)))
  (check-false (matches? (B 3) (V _)))

  (void))

(let ()
  (define-view DefVar (#:fields id v) #:partial)
  (define-ast Undefined () () #:singleton ())
  (define (def? ast) (not (Undefined? (DeclVar-v ast))))
  (define-ast DeclVar ([DefVar (#:predicate def?)])
    ([#:none id] [#:none v]))
  (check-true (DefVar? (DeclVar 'x 1)))
  (check-false (DefVar? (DeclVar 'y the-Undefined)))
  (check-eqv? 2 (length (filter-map
                         (match-lambda
                           [(DefVar id v) (list id v)]
                           [_ #f])
                         (list
                          (DeclVar 'x the-Undefined)
                          (DeclVar 'y 2)
                          (DeclVar 'z 3)
                          (DeclVar 'w the-Undefined)))))
  (void))

(let () ;; (NopStat) = (SeqStat ())
  (define-ast Thing () () #:singleton ())
  (define-view NopStat () #:partial)
  (define (nop? ast) (null? (SeqStat-ss ast)))
  (define-ast SeqStat ([NopStat (#:predicate nop?)]) ([#:many ss]))
  (check-false (NopStat? the-Thing))
  (check-false (NopStat? (SeqStat (list the-Thing))))
  (check-true (NopStat? (SeqStat null)))
  (void))

(let () ;; (AssignStat lv rv) = (ExprStat (AssignExpr lv rv))
  (define-ast Thing () () #:singleton ())
  (define-ast Var () ([#:none id]))
  (define get-lv (match-lambda [(ExprStat (AssignExpr lv rv)) lv]))
  (define get-rv (match-lambda [(ExprStat (AssignExpr lv rv)) rv]))
  (define (set-lv ast x)
    (match ast
      [(ExprStat (AssignExpr lv rv))
       (ExprStat (AssignExpr x rv))]))
  (define (set-rv ast x)
    (match ast
      [(ExprStat (AssignExpr lv rv))
       (ExprStat (AssignExpr lv x))]))
  (define-view AssignStat
    ([#:access lv get-lv set-lv]
     [#:access rv get-rv set-rv])
    #:partial)
  (define-ast AssignExpr () ([#:just lv] [#:just rv])) ;; as in C++
  (define (assign? ast)
    (AssignExpr? (ExprStat-e ast)))
  (define-ast ExprStat
    ([AssignStat (#:predicate assign?)])
    ([#:just e]))
  (check-false (AssignStat? the-Thing))
  (check-false (AssignStat? (ExprStat the-Thing)))
  (define x (Var 'x))
  (define y (Var 'y))
  (check-true (AssignStat? (ExprStat (AssignExpr x y))))
  (check-equal?
   (list x y)
   (match (ExprStat (AssignExpr x y))
     [(AssignStat lv rv) (list lv rv)]))
  (void))
