#lang racket

#|
|#

(require (relative-in magnolisp
                      "ast-repr.rkt" "ast-view.rkt"
                      "strategy.rkt" "strategy-stratego.rkt")
         racket/generic rackunit)

;; A list-based `all`.
(let ()
  (define l-all (make-strategy list-rewrite-all))
  (check-equal?
   ((l-all add1) '(1 2 3))
   '(2 3 4)))

;; View-based traversal.
(let ()
  (define-view V ([#:field #:many vs]) #:traversable)
  (define-ast A (V) ([#:none val] [#:many cs] [#:many vs]))
  
  (define (modify ast)
    (set-A-val ast (add1 (A-val ast))))

  (define V-all (make-view-all V))

  (define (sum ast)
    (define r 0)
    ((topdown-visitor (lambda (ast)
                        (set! r (+ r (A-val ast)))))
     ast)
    r)

  (define V-rw (topdown modify V-all))
  
  (define ast0 ;; 0
    (A 0 null null))

  (define ast1 ;; 1
    (V-rw ast0))

  (define ast2 ;; 7
    (A 5 (list ast0 ast1) (list ast1)))

  (define ast3 ;; 17
    (A 2 (list ast2 ast1) (list ast2)))
  
  (check-eqv? (sum ast0) 0)
  (check-eqv? (sum ast1) 1)
  (check-eqv? (sum (V-rw ast1)) 2)
  (check-eqv? (sum (V-rw ast2)) 9)
  (check-eqv? (sum ast3) 17)
  (check-eqv? (sum (V-rw ast3)) (+ 3 (+ 7 1) 9))
  (void))

;; Combined view-based and concrete traversal.
(let ()
  (define-view Ast ([#:field #:none annos]))

  (define (get-type ast) ;; (-> Ast? (or/c Type? #f))
    (hash-ref (Ast-annos ast) 'type #f))
  (define (set-type ast t) ;; (-> Ast? (or/c Type? #f) Ast?)
    (set-Ast-annos ast (hash-set (Ast-annos ast) 'type t)))

  (define-view Expr
    ([#:access #:maybe type get-type set-type])
    #:also (Ast) #:traversable)

  (define-ast NameT (Ast)
    ([#:none annos] [#:none id]))
  
  (define-ast Lit (Expr)
    ([#:none annos] [#:none dat]))

  (define-ast Inc (Expr)
    ([#:none annos] [#:just e]))

  (define Expr-all (make-view-all Expr))
  (define (Expr+all s) (<* (when-rw Expr? (Expr-all s)) (all s)))
  (define int (NameT #hasheq() 'int))
  (define type=int (hasheq 'type int))
  (define ast0 (Lit type=int 0))
  (define ast1 (Inc type=int ast0))

  (define rw
    (match-lambda
      [(Lit a d) (Lit a (add1 d))]
      [(NameT a id) (NameT a 'long)]
      [ast ast]))

  (define rw-tree (topdown rw Expr+all))

  (define check
    (topdown-visitor
     (lambda (ast)
       (when (Lit? ast)
         (check-eqv? 1 (Lit-dat ast)))
       (when (Expr? ast)
         (define t (Expr-type ast))
         (when t
           (check t)))
       (when (NameT? ast)
         (check-eq? 'long (NameT-id ast))))))

  (define ast2 (rw-tree ast1))
  (check ast2)

  (define ast3 (Inc type=int ast1))
  (define ast4 (rw-tree ast3))
  (check ast4)
  ;;(pretty-print ast4)

  (void))
