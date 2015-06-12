#lang racket

#|
|#

(require (relative-in magnolisp
                      "ast-magnolisp.rkt" "ast-repr.rkt")
         racket/generic rackunit)

(let ((ast (annoless Var (fresh-ast-identifier))))
  (check-true (Var? ast))
  (check-true (Ast? ast))
  (check-true (Id? (Var-id ast)))
  (check-true (hash? (Ast-annos ast)))
  (check-true (Ast? (set-Ast-annos ast #hasheq())))
  (check-false (Expr-type ast))
  (let ((a (Ast-annos ast)))
    (check-eq? a (Ast-annos (set-Ast-annos ast a))))
  (let ((a #hasheq((foo . bar))))
    (check-eq? a (Ast-annos (set-Ast-annos ast a)))
    (check-eq? a (Ast-annos (Ast-copy ast a)))))

(let ((ast (annoless Param (fresh-ast-identifier)
                     (annoless NameT (fresh-ast-identifier 't)))))
  (check-true (Param? ast))
  (check-true (Def? ast))
  (check-true (Ast? ast))
  (check-true (Id? (Def-id ast)))
  (let* ((id (fresh-ast-identifier))
         (n-ast (set-Def-id ast id)))
    (check-pred Param? n-ast)
    (check-pred Id? (Def-id n-ast))
    (check-not-eq? (Def-id ast) (Def-id n-ast)))
  (let* ((id (fresh-ast-identifier))
         (n-ast (Def-copy ast id)))
    (check-pred Param? n-ast)
    (check-pred Id? (Def-id n-ast))
    (check-not-eq? (Def-id ast) (Def-id n-ast))))

(let ((ast the-AnyT))
  (check-pred AnyT? ast)
  (check-pred Ast? ast)
  (check-pred hash? (AnyT-annos ast))
  (check-pred hash? (Ast-annos ast))
  (check-not-eq? ast (Ast-copy ast #hasheq()))
  (check-equal? ast (Ast-copy ast #hasheq((foo . bar))))
  (check-not-exn (thunk (set-Ast-annos ast #hasheq())))
  (check-not-exn (thunk (Ast-copy ast #hasheq()))))
