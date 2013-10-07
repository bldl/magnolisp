#lang racket

#|

Data types used for internal representation of abstract syntax. The
frontend creates (syntax for) such values in phase 1, whereas the
backend uses the values in phase 0.

We use a bound-id-table for recording information about identifiers
during macro expansion. The key being the ID, and the value being a
hasheq, keyed by the following:

  - 'type :: a Type for each declared typed construct

  - 'entry-point :: a #t value for each library entry point

The 'verbatim and 'external property of an operation body is encoded
in the AST, and no table is required for that information. This really
is a property of the body, and not of the signature.

  - 'external means an externally defined function (in C++)

  - 'verbatim means that body is foreign language (C++)

The reason we are using a bound id-table is that whenever we are
adding a metadata record it is for a freshly bound definition - we
never add a new metadata record for the same identifier.

Note quite sure if we should specify any #:phase for the identifier
tables, but phase 0 would seem appropriate as all Magnolisp names are
such.

|#

(require "ast-util.rkt" "util.rkt")

;;; 
;;; abstract node
;;; 

(abstract-struct* Ast (annos))

(define* (Ast-anno-ref ast k #:must (must #t))
  (let* ((annos (Ast-annos ast)))
    (if must
        (hash-ref annos k)
        (hash-ref annos k #f))))

;;; 
;;; type expressions
;;; 

(abstract-struct* Type Ast ())

(define-values (struct:AnyT make-AnyT AnyT? AnyT-ref AnyT-set!)
  (make-struct-type 'AnyT struct:Type 0 0))
(define AnyT (make-AnyT #hasheq()))
(provide AnyT AnyT?)

;; 'name' is a symbol
(define-ast* TypeName Type ((no-term name)))

;; 'ats' are the param types, and 'rt' is the return type
(define-ast* FunT Type ((list-of-term ats) (just-term rt)))

;;; 
;;; others
;;; 

;; Any recorded annotations from definitions are put into 'annos' from
;; the bound-id-table.
(define-ast* Def Ast
  ((no-term id) ;; syntax?
   (just-term body)
   (no-term rmp) ;; resolved-module-path?
   (no-term outer))) ;; (listof syntax?), IDs of outer defs

;; Null body.  xxx could use a convenience for defining singleton Ast nodes
(define-ast* NoBody Ast ())
(define* the-NoBody (NoBody #hasheq()))

;; Sequence of statements.
(define-ast* Begin Ast ((list-of-term body)))

;; Variable reference.
(define-ast* Var Ast ((no-term id)))

;; Function value.
(define-ast* Lambda Ast ((list-of-term params) (list-of-term body)))

;; Function parameter.
(define-ast* Param Def ())

;; Assignment.
(define-ast* Assign Ast ((just-term lv) (just-term rv)))

;; If expression.
(define-ast* IfExpr Ast ((just-term c) (just-term t) (just-term e)))

#|

(define-ast* Literal Ast ((no-term datum)))
(define-ast* Verbatim Ast ((no-term text)))
(define-ast* Pass Ast ())
(define-ast* Call Ast ((just-term proc)))

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

(define* (Var-rename ast n)
  (struct-copy Var ast (name n)))

|#
