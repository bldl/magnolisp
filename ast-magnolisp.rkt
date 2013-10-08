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

;; For more compact printing, we do not make annotations transparent.
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

(define-ast* AnyT Type () #:singleton (#hasheq()))

;; 'name' is a symbol
(define-ast* TypeName Type ((no-term name)))

;; 'ats' are the param types, and 'rt' is the return type
(define-ast* FunT Type ((list-of-term ats) (just-term rt)))

;;; 
;;; others
;;; 

;; Any recorded annotations from definitions are put into 'annos' from
;; the bound-id-table. All global defs (and only them) have an [r-mp
;; resolve-module-path-result?] annotation.
(abstract-struct* Def Ast
  (id) ;; syntax?
  #:transparent)

;; Variable definition.
(define-ast* DefVar Def ((just-term body)))

;; Function parameter declaration.
(define-ast* Param Def ())

;; 'defs' contains DefVar terms. Said bindings are not visible in
;; DefVar bodies.
(define-ast* Let Ast ((list-of-term defs) (list-of-term body)))

;; 'defs' contains DefVar terms. Said bindings are visible in DefVar
;; bodies.
(define-ast* Letrec Ast ((list-of-term defs) (list-of-term body)))

;; Sequence of statements.
(define-ast* Begin Ast ((list-of-term body)))

;; Variable reference.
(define-ast* Var Ast ((no-term id)))

;; Function value.
(define-ast* Lambda Ast ((list-of-term params) (list-of-term body)))

;; Assignment.
(define-ast* Assign Ast ((just-term lv) (just-term rv)))

;; If expression.
(define-ast* IfExpr Ast ((just-term c) (just-term t) (just-term e)))

;; A literal datum.
(define-ast* Literal Ast ((no-term datum)))

;; Function application, with function expression, and argument
;; expressions.
(define-ast* Apply Ast ((just-term f) (list-of-term args)))

#|

(define-ast* Verbatim Ast ((no-term text)))
(define-ast* Pass Ast ())
(define-ast* Call Ast ((just-term proc)))

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

(define* (Var-rename ast n)
  (struct-copy Var ast (name n)))

|#
