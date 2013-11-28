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

The 'verbatim and 'external property:

  - 'external means an externally defined function (in C++)

  - 'verbatim means that body is foreign language (C++)

The reason we are using a bound id-table is that whenever we are
adding a metadata record it is for a freshly bound definition - we
never add a new metadata record for the same identifier.

Note quite sure if we should specify any #:phase for the identifier
tables, but phase 0 would seem appropriate as all Magnolisp names are
such.

It is rather important for all Ast derived node types to be
#:transparent.

|#

(require "ast-util.rkt" "compiler-util.rkt"
         "util.rkt" "util/struct.rkt")

;;; 
;;; abstract node
;;; 

(define-ast-base* Ast)

(define* (annoless typ . arg*)
  (apply typ #hasheq() arg*))

(define* (syntaxed stx typ . arg*)
  (apply typ (hasheq 'stx stx) arg*))

(define* (ast-anno-must ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k)))

(define* (ast-anno-maybe ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k #f)))

(define* (Ast-anno-set ast k v)
  (define old-annos (Ast-annos ast))
  (define new-annos (hash-set old-annos k v))
  (ast-set-annos ast new-annos))

(define-with-contract*
  (-> Ast? (or/c syntax? #f))
  (Ast-stx ast)
  (define stx (ast-anno-maybe ast 'stx))
  ;;(when stx (writeln `(origin ,stx ,(syntax-property stx 'origin))))
  stx)

(define-with-contract*
  (-> (or/c syntax? Ast?) (or/c syntax? Ast?))
  (ast-displayable ast)
  (if (syntax? ast)
      ast
      (or (Ast-stx ast) ast)))

(define-with-contract*
  (-> (or/c syntax? Ast?) any/c)
  (ast-displayable/datum ast)
  (cond
   ((syntax? ast) (syntax->datum ast))
   ((Ast-stx ast) => syntax->datum)
   (else ast)))

(define-with-contract*
  (-> (or/c syntax? Ast?) (or/c symbol? #f))
  (form-get-name/ast x)
  (cond
   ((syntax? x)
    (form-get-name x))
   (else
    (define stx (Ast-stx x))
    (or (and stx (form-get-name stx))
        (struct-symbol x)))))

(define* not-magnolisp-message
  "incorrect Magnolisp")

(define-with-contract*
  (->* (string?)
       ((or/c Ast? syntax? #f) (or/c Ast? syntax? #f)
        (listof (or/c Ast? syntax?))
        #:fields (listof list?)
        #:name (or/c symbol? #f))
       any)
  (raise-language-error/ast message
                            [ast #f]
                            [sub-ast #f]
                            [extra-sources null]
                            #:fields [more-fields null]
                            #:name [fallback-name #f])
  (define name (or (and ast (form-get-name/ast ast))
                   (and sub-ast (form-get-name/ast sub-ast))
                   fallback-name))
  (define expr (and ast (ast-displayable ast)))
  (define sub-expr (and sub-ast (ast-displayable sub-ast)))
  (define extras (filter values (map ast-displayable extra-sources)))
  (raise-language-error name message
                        expr sub-expr extras
                        #:fields more-fields
                        #:continued not-magnolisp-message))

;;; 
;;; type expressions
;;; 

(abstract-struct* Type Ast () #:transparent)

(define-ast* AnyT Type () #:singleton (#hasheq()))

;; 'id' is an ID
(define-ast* NameT Type ((no-term id)))

;; these are always computed;
;; for these, 'id' is also 'def-id'
(define-ast* DefNameT NameT ())

;; 'ats' are the param types, and 'rt' is the return type
(define-ast* FunT Type ((list-of-term ats) (just-term rt)))

;; C++ only;
;; 'id' is an ID
(define-ast* CxxNameT Type ((no-term id)))

;; C++ only
(define-ast* ConstT Type ((just-term t)))

;; C++ only
(define-ast* RefT Type ((just-term t)))

;;; 
;;; definitions
;;; 

;; Any recorded annotations from definitions are put into 'annos' from
;; the bound-id-table. All defs have an [r-mp
;; resolve-module-path-result?] annotation, specifying the defining
;; module. At least all the global defs have a [top boolean?]
;; annotation, which specifies whether a definition is top-level.
(abstract-struct* Def Ast
  (id) ;; syntax?
  #:transparent)

;; Variable definition.
(define-ast* DefVar Def ((just-term t) (just-term body)))

;; Syntax definition.
(define-ast* DefStx Def ())

;; Function parameter declaration.
(define-ast* Param Def ((just-term t)))

;; Function declaration. 't' is the function type.
(define-ast* Defun Def ((just-term t) (list-of-term params) (just-term body)))

;; 't' is a Magnolisp type expression.
(define-ast* TypeAlias Def ((just-term t)))

;; 'cxx-t' is a C++ type expression.
(define-ast* ForeignTypeDecl Def ((just-term cxx-t)))

;;; 
;;; other Magnolisp
;;; 

;; 'defs' contains DefVar terms. Said bindings are not visible in
;; DefVar bodies.
(define-ast* Let Ast ((list-of-term defs) (list-of-term body)))

;; 'defs' contains DefVar terms. Said bindings are visible in DefVar
;; bodies.
(define-ast* Letrec Ast ((list-of-term defs) (list-of-term body)))

;; Sequence of expressions (as in Racket).
(define-ast* Begin Ast ((list-of-term body)))

;; Variable reference.
(define-ast* Var Ast ((no-term id)))

;; Function value.
(define-ast* Lambda Ast ((list-of-term params) (just-term body)))

;; Assignment.
(define-ast* Assign Ast ((just-term lv) (just-term rv)))

;; If expression.
(define-ast* IfExpr Ast ((just-term c) (just-term t) (just-term e)))

;; A literal datum.
(define-ast* Literal Ast ((no-term datum)))

;; Function (or predicate) application, with function expression, and
;; argument expressions.
(define-ast* Apply Ast ((just-term f) (list-of-term args)))

;; Procedure call.
(define-ast* Call Ast ((just-term f) (list-of-term args)))

;; Nil statement.
(define-ast* Pass Ast ())

;; Return statement.
(define-ast* Return Ast ((list-of-term es)))

#|

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

(define* (Var-rename ast n)
  (struct-copy Var ast (name n)))

|#

;;; 
;;; C++
;;;

;; kind is either 'user or 'system.
(define-ast* Include Ast ((no-term kind) (no-term s)))

;; 'rtype' is the return type, only.
(define-ast* CxxDefun Def ((no-term modifs) (just-term rtype) (list-of-term params) (list-of-term ss)))

;; A C++ function prototype declaration. No body, and some modifiers
;; may have to be different to the function definition.
(define-ast* Proto Def ((no-term modifs) (just-term rtype) (list-of-term params)))

(define-ast* CxxReturnNone Ast ())

(define-ast* CxxReturnOne Ast ((just-term e)))

(define-ast* CxxParam Def ((just-term t)))

;; Parenthesized expression.
(define-ast* Parens Ast ((just-term e)))

(define-ast* UnaryOp Ast ((no-term op) (just-term e)))
(define-ast* BinaryOp Ast ((no-term op) (just-term e1) (just-term e2)))
(define-ast* TrinaryOp Ast ((no-term op) (just-term e1) (just-term e2) (just-term e3)))

;; Top-level verbatim string.
(define-ast* TlVerbatim Ast ((no-term s)))

;;; 
;;; definition IDs
;;; 

(define-with-contract*
  (-> any/c (or/c syntax? #f))
  (get-def-id x)
  (cond
   ((identifier? x)
    (syntax-property x 'def-id))
   ((Def? x)
    (get-def-id (Def-id x)))
   ((Var? x)
    (get-def-id (Var-id x)))
   ((DefNameT? x)
    (NameT-id x))
   ((NameT? x)
    (get-def-id (NameT-id x)))
   (else
    (raise-argument-error
     'get-def-id
     "(or/c identifier? Def? Var? NameT?)"
     x))))

(define-with-contract*
  (-> any/c syntax? any/c)
  (set-def-id x def-id)
  (cond
   ((identifier? x)
    (syntax-property x 'def-id def-id))
   ((Def? x)
    (define id (set-def-id (Def-id x) def-id))
    (dynamic-struct-copy Def x (id id)))
   ((Var? x)
    (define id (set-def-id (Var-id x) def-id))
    (struct-copy Var x (id id)))
   ((NameT? x)
    (define id (set-def-id (NameT-id x) def-id))
    (struct-copy NameT x (id id)))
   (else
    (raise-argument-error
     'set-def-id
     "(or/c identifier? Def? Var? NameT?)"
     0 x def-id))))

(define* (get-def-id-or-fail x)
  (define def-id (get-def-id x))
  (unless def-id
    (raise-language-error/ast
     "reference to unbound name" x))
  def-id)

;;; 
;;; names
;;; 

(define* (name-ref? ast)
  (or (Var? ast) (NameT? ast)))

(define-with-contract*
  (-> Ast? identifier?)
  (name-ref-id ast)
  (cond
   ((Var? ast) (Var-id ast))
   ((NameT? ast) (NameT-id ast))
   (else (unsupported ast))))

;;; 
;;; exports
;;; 

(define* (actual-export? x)
  (cond
   ((hash? x) (hash-ref x 'actual-export #f))
   ((Def? x) (ast-anno-maybe x 'actual-export))
   (else
    (raise-argument-error
     'actual-export?
     "(or/c hash? Def?)" x))))

;;; 
;;; sexp dumping
;;; 

(define (->symbol x)
  (cond
   ((symbol? x) x)
   ((identifier? x) (syntax-e x))
   (else (unsupported x))))

(define (?->symbol x)
  (and x (->symbol x)))

(define* show-bindings? (make-parameter #f))

(define* (ast->sexp ast)
  (match ast
    ((DefVar _ id t v)
     `(var ,(->symbol id) ,(ast->sexp t) ,(ast->sexp v)))
    ((DefStx _ id)
     `(define-syntax ,(->symbol id)))
    ((Param _ id t)
     (->symbol id))
    ((Defun a id t ps b)
     (define export? (hash-ref a 'export #f))
     `(function (,(->symbol id) ,@(map ast->sexp ps))
        #:annos ((type ,(ast->sexp t))
                 (export ,export?))
        ,(ast->sexp b)))
    ((or (Let a ds bs)
         (Letrec a ds bs))
     (define n (if (Let? ast) 'let 'letrec))
     `(,n (,@(map ast->sexp ds))
          ,@(map ast->sexp bs)))
    ((Begin _ bs)
     `(begin ,@(map ast->sexp bs)))
    ((Var _ id)
     (if (show-bindings?)
         `(Var ,(->symbol id) ~> ,(?->symbol (get-def-id id)))
         (->symbol id)))
    ((Lambda _ ps b)
     `(lambda 
          (,@(map ast->sexp ps))
        ,(ast->sexp b)))
    ((Assign _ lv rv)
     `(set! ,(ast->sexp lv) ,(ast->sexp rv)))
    ((IfExpr _ c t e)
     `(if ,(ast->sexp c) ,(ast->sexp t) ,(ast->sexp e)))
    ((Literal _ d)
     (syntax->datum d))
    ((Apply _ f as)
     `(,(ast->sexp f) ,@(map ast->sexp as)))
    ((NameT _ id)
     (if (show-bindings?)
         `(NameT ,(->symbol id) ~> ,(?->symbol (get-def-id id)))
         (->symbol id)))
    ((FunT _ ats rt)
     `(fn ,@(map ast->sexp ats) ,(ast->sexp rt)))
    ((CxxNameT _ id)
     `(C++ ,(->symbol id)))
    ((AnyT _)
     'unresolved)
    ((ForeignTypeDecl _ id cxx-t)
     `(typedef ,(->symbol id) ,(ast->sexp cxx-t)))
    (_
     (raise-argument-error
      'ast->sexp "supported Ast?" ast))))

(define* (ast-pp ast)
  (pretty-print (ast->sexp ast)))
