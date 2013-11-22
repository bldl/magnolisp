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
  (-> Ast? (or/c syntax? Ast?))
  (Ast-displayable ast)
  (or (Ast-stx ast) ast))

(define* not-magnolisp-message
  "incorrect Magnolisp")

(define-with-contract*
  (->* (string?)
       ((or/c Ast? syntax? #f) (or/c Ast? syntax? #f)
        (listof (or/c Ast? syntax?))
        #:name (or/c symbol? #f))
       any)
  (raise-language-error/ast message
                            [ast #f]
                            [sub-ast #f]
                            [extra-sources null]
                            #:name [fallback-name #f])
  (define name (or (and (Ast? ast) (struct-symbol ast))
                   (and (Ast? sub-ast) (struct-symbol sub-ast))
                   fallback-name))
  (define expr (Ast-displayable ast))
  (define sub-expr (Ast-displayable sub-ast))
  (define extras (filter values (map Ast-stx extra-sources)))
  (raise-language-error name message
                        expr sub-expr extras
                        #:continued not-magnolisp-message))

;;; 
;;; type expressions
;;; 

(abstract-struct* Type Ast () #:transparent)

(define-ast* AnyT Type () #:singleton (#hasheq()))

;; 'name' is a symbol
(define-ast* TypeName Type ((no-term name)))

;; 'ats' are the param types, and 'rt' is the return type
(define-ast* FunT Type ((list-of-term ats) (just-term rt)))

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
(define-ast* DefVar Def ((just-term body)))

;; Syntax definition.
(define-ast* DefStx Def ())

;; Function parameter declaration.
(define-ast* Param Def ())

;; Function declaration.
(define-ast* Defun Def ((list-of-term params) (just-term body)))

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

(define-ast* CxxDefun Def ((no-term modifs) (just-term rtype) (list-of-term params) (list-of-term ss)))

(define-ast* CxxReturnNone Ast ())

(define-ast* CxxReturnOne Ast ((just-term e)))

(define-ast* CxxParam Def ((just-term t)))

;; Parenthesized expression.
(define-ast* Parens Ast ((just-term e)))

(define-ast* UnaryOp Ast ((no-term op) (just-term e)))
(define-ast* BinaryOp Ast ((no-term op) (just-term e1) (just-term e2)))
(define-ast* TrinaryOp Ast ((no-term op) (just-term e1) (just-term e2) (just-term e3)))

;;; 
;;; sexp dumping
;;; 

(define (->symbol x)
  (cond
   ((symbol? x) x)
   ((identifier? x) (syntax-e x))
   (else (unsupported x))))

(define* (ast->sexp ast)
  (match ast
    ((DefVar _ id v)
     `(var ,(->symbol id) ,(ast->sexp v)))
    ((DefStx _ id)
     `(define-syntax ,(->symbol id)))
    ((Param _ id)
     (->symbol id))
    ((Defun a id ps b)
     (define export? (hash-ref a 'export #f))
     (define n (if export? 'function/export 'function))
     `(,n (,(->symbol id) ,@(map ast->sexp ps))
          ,(ast->sexp b)))
    ((or (Let a ds bs)
         (Letrec a ds bs))
     (define n (if (Let? ast) 'let 'letrec))
     `(,n (,@(map ast->sexp ds))
          ,@(map ast->sexp bs)))
    ((Begin _ bs)
     `(begin ,@(map ast->sexp bs)))
    ((Var _ id)
     (->symbol id))
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
    (_
     (unsupported ast))))

(define* (ast-pp ast)
  (pretty-print (ast->sexp ast)))
