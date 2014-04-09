#lang racket/base

#|

Data types used for internal representation of abstract syntax.

It is rather important for all Ast derived node types to be
#:transparent, as such is assumed by some of the compiler machinery.

|#

(require "ast-util.rkt" "ast-view.rkt"
         "app-util.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         racket/contract racket/dict racket/function racket/match
         syntax/id-table)

;;; 
;;; abstract nodes
;;; 

(define-view* Ast #:fields (annos))
(define-view* Type #:fields ())
(define-view* Def #:fields (id))

;;; 
;;; annotations
;;; 

(define-syntax-rule* (preserve-annos v b ...)
  (let ((a (Ast-annos v)))
    (let ((v (begin b ...)))
      (set-Ast-annos v a))))

(define* (ast-anno-set ast k v)
  (define annos (Ast-annos ast))
  (set-Ast-annos ast (hash-set annos k v)))

(define* (ast-anno-must ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k)))

(define* (ast-anno-maybe ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k #f)))

(define* (annoless typ . arg*)
  (apply typ #hasheq() arg*))

(define* (syntaxed stx typ . arg*)
  (apply typ (hasheq 'stx stx) arg*))

(define* (self-syntaxed typ stx)
  (syntaxed stx typ stx))

(define* (ast-annotated ast typ . arg*)
  (apply typ (Ast-annos ast) arg*))

;;; 
;;; original syntax, display, and reporting
;;; 

(define-with-contract*
  (-> Ast? (or/c syntax? #f))
  (ast-stx ast)
  (define stx (ast-anno-maybe ast 'stx))
  ;;(when stx (writeln `(origin ,stx ,(syntax-property stx 'origin))))
  stx)

(define-with-contract*
  (-> (or/c syntax? Ast?) (or/c syntax? Ast?))
  (ast-displayable ast)
  (if (syntax? ast)
      ast
      (or (ast-stx ast) ast)))

(define-with-contract*
  (-> (or/c syntax? Ast?) any/c)
  (ast-displayable/datum ast)
  (cond
   ((syntax? ast) (syntax->datum ast))
   ((ast-stx ast) => syntax->datum)
   (else ast)))

(define-with-contract*
  (-> (or/c syntax? Ast?) (or/c symbol? #f))
  (form-get-name/ast x)
  (cond
   ((syntax? x)
    (form-get-name x))
   (else
    (define stx (ast-stx x))
    (or (and stx (form-get-name stx))
        (struct-symbol x)))))

(define* not-magnolisp-message
  "incorrect Magnolisp")

(define-with-contract*
  (->* (string?)
       ((or/c Ast? syntax? #f) (or/c Ast? syntax? #f)
        (listof (or/c Ast? syntax?))
        #:fields (listof list?)
        #:continued (or/c string? (listof string?))
        #:name (or/c symbol? #f))
       any)
  (raise-language-error/ast message
                            [ast #f]
                            [sub-ast #f]
                            [extra-sources null]
                            #:fields [more-fields null]
                            #:continued [continued not-magnolisp-message]
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
                        #:continued continued))

;;; 
;;; identifiers
;;; 

(define (id-write v out mode)
  (fprintf out "~a.~a" (Id-name v) (Id-bind v)))

;; [name symbol?] is the name of the identifier. [bind symbol?] is
;; used for comparison with other identifiers, and solely determines
;; if two identifiers access the same binding.
(define-ast* Id (Ast) ((no-term annos) (no-term name) (no-term bind))
  #:custom-write id-write)

(define-with-contract*
  (-> Id? Id? boolean?)
  (ast-identifier=? x y)
  (eq? (Id-bind x) (Id-bind y)))

(define-with-contract*
  (-> Id? string?)
  (ast-identifier->string x)
  (symbol->string (Id-name x)))

(define* (ast-identifier<? x y)
  (string<? (ast-identifier->string x)
            (ast-identifier->string y)))

;; Creates a fresh identifier with the specified basename 'sym' that
;; is not (yet) ast-identifier=? to any other.
(define-with-contract*
  (->* () ((or/c symbol? string?)) Id?)
  (fresh-ast-identifier [sym 'g])
  (annoless Id sym (gensym sym)))

;; Converts the specified syntax object identifier to an Id one,
;; making it ast-identifier=? to 'other' (if any is given).
(define-with-contract*
  (->* (identifier?) (#:bind (or/c symbol? Id?)) Id?)
  (identifier->ast id #:bind [other #f])
  (define name (syntax-e id))
  (define bind (cond
                ((symbol? other) other)
                ((Id? other) (Id-bind other))
                (else (gensym name))))
  (Id (hasheq 'stx id) name bind))

(define* (set-Id-bind id bind)
  (struct-copy Id id [bind bind]))

;; Looks up a definition corresponding to the specified identifier.
(define-with-contract*
  (-> hash? Id? (or/c Def? #f))
  (ast-identifier-lookup bind->def id)
  (hash-ref bind->def (Id-bind id) #f))

;; Stores a definition corresponding to the specified Identifier.
(define-with-contract*
  (-> hash? Id? Def? hash?)
  (ast-identifier-put bind->def id def)
  (hash-set bind->def (Id-bind id) def))

(define-with-contract*
  (-> hash? procedure? void?)
  (defs-for-each-def/Id defs f)
  (for (((base def) (in-dict defs)))
    (f def)))

(define-with-contract*
  (-> hash? procedure? hash?)
  (defs-map-each-def/Id defs rw)
  (for/dict
   #hasheq()
   (((base def) (in-dict defs)))
   (values base (rw def))))

;;; 
;;; type expressions
;;; 

(define-ast* AnyT (Ast Type) ((no-term annos)) #:singleton (#hasheq()))

(define-ast* VarT (Ast Type) ((no-term annos) (no-term sym)))

;; 'id' is an ID
(define-ast* NameT (Ast Type) ((no-term annos) (no-term id)))

;; 'ats' are the param types, and 'rt' is the return type
(define-ast* FunT (Ast Type) ((no-term annos)
                              (list-of-term ats) (just-term rt)))

;; C++ only;
;; 'id' is an ID
(define-ast* CxxNameT (Ast Type) ((no-term annos) (no-term id)))

;; C++ only
(define-ast* ConstT (Ast Type) ((no-term annos) (just-term t)))

;; C++ only
(define-ast* RefT (Ast Type) ((no-term annos) (just-term t)))

;;; 
;;; definitions
;;; 

;; Any recorded annotations from definitions are put into 'annos' from
;; the id-table. At least all the global defs will be given a [top
;; boolean?] annotation, which specifies whether a definition is
;; top-level.

;; Variable definition.
(define-ast* DefVar (Ast Def) ((no-term annos) (no-term id)
                               (just-term t) (just-term body)))

;; Function parameter declaration.
(define-ast* Param (Ast Def) ((no-term annos) (no-term id) (just-term t)))

;; Function declaration. 't' is the function type.
(define-ast* Defun (Ast Def) ((no-term annos) (no-term id)
                              (just-term t) (list-of-term params)
                              (just-term body)))

;; 't' is a Magnolisp type expression.
(define-ast* TypeAlias (Ast Def) ((no-term annos) (no-term id) (just-term t)))

;; 'cxx-t' is a C++ type expression.
(define-ast* ForeignTypeDecl (Ast Def) ((no-term annos) (no-term id)
                                        (just-term cxx-t)))

(define-ast* Unresolved (Ast) ((no-term annos)) #:singleton (#hasheq()))

;;; 
;;; other Magnolisp
;;; 

(define-ast* CompilationUnit (Ast) ((no-term annos) (list-of-term lst))) 

;; For functions with no Magnolisp body.
(define-ast* NoBody (Ast) ((no-term annos)))

;; 'def' contains a DefVar term. 'let-kind annotation has either
;; 'let-values or 'letrec-values or 'letrec-syntaxes+values; we mostly
;; do not care, since Racket has done name resolution.
(define-ast* LetStat (Ast) ((no-term annos) (just-term def) (list-of-term ss)))

;; We only allow a limited form of 'let' expressions. There is a
;; 'let-kind annotation.
(define-ast* LetExpr (Ast) ((no-term annos) (just-term def) (just-term e)))

;; Sequence of statements.
(define-ast* BlockStat (Ast) ((no-term annos) (list-of-term ss)))

;; Variable reference.
(define-ast* Var (Ast) ((no-term annos) (no-term id)))

;; Function value.
(define-ast* Lambda (Ast) ((no-term annos) (list-of-term params) (just-term body)))

;; Assignment.
(define-ast* Assign (Ast) ((no-term annos) (just-term lv) (just-term rv)))

;; If expression.
(define-ast* IfExpr (Ast) ((no-term annos) (just-term c) (just-term t) (just-term e)))

;; If statement.
(define-ast* IfStat (Ast) ((no-term annos) (just-term c) (just-term t) (just-term e)))

;; A literal datum.
(define-ast* Literal (Ast) ((no-term annos) (no-term datum)))

;; Function (or predicate) application, with function expression, and
;; argument expressions.
(define-ast* Apply (Ast) ((no-term annos) (just-term f) (list-of-term args)))

;; Procedure call. A statement.
(define-ast* Call (Ast) ((no-term annos) (just-term f) (list-of-term args)))

;; Transient. Corresponds to a let/ec that only escapes to a local,
;; immediately surrounding call/cc continuation. 'k' is a label (an
;; ID) naming the continuation.
(define-ast* LetLocalEc (Ast) ((no-term annos) (just-term k) (list-of-term ss)))

;; Escapes to the named LetLocalEc continuation 'k' (an ID) with the
;; value given by expression 'e'.
(define-ast* AppLocalEc (Ast) ((no-term annos) (just-term k) (just-term e)))

;; Label, either a binding or use context.
(define-ast* Label (Ast) ((no-term annos) (no-term id)))

;; Block expression. Contains statements.
(define-ast* BlockExpr (Ast) ((no-term annos) (list-of-term ss)))

;; Return statement. For now we only support single value returns. The
;; semantics are to escape from a surrounding BlockExpr.
(define-ast* Return (Ast) ((no-term annos) (just-term e)))

(define-ast* RacketExpr (Ast) ((no-term annos)))

;;; 
;;; C++
;;;

;; kind is either 'user or 'system.
(define-ast* Include (Ast) ((no-term annos) (no-term kind) (no-term s)))

;; 'rtype' is the return type, only.
(define-ast* CxxDefun (Ast Def) ((no-term annos) (no-term id)
                                 (no-term modifs) (just-term rtype)
                                 (list-of-term params) (list-of-term ss)))

;; A C++ function prototype declaration. No body, and some modifiers
;; may have to be different to the function definition.
(define-ast* Proto (Ast Def) ((no-term annos) (no-term id)
                              (no-term modifs) (just-term rtype)
                              (list-of-term params)))

(define-ast* CxxReturnNone (Ast) ((no-term annos)))

(define-ast* CxxReturnOne (Ast) ((no-term annos) (just-term e)))

(define-ast* CxxIfSugar (Ast) ((no-term annos) (just-term c) (just-term t)))

(define-ast* CxxParam (Ast Def) ((no-term annos) (no-term id)
                                 (just-term t)))

(define-ast* CxxDeclVar (Ast Def) ((no-term annos) (no-term id)
                                   (just-term t)))

;; Statement expression (GCC extension).
(define-ast* GccStatExpr (Ast) ((no-term annos) (list-of-term ss) (just-term e)))

;; Local label declaration (GCC extension). A statement.
(define-ast* GccLabelDecl (Ast) ((no-term annos) (no-term n)))

;; Label for the following statements. Itself a statement.
(define-ast* CxxLabel (Ast) ((no-term annos) (no-term n)))

;; Where 'n' is a label. A statement.
(define-ast* Goto (Ast) ((no-term annos) (no-term n)))

;; Parenthesized expression.
(define-ast* Parens (Ast) ((no-term annos) (just-term e)))

(define-ast* UnaryOp (Ast) ((no-term annos) (no-term op) (just-term e)))
(define-ast* BinaryOp (Ast) ((no-term annos) (no-term op) (just-term e1) (just-term e2)))
(define-ast* TrinaryOp (Ast) ((no-term annos) (no-term op) (just-term e1)
                              (just-term e2) (just-term e3)))

;; Top-level verbatim string.
(define-ast* TlVerbatim (Ast) ((no-term annos) (no-term s)))

;;; 
;;; Id replacing AST rewrite
;;;

(define* (ast-rw-Ids rw-id ast)
  (define (rw-annos annos)
    (define type-ast (hash-ref annos 'type-ast #f))
    (and type-ast
         (hash-set annos 'type-ast (rw type-ast))))

  (define (ast-rw-annos ast)
    (define annos (rw-annos (Ast-annos ast)))
    (if annos (set-Ast-annos ast annos) ast))
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((? Def?)
          (define id (Def-id ast))
          (define id-ast (rw-id id))
          (Def-copy ast id-ast))
         ((Var a id)
          (define id-ast (rw-id id))
          (Var (or (rw-annos a) a) id-ast))
         ((NameT a id)
          (define id-ast (rw-id id))
          (NameT a id-ast))
         ((Label a id)
          (define id-ast (rw-id id))
          (Label a id-ast))
         (else
          (ast-rw-annos ast))))))

  (rw ast))

;;; 
;;; tests
;;; 

(module+ test
  (require rackunit)
  (let ((ast (annoless Var (fresh-ast-identifier))))
    (check-true (Var? ast))
    (check-true (Ast? ast))
    (check-true (Id? (Var-id ast)))
    (check-true (hash? (Ast-annos ast)))
    (check-true (Ast? (set-Ast-annos ast #hasheq())))
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
    (check-exn #rx"singleton" (thunk (set-Ast-annos ast #hasheq())))
    (check-exn #rx"singleton" (thunk (Ast-copy ast #hasheq())))))
