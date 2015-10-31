#lang racket/base

#|

Data types used for internal representation of abstract syntax.

Assumptions for AST node types:

- the first field of each node is for annotations,
  and named `annos`, and declared as `#:none`

|#

(require "app-util.rkt" "ast-repr.rkt" "ast-view.rkt"
         "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         racket/contract racket/dict racket/function racket/match
         syntax/id-table
         (for-syntax racket/base racket/syntax))

;;; 
;;; abstract nodes
;;; 

(define-view* Ast (#:fields annos))
(define-view* Anno ())
(define-view* Type ())
(define-view* Def (#:fields id))
(define-view* Stat ())
(define-view* SeqCont (#:fields ss))
(define-view* If ([#:field c] [#:field t] [#:field e]))

(define-syntax-rule*
  (define-Ast-anno-accessors name get set)
  (begin
    (define (get ast)
      (hash-ref (Ast-annos ast) 'name #f))
    (define (set ast v)
      (set-Ast-annos ast (hash-set (Ast-annos ast) 'name v)))))
  
(define-syntax-rule*
  (define-Ast-anno-accessors* name get set)
  (begin
    (define-Ast-anno-accessors name get set)
    (provide get set)))

(define-Ast-anno-accessors type get-type set-type)

(define-view* Expr ([#:access #:maybe type get-type set-type])
  #:traversable)

;; Either an Expr or a Stat, without any ad-hoc members.
(define-view* ExprLike ([#:access type get-type set-type])
  #:generics-options
  (#:defaults ([Expr?
                (define (ExprLike-type ast) (get-type ast))
                (define (set-ExprLike-type ast t) (set-type ast t))
                (define (ExprLike-copy ast t) (Expr-copy ast t))]
               [Stat?
                (define (ExprLike-type ast) the-Void-type)
                (define (set-ExprLike-type ast t) (void))
                (define (ExprLike-copy ast t) ast)])))

(define* (ExprLike-set-type-from-annos ast annos)
  (define t (hash-ref annos 'type #f))
  (if (and t (not (AnyT? t)))
      (set-ExprLike-type ast t)
      ast))

(define* (annos-with-type-from-ExprLike ast)
  (define t (ExprLike-type ast))
  (assert t)
  (hasheq 'type t))

(define* (Expr-typed? ast)
  (define t (Expr-type ast))
  (and t (not (AnyT? t))))

;;; 
;;; annotations
;;; 

(define-syntax-rule* (preserve-annos v b ...)
  (let ((a (Ast-annos v)))
    (let ((v (begin b ...)))
      (set-Ast-annos v a))))

(define* (modify-ast-annos ast f)
  (let ((a (Ast-annos ast)))
    (set-Ast-annos ast (f a))))

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

(define* (annos-remove-type annos)
  (hash-remove annos 'type))

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
  (-> Ast? string?)
  (ast-~a ast)
  (let ((stx (ast-stx ast)))
    (if stx
        (format "~a" (syntax->datum stx))
        (format "~s" ast))))

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

(define (Id-write v out mode)
  (fprintf out "~a«~a»" (Id-name v) (Id-bind v)))

;; [name symbol?] is the name of the identifier. [bind symbol?] is
;; used for comparison with other identifiers, and solely determines
;; if two identifiers access the same binding.
(define-ast* Id (Ast) ((#:none annos) (#:none name) (#:none bind))
  #:custom-write Id-write)

(define-with-contract*
  (-> Id? Id? boolean?)
  (Id=? x y)
  (eq? (Id-bind x) (Id-bind y)))

(define-with-contract*
  (-> Id? string?)
  (Id->string x)
  (symbol->string (Id-name x)))

(define* (Id<? x y)
  (string<? (Id->string x)
            (Id->string y)))

;; Creates a fresh identifier with the specified basename `sym` that
;; is not (yet) Id=? to any other. An uninterned value is
;; given to `bind`.
(define-with-contract*
  (->* () ((or/c symbol? string?)) Id?)
  (fresh-Id [sym 'g])
  (annoless Id sym (gensym sym)))

(define-with-contract*
  (-> Id? Id?)
  (another-Id other)
  (fresh-Id (Id-name other)))

;; Converts the specified syntax object identifier to an Id one,
;; making it Id=? to `other` (if any is given).
(define-with-contract*
  (->* (identifier?) (#:bind (or/c symbol? Id?)) Id?)
  (identifier->Id id #:bind [other #f])
  (define name (syntax-e id))
  (define bind (cond
                ((symbol? other) other)
                ((Id? other) (Id-bind other))
                (else (gensym name))))
  (Id (hasheq 'stx id) name bind))

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
  (-> procedure? hash? hash?)
  (defs-map/bind rw defs)
  (for/hasheq ([(bind def) (in-dict defs)])
    (values bind (rw def))))

(define-with-contract*
  (-> procedure? hash? any/c)
  (defs-for-each/bind rw defs)
  (for ([(bind def) (in-dict defs)])
    (rw def)))

;;; 
;;; type expressions
;;; 

(define-ast* AnyT (Ast Type) 
  ((#:none annos)) #:singleton (#hasheq()))

(define-ast* VarT (Ast Type) 
  ((#:none annos) (#:none sym)))

;; `ns` is (listof NameT?)
(define-ast* ExistsT (Ast Type)
  ((#:none annos) (#:many ns) (#:just t)))

;; `ns` is (listof NameT?)
(define-ast* ForAllT (Ast Type)
  ((#:none annos) (#:many ns) (#:just t)))

;; Either type `t1` or `t2` (runtime determined).
(define-ast* PhiT (Ast Type) 
  ((#:none annos) (#:just t1) (#:just t2)))

;; The type of a reified continuation.
(define-ast* KontT (Ast Type)
  ((#:none annos)) #:singleton (#hasheq()))

;; `id` is an `Id` object
(define-ast* NameT (Ast Type)
  ((#:none annos) (#:none id)))

;; parameterized type `t` is always a `NameT`
(define-ast* ParamT (Ast Type)
  ((#:none annos) (#:just t) (#:many ats)))

;; `ats` are the param types, and `rt` is the return type
(define-ast* FunT (Ast Type) 
  ((#:none annos) (#:many ats) (#:just rt)))

;; `id` is a Racket identifier
(define-ast* ForeignNameT (Ast Type) 
  ((#:none annos) (#:none id)))

;; C++ only
(define-ast* ConstT (Ast Type) 
  ((#:none annos) (#:just t)))

;; C++ only
(define-ast* RefT (Ast Type) 
  ((#:none annos) (#:just t)))

;;; 
;;; annotations
;;; 

;; An expression that annotates expression `e` with annotations `as`.
(define-ast* AnnoExpr (Ast) ((#:none annos) (#:many as)
                             (#:just e)))

;; `t` is a type expression
(define-ast* TypeAnno (Ast Anno) ((#:none annos) (#:just t)))

;; `kind` is a symbol naming the annotation type, whereas `datum` is
;; its (parsed) value
(define-ast* GenericAnno (Ast Anno) ((#:none annos) (#:none kind)
                                     (#:none datum)))

;;; 
;;; definitions
;;; 

;; Any recorded annotations from definitions are put into `annos` from
;; the id-table. At least all the global defs will be given a [top
;; boolean?] annotation, which specifies whether a definition is
;; top-level.

;; Variable definition.
(define-ast* DefVar (Ast Def) ((#:none annos) (#:none id)
                               (#:just t) (#:just body)))

;; Function parameter declaration.
(define-ast* Param (Ast Def) ((#:none annos) (#:none id) (#:just t)))

;; Function declaration. `t` is the function type.
(define-ast* Defun (Ast Def) ((#:none annos) (#:none id)
                              (#:just t) (#:many params)
                              (#:just body)))

;; `t` is a Magnolisp type expression.
(define-ast* TypeAlias (Ast Def) ((#:none annos) (#:none id) (#:just t)))

;; `frg-t` is a foreign type expression.
(define-ast* ForeignTypeDecl (Ast Def) ((#:none annos) (#:none id)
                                        (#:just frg-t)))

;;; 
;;; other Magnolisp
;;; 

;; For functions with no Magnolisp body.
(define-ast* NoBody (Ast) 
  ((#:none annos)) #:singleton (#hasheq()))

(define-ast* ForeignTypeExpr (Ast) ((#:none annos)))

;; `def` contains a DefVar term. 'let-kind annotation has either
;; 'let-values or 'letrec-values or 'letrec-syntaxes+values; we mostly
;; do not care, since Racket has done name resolution.
(define-ast* LetStat (Ast Stat SeqCont) 
  ((#:none annos) (#:just def) (#:many ss)))

;; There is a 'let-kind annotation.
(define-ast* LetExpr (Ast Expr SeqCont) 
  ((#:none annos) (#:just def) (#:many ss)))

;; Sequence of statements.
(define-ast* CxxBlockStat (Ast Stat SeqCont) 
  ((#:none annos) (#:many ss)))

;; Spliced sequence of statements.
(define-ast* SeqStat (Ast Stat SeqCont) 
  ((#:none annos) (#:many ss)))

;; Spliced sequence of expressions. Like `begin`.
(define-ast* SeqExpr (Ast Expr SeqCont) 
  ((#:none annos) (#:many ss)))

(define-ast* Begin0 (Ast Expr SeqCont) 
  ((#:none annos) (#:many ss)))

;; A statement that does nothing.
(define-ast* VoidStat (Ast Stat) ((#:none annos)))

;; An expression of unit type (C++ only).
(define-ast* VoidExpr (Ast Expr) ((#:none annos)))

;; Variable reference.
(define-ast* Var (Ast Expr) ((#:none annos) (#:none id)))

;; Function value.
(define-ast* Lambda (Ast Expr) 
  ((#:none annos) (#:many params) (#:just body)))

;; Assignment. Both expression and statement variants. In Magnolisp it
;; is a statement, and in C++ it could be either.
(define-ast* AssignExpr (Ast Expr) 
  ((#:none annos) (#:just lv) (#:just rv)))
(define-ast* AssignStat (Ast Stat) 
  ((#:none annos) (#:just lv) (#:just rv)))

;; If expression.
(define-ast* IfExpr (Ast Expr If) ([#:none annos] [#:just c] 
                                   [#:just t] [#:just e]))

;; If statement.
(define-ast* IfStat (Ast Stat If) ([#:none annos] [#:just c] 
                                   [#:just t] [#:just e]))

;; A literal datum.
(define-ast* Literal (Ast Expr) ((#:none annos) (#:none datum)))

;; Function application, with a function expression, and argument
;; expressions.
(define-ast* ApplyExpr (Ast Expr) ((#:none annos) (#:just f) 
                                   (#:many args)))

;; A statement that is just an expression with a discarded result.
(define-ast* ExprStat (Ast Stat) 
  ((#:none annos) (#:just e)))

;; A Racket expression. Should get dropped at some point during
;; compilation as most back ends cannot handle it. Its type can also
;; only be determined from context.
(define-ast* RacketExpr (Ast Expr) 
  ((#:none annos)))

;;; 
;;; C++
;;;

;; kind is either 'user or 'system.
(define-ast* Include (Ast) ((#:none annos) (#:none kind) (#:none s)))

;; `rtype` is the return type, only. `s` is the body statement, which
;; should be a `CxxBlockStat` for printing, or it can be `NoBody` also.
(define-ast* CxxDefun (Ast Def) ((#:none annos) (#:none id)
                                 (#:none modifs) (#:just rtype)
                                 (#:many params) (#:just s)))

;; A C++ function prototype declaration. No body, and some modifiers
;; may have to be different compared to the function definition.
(define-ast* Proto (Ast Def) ((#:none annos) (#:none id)
                              (#:none modifs) (#:just rtype)
                              (#:many params)))

(define-ast* ReturnStat (Ast Stat) ((#:none annos) (#:just e)))

(define-ast* PpCxxIfStat (Ast) ((#:none annos) (#:just c)
                                (#:many ts) (#:many es)))

(define-ast* DeclVar (Ast Def) 
  ((#:none annos) (#:none id) (#:just t)))

;; An expression whose value is given by variable `id`, and assigned
;; to by the statement sequence `ss`, except where the expression has
;; unit type. The variable will be automatically declared upon
;; lifting, and the statements will be lifted to a suitable context.
;; The result should always get assigned to by the statements, at
;; least if the lifted expression is ever to be evaluated.
(define-ast* LiftStatExpr (Ast Expr SeqCont) 
  ((#:none annos) (#:none id) (#:many ss)))

;; Label for the following statements. Itself a statement. `id` is the
;; label Id.
(define-ast* LabelDef (Ast Stat)
  ((#:none annos) (#:none id)))

;; Where `id` is a label Id. A statement.
(define-ast* Goto (Ast Stat) 
  ((#:none annos) (#:none id)))

;; Top-level verbatim string.
(define-ast* TlVerbatim (Ast) 
  ((#:none annos) (#:none s)))

;;; 
;;; built-in types
;;; 

(define ((make-NameT-pred id) ast)
  (matches? ast (NameT _ (? (lambda (x) (Id=? x id))))))

;; By convention any built-ins get the "bare" bind value, i.e. the
;; same symbol as the name.
(define-syntax (define-builtin-type* stx)
  (syntax-case stx ()
    ((_ name)
     (let ((sym (syntax->datum #'name)))
       (with-syntax ((the-id (format-id stx "the-~a-id" sym))
                     (the-type (format-id stx "the-~a-type" sym))
                     (type? (format-id stx "~a-type?" sym)))
         #'(begin
             (define* the-id (annoless Id 'name 'name))
             (define* the-type (annoless NameT the-id))
             (define* type? (make-NameT-pred the-id))))))))

;; The unit type.
(define-builtin-type* Void)

;; The boolean type.
(define* the-Bool-id (annoless Id 'Bool 'Bool))
(define* the-Bool-type (annoless NameT the-Bool-id))

(define* builtin-type-id-lst
  (list the-Bool-id the-Void-id))

(define* a-VoidExpr (VoidExpr (hasheq 'type the-Void-type)))

;;; 
;;; annotation merging
;;; 

;; Later annotations in `hs` are of increasing significance. Any 'type
;; annotations are treated specially, so that `AnyT` types are not as
;; significant as others.
(define* (merge-annos . hs)
  (for/fold ([r #hasheq()]) ([h hs])
    (for ([(k v) h])
      (cond
       [(and (eq? 'type k) (hash-has-key? r k) (AnyT? v))
        (void)]
       [else
        (set! r (hash-set r k v))]))
    r))

;;; 
;;; Id replacing AST rewrite
;;;

(define* (ast-rw-Ids rw-id ast)
  (define (rw-annos annos)
    (define type (hash-ref annos 'type #f))
    (and type
         (hash-set annos 'type (rw type))))

  (define (ast-rw-annos ast)
    (define annos (rw-annos (Ast-annos ast)))
    (if annos (set-Ast-annos ast annos) ast))
  
  (define rw
    (topdown-rewriter
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
         (else
          (ast-rw-annos ast))))))

  (rw ast))

;;; 
;;; location info dumping
;;; 

(define* (ast-dump-loc-info ast)
  (define rw
    (topdown-visitor
     (lambda (ast)
       (define sym (struct-symbol ast))
       (define stx (ast-stx ast))
       (define src 
         (and stx
              (list
               (let-and p (syntax-source stx)
                 (if (path? p)
                     (path-basename-as-string p)
                     p))
               (syntax-line stx)
               (syntax-column stx)
               (syntax-position stx)
               (syntax-span stx)
               (let-and p (syntax-source-module stx #t)
                 (if (path? p)
                     (path-basename-as-string p)
                     p)))))
       (writeln `(,sym ,src)))))
  (rw ast))
