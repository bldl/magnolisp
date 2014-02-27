#lang racket

#|

Data types used for internal representation of abstract syntax.

It is rather important for all Ast derived node types to be
#:transparent, as such is assumed by some of the compiler machinery.

|#

(require "ast-util.rkt" "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt" syntax/id-table)

;;; 
;;; abstract node
;;; 

(define-ast-base* Ast)

(define* (annoless typ . arg*)
  (apply typ #hasheq() arg*))

(define* (syntaxed stx typ . arg*)
  (apply typ (hasheq 'stx stx) arg*))

(define* (self-syntaxed typ stx)
  (syntaxed stx typ stx))

(define* (ast-anno-must ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k)))

(define* (ast-anno-maybe ast k)
  (let* ((annos (Ast-annos ast)))
    (hash-ref annos k #f)))

(define* (ast-anno-set ast k v)
  (define old-annos (Ast-annos ast))
  (define new-annos (hash-set old-annos k v))
  (ast-set-annos ast new-annos))

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
(define-ast* Id Ast ((no-term name) (no-term bind))
  #:property prop:custom-write id-write)

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

(define-with-contract*
  (->* () ((or/c symbol? string?)) Id?)
  (fresh-ast-identifier [sym 'g])
  (annoless Id sym (gensym sym)))

(define-with-contract*
  (->* (identifier?) (#:bind (or/c symbol? Id?)) Id?)
  (identifier->ast id #:bind [other #f])
  (define name (syntax-e id))
  (define bind (cond
                ((symbol? other) other)
                ((Id? other) (Id-bind other))
                (else (gensym name))))
  (Id (hasheq 'stx id) name bind))

;; Use (make-immutable-free-id-table #:phase 0) to create initial
;; state.
(define-with-contract*
  (-> immutable-free-id-table? identifier? 
      (values immutable-free-id-table? Id?))
  (identifier->ast/stateful id->bind id)
  (define name (syntax-e id))
  (define def-id (or (syntax-property id 'def-id) id))
  (define bind (dict-ref id->bind def-id #f))
  (unless bind
    (set! bind (gensym name))
    (set! id->bind (dict-set id->bind def-id bind)))
  (values id->bind
          (Id (hasheq 'stx id) name bind)))

(define-with-contract*
  (-> hash? Id? any/c)
  (ast-identifier-lookup bind->def id)
  (define def (hash-ref bind->def (Id-bind id) #f))
  def)

;; Returns #f instead of Id in the bind? = #t case if already bound,
;; or in the bind? = #f case if unbound. Otherwise returns a
;; functionally modified Id, in addition to the updated state.
(define-with-contract*
  (-> hash? hash? boolean? Id?
      (values hash? hash? (or/c Id? #f)))
  (ast-identifier-assign-name name->num bind->name bind? id)
  (match-define (Id a n b) id)
  (cond
   (bind?
    (cond
     ((hash-has-key? bind->name b)
      (values name->num bind->name #f))
     (else
      (define-values (name->num+ name)
        (next-gensym name->num (string->symbol n)))
      (values name->num+
              (hash-set bind->name b name)
              (Id a name b)))))
   (else
    (define name (hash-ref bind->name b #f))
    (values name->num bind->name
            (and name
                 (Id a name b))))))

;;; 
;;; type expressions
;;; 

(abstract-struct* Type Ast () #:transparent)

(define-ast* AnyT Type () #:singleton (#hasheq()))

(define-ast* VarT Type ((no-term sym)))

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

;; For functions with no Magnolisp body.
(define-ast* NoBody Ast ())

;; 'defs' contains DefVar terms. 'let-kind annotation has either
;; 'let-values or 'letrec-values; we mostly do not care, since Racket
;; has done name resolution.
(define-ast* Let Ast ((list-of-term defs) (list-of-term ss)))

;; We only allow a limited form of 'let' expressions.
(define-ast* LetExpr Ast ((just-term def) (just-term e)))

;; Sequence of statements.
(define-ast* BlockStat Ast ((list-of-term ss)))

;; Variable reference.
(define-ast* Var Ast ((no-term id)))

;; Function value.
(define-ast* Lambda Ast ((list-of-term params) (just-term body)))

;; Assignment.
(define-ast* Assign Ast ((just-term lv) (just-term rv)))

;; If expression.
(define-ast* IfExpr Ast ((just-term c) (just-term t) (just-term e)))

;; If statement.
(define-ast* IfStat Ast ((just-term c) (just-term t) (just-term e)))

;; A literal datum.
(define-ast* Literal Ast ((no-term datum)))

;; Function (or predicate) application, with function expression, and
;; argument expressions.
(define-ast* Apply Ast ((just-term f) (list-of-term args)))

;; Procedure call. A statement.
(define-ast* Call Ast ((just-term f) (list-of-term args)))

;; Transient. Corresponds to a let/ec that only escapes to a local,
;; immediately surrounding call/cc continuation. 'k' is a label (an
;; ID) naming the continuation.
(define-ast* LetLocalEc Ast ((no-term k) (list-of-term ss)))

;; Escapes to the named LetLocalEc continuation 'k' (an ID) with the
;; value given by expression 'e'.
(define-ast* AppLocalEc Ast ((no-term k) (just-term e)))

;; Nil statement.
(define-ast* Pass Ast ())

;; Block expression. Contains statements.
(define-ast* BlockExpr Ast ((list-of-term ss)))

;; Return statement. For now we only support single value returns. The
;; semantics are to escape from a surrounding BlockExpr.
(define-ast* Return Ast ((just-term e)))

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

(define-ast* CxxDeclVar Def ((just-term t)))

;; Statement expression (GCC extension).
(define-ast* GccStatExpr Ast ((list-of-term ss) (just-term e)))

;; Local label declaration (GCC extension). A statement.
(define-ast* GccLabelDecl Ast ((no-term n)))

;; Label for the following statements. Itself a statement.
(define-ast* CxxLabel Ast ((no-term n)))

;; Where 'n' is a label. A statement.
(define-ast* Goto Ast ((no-term n)))

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
  ;;(writeln x)
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
;;; expressions
;;; 

(define* (ast-expr? ast)
  (any-pred-holds
   Apply?
   BlockExpr?
   IfExpr?
   Literal?
   Var?
   ast))

;;; 
;;; statement containers
;;;

(define* (StatCont? ast)
  (any-pred-holds BlockExpr? BlockStat? Let? ast))

(define-match-expander* StatCont
  (syntax-rules ()
    [(_ a ss)
     (or (BlockStat a ss)
         (BlockExpr a ss)
         (Let a _ ss))]))

(define* (StatCont-ss ast)
  (match ast
    ((BlockStat _ ss) ss)
    ((BlockExpr _ ss) ss)
    ((Let _ _ ss) ss)
    (_ #f)))

(define* (set-StatCont-ss ast n-ss)
  (match ast
    ((BlockExpr a ss)
     (BlockExpr a n-ss))
    ((BlockStat a ss)
     (BlockStat a n-ss))
    ((Let a bs ss)
     (Let a bs n-ss))))

(define* (StatCont-copy ast n-a n-ss)
  (match ast
    ((BlockExpr a ss)
     (BlockExpr n-a n-ss))
    ((BlockStat a ss)
     (BlockStat n-a n-ss))
    ((Let a bs ss)
     (Let n-a bs n-ss))))

;;; 
;;; types
;;; 

(define* (expr-get-type ast)
  (ast-anno-maybe ast 'type-ast))

(define* (expr-set-type ast t)
  (ast-anno-set ast 'type-ast t))

;;; 
;;; exports
;;; 

(define* (get-export-name x)
  (cond
   ((hash? x) (hash-ref x 'export-name #f))
   ((Def? x) (ast-anno-maybe x 'export-name))
   (else
    (raise-argument-error
     'get-export-name
     "(or/c hash? Def?)" x))))

(define* (get-export-name-as-symbol x)
  (define y (get-export-name x))
  (if (identifier? y)
      (syntax-e y)
      y))

;;; 
;;; externals
;;; 

(define* (get-foreign-name x)
  (cond
   ((hash? x) (hash-ref x 'foreign #f))
   ((Def? x) (ast-anno-maybe x 'foreign))
   (else
    (raise-argument-error
     'get-foreign-name
     "(or/c hash? Def?)" x))))

(define* (get-foreign-name-as-symbol x)
  (define y (get-foreign-name x))
  (if (identifier? y)
      (syntax-e y)
      y))

;;; 
;;; simplification
;;; 

(define ast-empty-Let->BlockStat
  (topdown
   (lambda (ast)
     (match ast
       ((Let a (list) ss)
        (BlockStat a ss))
       (_ ast)))))

(define ast-nested-BlockStat->BlockStat
  (topdown
   (repeat
    (lambda (ast)
      (define ss (StatCont-ss ast))
      (cond
       ((and ss (ormap BlockStat? ss))
        (define n-ss
          (apply append (for/list ((s ss))
                          (if (BlockStat? s)
                              (BlockStat-ss s)
                              (list s)))))
        (set-StatCont-ss ast n-ss))
       (else
        ;; Signifies failed strategy.
        #f))))))
       
(define (list-rm-Pass ss)
  (filter (negate Pass?) ss))

(define ast-rm-Pass
  (topdown
   (lambda (ast)
     (match ast
       [(StatCont a (? (curry ormap Pass?) ss))
        (StatCont-copy ast a (filter (negate Pass?) ss))]
       [_ ast]))))

(define (take-until/inclusive p? lst)
  (define n-lst null)
  (let loop ((lst lst))
    (cond
     ((null? lst)
      (void))
     (else
      (define e (car lst))
      (set! n-lst (cons e n-lst))
      (cond
       ((p? e) (void))
       (else (loop (cdr lst)))))))
  (reverse n-lst))

(define ast-rm-dead-code
  (topdown
   (lambda (ast)
     (define ss (StatCont-ss ast))
     (cond
      ((and ss (ormap Return? ss))
       (define n-ss (take-until/inclusive Return? ss))
       (set-StatCont-ss ast n-ss))
      (else
       ast)))))

(define ast-simplify-BlockExpr
  (topdown
   (repeat
    (lambda (ast)
      (match ast
        ((BlockExpr _ (list (Return a e)))
         e)
        (_ #f))))))

(define* ast-simplify
  (compose1->
   ast-empty-Let->BlockStat
   ast-rm-Pass
   ast-nested-BlockStat->BlockStat
   ast-rm-dead-code
   ast-simplify-BlockExpr))

;;; 
;;; AST dumping
;;;

(define* (ast->list ast (annos null))
  (define lst null)
  ((topdown-visit
    (lambda (ast)
      (define h (Ast-annos ast))
      (set! lst
            (cons
             `(,ast ANNOS ,@(for/list ((k annos)
                                       #:when (hash-has-key? h k))
                              `(,k ,(hash-ref h k))))
             lst))))
   ast)
  (reverse lst))

(define* (defs-dump defs (annos null))
  (pretty-print
   (for/list (((id def) (in-dict defs)))
     `(DEF ,(syntax-e id) IS ,@(ast->list def annos)))))

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
    ((NoBody _)
     'no-body)
    ((Defun a id t ps b)
     `(function (,(->symbol id) ,@(map ast->sexp ps))
        #:annos ((type ,(ast->sexp t))
                 (export ,(get-export-name-as-symbol a))
                 (foreign ,(get-foreign-name-as-symbol a)))
        ,(ast->sexp b)))
    ((Let a ds bs)
     (define n (hash-ref a 'let-kind 'let))
     `(,n (,@(map ast->sexp ds))
          ,@(map ast->sexp bs)))
    ((BlockStat _ ss)
     `(block-stat ,@(map ast->sexp ss)))
    ((BlockExpr _ ss)
     `(block-expr ,@(map ast->sexp ss)))
    ((Return _ e)
     `(return ,(ast->sexp e)))
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
    ((IfStat _ c t e)
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
