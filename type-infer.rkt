#lang racket

#|
|#

(require "ast-magnolisp.rkt" "compiler-util.rkt"
         "strategy.rkt" "util.rkt"
         syntax/id-table)

;;; 
;;; generic passes
;;;

;; This pass may be used to synchronize the definitions table with
;; updated local definitions (such as Param) after changes have been
;; made within the global definitions.
(define (defs-update-defs-table defs)
  (define f
    (topdown-visit
     (lambda (ast)
       (when (ast-local-def? ast)
         (define id (Def-id ast))
         (set! defs (dict-set defs id ast))))))
  
  (for (((id def) (in-dict defs)))
    (unless (ast-local-def? def)
      (f def)))

  defs)

;;; 
;;; utilities
;;; 

;; In IR we do not allow global DefVars.
(define (ast-local-def? ast)
  (any-pred-holds
   Param? DefVar?
   ast))

(define (expr-get-type ast)
  (ast-anno-maybe ast 'type))

(define (expr-set-type ast t)
  (Ast-anno-set ast 'type t))

(define (def-get-type def)
  (match def
    ((DefVar _ _ t _)
     t)
    ((Param _ _ t)
     t)
    ((Defun _ _ t _ _)
     t)
    ((ForeignTypeDecl _ id _)
     (syntaxed id DefNameT id))
    ))

(define (def-set-type def t)
  (match def
    ((DefVar a id _ v)
     (DefVar a id t v))
    ((Param a id _)
     (Param a id t))
    ((Defun a id _ ps b)
     (Defun a id t ps b))
    ((? ForeignTypeDecl?)
     def)))

(define (lookup-type-from-defs defs x)
  (assert (Var? x))
  (define def-id (get-def-id-or-fail x))
  (define def (dict-ref defs def-id))
  (def-get-type def))

;; Returns #f for nodes that have no type.
(define (ast-get-type ast)
  (if (Def? ast)
      (def-get-type ast)
      (expr-get-type ast)))

(define (ast-set-type ast t)
  (if (Def? ast)
      (def-set-type ast t)
      (expr-set-type ast t)))

(define (type=? x y)
  (cond
   ((VarT? x) #t)
   ((VarT? y) #t)
   ((and (NameT? x) (NameT? y))
    (free-identifier=?
     (get-def-id-or-fail x)
     (get-def-id-or-fail y)))
   ((and (FunT? x) (FunT? y))
    (define x-rt (FunT-rt x))
    (define y-rt (FunT-rt y))
    (define x-ats (FunT-ats x))
    (define y-ats (FunT-ats y))
    (and (= (length x-ats) (length y-ats))
         (type=? x-rt y-rt)
         (andmap type=? x-ats y-ats)))
   (else #f)))

(define (variable-type? t)
  (let/ec k
    ((topdown-visit
      (lambda (ast)
        (when (VarT? ast)
          (k #t))))
     t)
    #f))

;;; 
;;; init with fresh type variables
;;; 

(define (fresh-type)
  (annoless VarT (gensym "t")))
  
(define type-AnyT->VarT
  (topdown
   (lambda (t)
     (cond
      ((AnyT? t) (fresh-type))
      (else t)))))

(define (type-add-VarT t)
  (cond
   ((not t) (fresh-type))
   ((AnyT? t) (fresh-type))
   (else (type-AnyT->VarT t))))

(define (ast-expr-add-VarT defs-t def)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((? ast-expr?)
          (define t (type-add-VarT (expr-get-type ast)))
          (expr-set-type ast t))
         (_ ast)))))
  
  (rw def))

(define (def-add-VarT ast)
  (match ast
    ((Defun a id t ps b)
     (define n-t
       (cond
        ((AnyT? t)
         (annoless FunT
                   (map (lambda (x) (fresh-type)) ps)
                   (fresh-type)))
        ((FunT? t)
         (unless (= (length ps) (length (FunT-ats t)))
           (raise-language-error/ast
            "arity mismatch between function and its type"
            ast t))
         (type-add-VarT t))
        (else
         (raise-language-error/ast
          "illegal type for a function"
          ast t))))
     (define n-ps
       (map
        (lambda (p t)
          (match p
            ((Param a n o-t)
             (assert (AnyT? o-t))
             (Param a n t))))
        ps (FunT-ats n-t)))
     (Defun a id n-t n-ps b))
    (_ ast)))

(define (defs-add-VarT defs)
  ;; First add types to top-level definitions. This includes any local
  ;; definitions within them.
  (set! defs
        (for/dict
         (make-immutable-free-id-table #:phase 0)
         (((id def) (in-dict defs)))
         (values id (def-add-VarT def))))

  ;; Sync local definitions info into 'defs' table.
  (set! defs (defs-update-defs-table defs))

  ;; Now add types to expression. For variable definitions, use the
  ;; types given to the definitions.
  (set! defs
        (for/dict
         (make-immutable-free-id-table #:phase 0)
         (((id def) (in-dict defs)))
         (values id (ast-expr-add-VarT defs def))))

  defs)

;;; 
;;; constraint solver
;;; 

;; Simplifies type 't' using any applicable substitions in 'h'
;; (applied recursively). As a side effect, may update 'h' for
;; purposes for memoization. Returns the simplified type. Note that
;; eq? may be used to determine if any simplification took place.
(define (subst h t) ;; (-> hash? Type? Type?)
  ;; 'ix' of last successful lookup.
  (define last-ix 0)
  
  (define (lookup sym ix)
    (define ast (hash-ref h sym #f))
    (and ast
         (begin
           (set! last-ix ix)
           ast)))

  (define (memoize ix sym ast)
    (when (< ix last-ix)
      ;;(writeln `(memoize ,sym = ,ast))
      (hash-set! h sym ast))
    ast)
  
  (define (f ix ast)
    (match ast
     ((VarT _ sym)
      (define n-ast (lookup sym ix))
      (cond
       ((not n-ast) ast)
       (else
        (memoize ix sym (f (+ ix 1) n-ast)))))
     ((? NameT?)
      ast)
     ((FunT a ats rt)
      (FunT a (map (fix subst h) ats) (subst h rt)))
     (else
      (raise-argument-error
       'f "(or/c FunT? NameT? VarT?)"
       1 ix ast))))

  (f 0 t))

;; Unifies types 'x' and 'y', in the context of the given
;; substitutions [h mutable (hash/c symbol? Type?)]. As a side effect,
;; may modify 'h' to add new substitutions. Returns #t if 'x' and 'y'
;; are unifiable, and #f otherwise.
(define (unify h x y)
  ;; 'x' and 'y' must have any substitutions applied.
  (define (f x y)
    (cond
     ((and (VarT? x) (VarT? y))
      (define x-sym (VarT-sym x))
      (define y-sym (VarT-sym y))
      (unless (eq? x-sym y-sym)
        (hash-set! h x-sym y))
      #t)
     ((VarT? x)
      (define x-sym (VarT-sym x))
      (hash-set! h x-sym y)
      #t)
     ((VarT? y)
      (define y-sym (VarT-sym y))
      (hash-set! h y-sym x)
      #t)
     ((and (NameT? x) (NameT? y))
      (free-identifier=?
       (get-def-id-or-fail x)
       (get-def-id-or-fail y)))
     ((and (FunT? x) (FunT? y))
      (define x-rt (FunT-rt x))
      (define y-rt (FunT-rt y))
      (define x-ats (FunT-ats x))
      (define y-ats (FunT-ats y))
      (and
       (= (length x-ats) (length y-ats))
       (unify h x-rt y-rt)
       (andmap (fix unify h) x-ats y-ats)))
     (else
      #f)))
  
  (define s-x (subst h x))
  (define s-y (subst h y))
  (f s-x s-y))

;;; 
;;; VarT removal
;;; 

;; Ensures that no VarT type expressions remain in type expression
;; 't'. It is an error for any appearing VarT to not have an entry in
;; 'var-h', as that means that the program's (concrete) types cannot
;; be fully determined. 'ctx-ast' is the expression or definition
;; whose type is being simplified; it is only used for error
;; reporting.
(define (type-rm-VarT var-h t ctx-ast)
  (define f
    (topdown
     (lambda (ast)
       (cond
        ((VarT? ast)
         (define n-ast (subst var-h ast))
         (when (VarT? n-ast)
           (raise-language-error/ast
            "cannot resolve concrete type"
            #:continued "program is not fully typed"
            ctx-ast t))
         n-ast)
        (else ast)))))
  (f t))

;; Uses the 'var-h' table to substitute any VarT nodes in the type
;; fields and annotations of the program tree 'ast' with concrete type
;; expressions.
(define (ast-rm-VarT var-h ast)
  (define f
    (topdown
     (lambda (ast)
       (define t (ast-get-type ast))
       (if t
           (ast-set-type ast (type-rm-VarT var-h t ast))
           ast))))
  (f ast))

;;; 
;;; API
;;; 

;; Takes an immutable-free-id-table containing just the program, and
;; checks/infers its types. 'defs' itself is used as the type
;; environment. The input may contain AnyT values, long as their
;; meaning can be inferred. Returns a fully typed program, with
;; definitions having resolved type fields (where appropriate), and
;; expressions having resolved 'type' annotations.
(define* (defs-type-infer defs)
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values defs))))

  (define lookup (fix lookup-type-from-defs defs))

  (define var-h (make-hasheq))

  ;; Possibly adds a constraint between types 'x' and 'y'. Returns #t
  ;; if they are unifiable or possibly solvable, and #f otherwise.
  (define (unify! x y) ;; Type? Type? -> boolean?
    (unify var-h x y))

  (define (expr-unify! e t)
    (define e-t (expr-get-type e))
    (assert e-t)
    (unify! e-t t)
    t)
    
  (define (ti-def ast) ;; Def? -> void?
    (match ast
      ((? ForeignTypeDecl?)
       ;; Type can always be derived from 'id'.
       (void))
      ((? Param?)
       ;; We cannot learn any new information here.
       (void))
      ((Defun a id t ps b)
       ;; Type kind and arity correctness wrt parameters has already
       ;; been checked earlier. There is only any checking to do now
       ;; if there is a body.
       (unless (NoBody? b)
         (define r-t (FunT-rt t))
         (define b-t (ti-expr b))
         (unless (unify! r-t b-t)
           (raise-language-error/ast
            "function return type does not match body expression"
            #:fields (list (list "declared return type"
                                 (ast-displayable/datum r-t))
                           (list "actual return type"
                                 (ast-displayable/datum b-t)))
            ast b)))
       (void))
      (else
       (raise-argument-error
        'ti-def "supported Ast?" ast))))

  ;; Initialized to #f for each BlockExpr scope.
  (define return-type (make-parameter #f))
  
  (define (ti-stat ast) ;; Ast? -> void?
    (match ast
      ((Return _ e)
       (define t (ti-expr e))
       (define expect-t (return-type))
       (when (and expect-t (not (unify! expect-t t)))
         (raise-language-error/ast
          "conflicting return type in block"
          ast e
          #:fields (list (list "previously"
                               (ast-displayable/datum
                                expect-t)))))
       (return-type t)
       (void))
      ((Pass _)
       (void))
      ((BlockStat _ ss)
       (for-each ti-stat ss))
      ((Let _ bs ss)
       (for-each ti-def bs)
       (for-each ti-stat ss))
      (else
       (raise-argument-error
        'ti-stat "supported Ast?" ast))))
      
  (define (ti-expr ast) ;; Ast? -> Type?
    (match ast
      ((BlockExpr _ ss)
       (parameterize ((return-type #f))
         (for-each ti-stat ss)
         (define t (return-type))
         (unless t
           (raise-language-error/ast
            "block expression without return"
            ast))
         (expr-unify! ast t)))
      ((? Var?)
       (define t (lookup ast))
       (when (FunT? t)
         (raise-language-error/ast
          "reference to a function as a value"
          #:fields (list (list "type" (ast-displayable t)))
          ast))
       (expr-unify! ast t))
      ((Apply _ f as)
       ;; We bypass invoking (ti-expr f) here, as we only want to
       ;; allow FunT typed expressions in this context. We must still
       ;; be sure to set up a constraint for the expression 'f', lest
       ;; its type be left unresolved.
       (define f-t (lookup f))
       (expr-unify! f f-t)

       ;; We have done prior work to ensure that a function
       ;; declaration always has FunT type. This check should not
       ;; fail.
       (unless (FunT? f-t)
         (raise-language-error/ast
          "application of a non-function"
          #:fields (list (list "type" (ast-displayable f-t)))
          ast f))

       ;; Now we can unify against the argument expressions' types,
       ;; and also the type of this expression. We could construct a
       ;; FunT instance from said information and leave the checking
       ;; to 'unify', but we get better error messages by doing some
       ;; extra work here. We make sure to add the same constraints
       ;; separately here.
       (unless (= (length as) (length (FunT-ats f-t)))
         (raise-language-error/ast
          "function arity does not match number of arguments"
          #:fields (list (list "function type"
                               (ast-displayable f-t)))
          ast))
       (for ([e as] [p-t (FunT-ats f-t)])
         (define e-t (ti-expr e))
         (unless (unify! p-t e-t)
           (raise-language-error/ast
            "parameter type does not match that of argument"
            #:fields (list
                      (list "parameter type"
                            (ast-displayable p-t)))
            ast e)))

       ;; The type of the Apply expression must unify with the return
       ;; type of the function.
       (define t (FunT-rt f-t))
       (expr-unify! ast t))
      ((? Literal?)
       ;; Cannot learn anything from this. Might in some cases want an
       ;; annotation for an expression, but such are to be supported.
       (expr-get-type ast))
      (else
       (raise-argument-error
        'ti-expr "supported Ast?" ast))))

  (set! defs (defs-add-VarT defs))

  (for (((id def) (in-dict defs)))
    (ti-def def))

  (set! defs
        (for/dict
         (make-immutable-free-id-table #:phase 0)
         (((id def) (in-dict defs)))
         (values id (ast-rm-VarT var-h def))))
  
  defs)
