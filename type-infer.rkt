#lang racket

#|
|#

(require "ast-magnolisp.rkt" "strategy.rkt" "util.rkt"
         (rename-in racklog [_ make-logic-var])
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

;;; 
;;; init with fresh type variables
;;; 

(define (fresh-type)
  (annoless VarT (make-logic-var)))
  
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
;;; other
;;; 

;; Removes any BoxT wrappers. Ensures that no VarT type expressions
;; remain. 'err-ast' is used for error reporting, only.
(define (type-rm-BoxT err-ast t)
  (define f
    (topdown
     (repeat
      (lambda (ast)
        (cond
         ((BoxT? ast)
          (unbox-t ast))
         ((VarT? ast)
          (raise-language-error/ast
           "cannot infer type"
           err-ast t))
         (else #f))))))
  (f t))

(define ast-rm-BoxT
  (topdown
   (lambda (ast)
     (define t (ast-get-type ast))
     (if t
         (ast-set-type ast (type-rm-BoxT ast t))
         ast))))

;; Racklog does not seem to provide any lower-level access routine to
;; the value of a logic variable, but we can do this. Returns #f if
;; 'lvar' is not bound.
(define (logic-var-val lvar)
  (define r (%which (x) (%= x lvar)))
  (and r (cdr (assq 'x r))))

(define (logic->type type-sym->name a-t)
  (define (f t)
    (match t
      ((? logic-var? t)
       (define v (logic-var-val t))
       (assert v)
       (f v))
      ((list 'fn ats ... rt)
       (annoless FunT (map f ats) (f rt)))
      ((? symbol? t)
       (type-sym->name t))))

  (f a-t))

(define (type->logic type-name->sym a-t)
  (define (f t)
    (match t
      ((VarT _ lvar) lvar)
      ((NameT _ id) (type-name->sym id))
      ((FunT _ ats rt) `(fn ,@(map f ats) ,(f rt)))))
  
  (f a-t))

;; Takes an immutable-free-id-table containing just the program, and
;; checks/infers its types. 'defs' itself is used as the type
;; environment. The input may contain AnyT values, long as their
;; meaning can be inferred. Returns a fully typed program, with
;; definitions having resolved type fields (where appropriate), and
;; expressions having resolved 'type' annotations.
(define* (defs-type-infer defs)
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values defs))))

  (define type-names (make-free-id-table #:phase 0))
  (define type-syms (make-hasheq))
  (define (type-name->sym id)
    (define sym (dict-ref type-names id #f))
    (cond
     ((sym sym))
     (else
      (define b (symbol->string (syntax-e id)))
      (define sym (gensym b))
      (dict-set! type-names id sym)
      (hash-set! type-syms sym id)
      sym)))
  (define (type-sym->name sym)
    (hash-ref type-syms sym))
  
  (define lookup (fix lookup-type-from-defs defs))

  (define (unify! x y) ;; Type? Type? -> void?
    xxx)
    
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
       ;; been checked earlier.
       (define r-t (FunT-rt t))
       (define b-t r-t)
       (unless (NoBody? b)
         (set! b-t (ti-expr b)))
       (define u-t (unify! r-t b-t))
       (unless u-t
         (raise-language-error/ast
          "function return type does not match body expression"
          #:fields (list (list "declared return type"
                               (ast-displayable/datum (unbox-t r-t)))
                         (list "actual return type"
                               (ast-displayable/datum (unbox-t b-t))))
          ast b))
       (assert (type=? r-t u-t))
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
                                (unbox-t expect-t))))))
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
         (define r-t (return-type))
         (unless r-t
           (raise-language-error/ast
            "block expression without return"
            ast))
         (expr-set-boxed-type! ast r-t)
         r-t))
      ((? Var?)
       (define t (lookup ast))
       ;;(writeln `(var has type ,(expr-get-type ast) setting to ,t))
       (expr-set-boxed-type! ast t)
       t)
      ((Apply _ f as)
       (define f-t (ti-expr f))
       (unless (FunT? (unbox-t f-t))
         (raise-language-error/ast
          "application of a non-function"
          #:fields (list (list "type" (ast-displayable (unbox-t f-t))))
          ast f))
       (unless (= (length as) (length (FunT-ats (unbox-t f-t))))
         (raise-language-error/ast
          "function arity does not match number of arguments"
          #:fields (list (list "function type"
                               (ast-displayable (unbox-t f-t))))
          ast))
       (for ([e as] [p-t (FunT-ats (unbox-t f-t))])
         (define e-t (ti-expr e))
         (unless (unify! p-t e-t)
           (raise-language-error/ast
            "parameter type does not match that of expression"
            #:fields (list
                      (list "parameter type"
                            (ast-displayable (unbox-t p-t)))
                      (list "function type"
                            (ast-displayable (unbox-t f-t))))
            ast e)))
       (define t (FunT-rt (unbox-t f-t)))
       (expr-set-boxed-type! ast t)
       t)
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
         (values id (ast-rm-BoxT def))))
  
  defs)
