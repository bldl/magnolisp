#lang racket

#|
|#

(require "ast-magnolisp.rkt" "strategy.rkt" "util.rkt"
         syntax/id-table)

(define (ast-expr? ast)
  (any-pred-holds
   Apply?
   BlockExpr?
   Literal?
   Var?
   ast))

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

(define (fresh-type)
  (annoless BoxT (annoless VarT (gensym "t"))))
  
(define (expr-get-type ast)
  (ast-anno-maybe ast 'type))

(define (expr-get-type-or-make ast)
  (or (expr-get-type ast) (fresh-type)))

(define (expr-set-type ast t)
  (Ast-anno-set ast 'type t))

(define (expr-set-boxed-type! ast t)
  (define b (expr-get-type ast))
  (set-BoxT-t! b t))

;; Returns #f for nodes that have no type.
(define (ast-get-type ast)
  (if (Def? ast)
      (def-get-type ast)
      (expr-get-type ast)))

(define (ast-set-type ast t)
  (if (Def? ast)
      (def-set-type ast t)
      (expr-set-type ast t)))

(define ast-add-VarT
  (topdown
   (lambda (ast)
     (match ast
       ((? ast-expr?)
        (expr-set-type ast (fresh-type)))
       ((DefVar a id t v)
        (cond
         ((AnyT? t)
          (DefVar a id (fresh-type) v))
         ((NameT? t)
          ast)
         (else
          (raise-language-error/ast
           "illegal type for a variable"
           ast t))))
       ((Param a id t) ;;; xxx Param can also appear as a definition in its own right, not just under Defun - we may need to lookup a Param Var, so we need it in table - but the Param in AST should probably also get the same type info
        (match t
          ((? NameT?) ast)
          ((BoxT _ (VarT _ _)) ast)
          (else
           (raise-language-error/ast
            "illegal type for a parameter"
            ast t))))
       ((Defun a id t ps b)
        (define n-t t)
        (cond
         ((AnyT? t)
          (set! n-t
            (annoless FunT
                      (map (lambda (x) (fresh-type)) ps)
                      (fresh-type))))
         ((FunT? t)
          (unless (= (length ps) (length (FunT-ats t)))
            (raise-language-error/ast
             "arity mismatch between function and its type"
             ast t)))
         (else
           (raise-language-error/ast
            "illegal type for a function"
            ast t)))
        (define n-ps
          (map
           (lambda (p t)
             (match p
               ((Param a n o-t)
                (assert (AnyT? o-t))
                (Param a n t))))
           ps (FunT-ats n-t)))
        (Defun a id n-t n-ps b))
       (_ ast)))))

(define (unbox-t t)
  (if (BoxT? t)
      (BoxT-t t)
      t))

(define (type=? x y)
  (set! x (unbox-t x))
  (set! y (unbox-t y))
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

;; Takes an immutable-free-id-table containing just the program, and
;; checks/infers its types. 'defs' itself is used as the type
;; environment. The input may contain AnyT values, long as their
;; meaning can be inferred. Returns a fully typed program, with
;; expressions having 'type' annotations.
(define* (defs-type-infer defs)
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values defs))))
  
  (define (lookup x)
    (assert (Var? x))
    (define def-id (get-def-id-or-fail x))
    (define def (dict-ref defs def-id))
    (def-get-type def))

  (define (unify! x y) ;; Type? Type? -> (or/c Type? #f)
    (cond
     ((VarT? (unbox-t x))
      (set-BoxT-t! x (unbox-t y))
      y)
     ((VarT? (unbox-t y))
      (set-BoxT-t! y (unbox-t x))
      x)
     ((and (NameT? (unbox-t x)) (NameT? (unbox-t y)))
      (and (free-identifier=?
            (get-def-id-or-fail (unbox-t x))
            (get-def-id-or-fail (unbox-t y)))
           (unbox-t x)))
     ((and (FunT? (unbox-t x)) (FunT? (unbox-t y)))
      (define x-rt (FunT-rt (unbox-t x)))
      (define y-rt (FunT-rt (unbox-t y)))
      (define x-ats (FunT-ats (unbox-t x)))
      (define y-ats (FunT-ats (unbox-t y)))
      (define f-t
        (and
         (= (length x-ats) (length y-ats))
         (let ((rt (unify! x-rt y-rt)))
           (and rt
                (let ((ats (map unify! x-ats y-ats)))
                  (and (andmap values ats)
                       (annoless FunT ats rt)))))))
      (and f-t
           (begin (assert (type=? f-t x))
                  (assert (type=? f-t y))
                  (if (ast-anno-maybe (unbox-t x) 'stx)
                      x y))))
     (else #f)))

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

  (set! defs
        (for/dict
         (make-immutable-free-id-table #:phase 0)
         (((id def) (in-dict defs)))
         (let ()
           (define n-def (ast-add-VarT def))
           (values id n-def))))

  (for (((id def) (in-dict defs)))
    (ti-def def))

  (set! defs
        (for/dict
         (make-immutable-free-id-table #:phase 0)
         (((id def) (in-dict defs)))
         (values id (ast-rm-BoxT def))))
  
  defs)
