#lang racket/base

#|
|#

(require racket/contract/base
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/set
         "ir-ast.rkt"
         "ir-transform.rkt"
         "strategy-stratego.rkt"
         "strategy.rkt"
         "util.rkt")

;;; 
;;; utilities
;;; 

(define (lookup-def-from-defs defs x)
  (assert (Var? x))
  (define def (ast-identifier-lookup defs (Var-id x)))
  (unless def
    (raise-language-error/ast
     "reference to unbound name" x))
  def)

(define (lookup-type-from-defs defs x)
  (def-get-type (lookup-def-from-defs defs x)))

(define (type=? x y)
  (cond
   ((VarT? x) #t)
   ((VarT? y) #t)
   ((and (NameT? x) (NameT? y))
    (Id-bind=? (NameT-id x) (NameT-id y)))
   ((and (FunT? x) (FunT? y))
    (define x-rt (FunT-rt x))
    (define y-rt (FunT-rt y))
    (define x-ats (FunT-ats x))
    (define y-ats (FunT-ats y))
    (and (= (length x-ats) (length y-ats))
         (type=? x-rt y-rt)
         (andmap type=? x-ats y-ats)))
   ((and (ParamT? x) (ParamT? y))
    (define x-t (ParamT-t x))
    (define y-t (ParamT-t y))
    (define x-ats (ParamT-ats x))
    (define y-ats (ParamT-ats y))
    (and (type=? x-t y-t)
         (= (length x-ats) (length y-ats))
         (andmap type=? x-ats y-ats)))
   (else #f)))

;; Augments type expression `t` with the type facts available in `h`.
;; Intended for more informative reporting of type errors.
(define (type-concretely h t)
  (let loop ([ast t])
    (match ast
      [(VarT _ sym)
       (if-let n-ast (hash-ref h sym #f)
         (loop n-ast)
         ast)]
      [(FunT a rt ats)
       (FunT a (loop rt) (map loop ats))]
      [(ParamT a t ps)
       (ParamT a (loop t) (map loop ps))]
      [(PhiT a t u)
       (PhiT a (loop t) (loop u))]
      [_ ast])))

;;; 
;;; generics specialization
;;; 

;; The `param-syms` field is the list of VarT syms corresponding to
;; the particular instantiation of the relevant universal type
;; parameters.
(struct ApplyInfo (param-syms) #:transparent)

(define-with-contract
  (-> hash? Var? (values (or/c #f ApplyInfo?) Type?))
  (lookup-use-type-from-defs defs x)
  
  (define def (lookup-def-from-defs defs x))
  (match def
    [(Defun (? (lambda (a) (hash-has-key? a 'univ-type-params)) as) _ _ _ _)
     (define g-t (hash-ref as 'generic-type))
     (define-values (bind->sym n-def)
       (type-expr-rm-ForAllT/use g-t))
     (define info
       (and (hash-ref as 'return-type-overloaded? #f)
            (let* ((ns (hash-ref as 'univ-type-params))
                   (syms (for/list ((n ns))
                           (define id (NameT-id n))
                           (define bind (Id-bind id))
                           (hash-ref bind->sym bind))))
              (ApplyInfo syms))))
     (values info n-def)]
    [_
     (values #f (def-get-type def))]))

(define (defs-add-type<> var-h apply-h defs)
  (define rw-type<>
    (topdown
     (lambda (ast)
       (match ast
         [(ApplyExpr (and (app (lambda (a) 
                                 (define uid (hash-ref a 'uid))
                                 (hash-ref apply-h uid #f))
                               (? identity info))
                          annos) f args)
          (define syms (ApplyInfo-param-syms info))
          (define params
            (for/list ((sym syms))
              (define var (annoless VarT sym))
              (type-rm-VarT var-h var var)))
          (define n-annos
            (hash-set annos 'type<> params))
          (ApplyExpr n-annos f args)]
         [_ ast]))))
  (defs-map/bind rw-type<> defs))

;;; 
;;; init with fresh type variables
;;; 

(define (fresh-VarT)
  (annoless VarT (gensym "t")))
  
(define type-AnyT->VarT
  (topdown
   (lambda (t)
     (cond
      [(AnyT? t) (fresh-VarT)]
      [else t]))))

(define (type-add-VarT t)
  (cond
   ((not t) (fresh-VarT))
   ((AnyT? t) (fresh-VarT))
   (else (type-AnyT->VarT t))))

;; For each expression in 'def', if it has no type expression, add one
;; referring to a fresh type variable. For any existing type
;; expressions in 'def', replace AnyT type expressions with fresh
;; type variables.
(define (ast-expr-add-VarT defs-t def)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(? Expr?)
          (define t (type-add-VarT (Expr-type ast)))
          (set-Expr-type ast t)]
         [_ ast]))))
  
  (rw def))

(define (def-add-VarT def)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((Defun a id t ps b) ;; handles associated Param nodes also
          (define n-t
            (cond
             ((AnyT? t)
              (annoless FunT
                        (map (lambda (x) (fresh-VarT)) ps)
                        (fresh-VarT)))
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
                 [(Param a n o-t)
                  (assert (AnyT? o-t))
                  (Param a n t)]))
             ps (FunT-ats n-t)))
          (Defun a id n-t n-ps b))
         ((DefVar a id t v)
          (DefVar a id (type-add-VarT t) v))
         (_ ast)))))
  (rw def))

(define (defs-add-VarT defs)
  ;; First add types for bindings.
  (set! defs (defs-map/bind def-add-VarT defs))

  ;; Sync local definitions info into 'defs' table.
  (set! defs (defs-table-update-locals/Id defs))

  ;; Now add types to expressions.
  (set! defs (defs-map/bind (fix ast-expr-add-VarT defs) defs))

  defs)

;;; 
;;; constraint solver
;;; 

;; Simplifies type `t` using any applicable substitutions in `h`,
;; which maps (unique) `VarT` symbols to type expressions.
;; Substitutions are applied recursively. As a side effect, may update
;; `h` for purposes of memoization, to replace more complex type
;; expressions with simpler ones. Returns the simplified version of
;; type `t`. The `eq?` operation may be used to determine if any
;; simplification took place. `VarT` nodes will still remain in `t` if
;; not all the appearing type variables had assignments in `h`.
(define-with-contract
  (-> hash? Type? Type?)
  (subst! h t)
  ;;(writeln `(subst! ,h ,t))
  
  (define (lookup sym)
    (hash-ref h sym #f))

  (define (memoize! sym ast)
    (hash-set! h sym ast))

  (let loop ([vars (seteq)] [ast t])
    ;;(writeln `(loop ,vars ,ast))
    (match ast
      [(VarT _ sym)
       (define h-ast (lookup sym))
       (cond
         ((not h-ast) ast)
         (else
          (when (set-member? vars sym)
            (error 'subst! "recursive type ~a: ~s ≡ ~s"
                   (ast-~a t) ast h-ast))
          (define n-ast (loop (set-add vars sym) h-ast))
          (unless (eq? ast n-ast)
            (memoize! sym n-ast))
          n-ast))]
      [(? NameT?)
       ast]
      [(FunT a ats rt)
       (define n-ats (map (curry loop vars) ats))
       (define n-rt (loop vars rt))
       (if (and (eq? rt n-rt) (andmap eq? ats n-ats))
           ast
           (FunT a n-ats n-rt))]
      [(PhiT a t u)
       (define n-t (loop vars t))
       (define n-u (loop vars u))
       (if (and (eq? t n-t) (eq? u n-u))
           ast
           (PhiT a n-t n-u))]
      [(ParamT a bt ats)
       (define n-bt (loop vars bt))
       (define n-ats (map (curry loop vars) ats))
       (if (and (eq? bt n-bt) (andmap eq? ats n-ats))
           ast
           (ParamT a n-bt n-ats))]
      [else
       (error
        'subst!
        "expected (or/c FunT? NameT? ParamT? PhiT? VarT?): ~s"
        ast)])))

(define (unify-with-PhiT? x)
  (or (NameT? x)
      (and (ParamT? x)
           (NameT? (ParamT-t x)))))

;; Unifies types 'x' and 'y', in the context of the given
;; substitutions [h mutable (hash/c symbol? Type?)]. As a side effect,
;; may modify 'h' to add new substitutions. Returns #t if 'x' and 'y'
;; are unifiable, and #f otherwise.
(define (unify! h x y)
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
      (Id-bind=? (NameT-id x) (NameT-id y)))
     ((and (FunT? x) (FunT? y))
      (define x-rt (FunT-rt x))
      (define y-rt (FunT-rt y))
      (define x-ats (FunT-ats x))
      (define y-ats (FunT-ats y))
      (and
       (= (length x-ats) (length y-ats))
       (unify! h x-rt y-rt)
       (andmap (curry unify! h) x-ats y-ats)))
     ((and (ParamT? x) (ParamT? y))
      (define x-t (ParamT-t x))
      (define y-t (ParamT-t y))
      (define x-ats (ParamT-ats x))
      (define y-ats (ParamT-ats y))
      (and
       (= (length x-ats) (length y-ats))
       (unify! h x-t y-t)
       (andmap (curry unify! h) x-ats y-ats)))
     ((and (PhiT? x) (unify-with-PhiT? y))
      (and (unify! (PhiT-t1 x) y)
           (unify! (PhiT-t2 x) y)))
     ((and (PhiT? y) (unify-with-PhiT? x))
      (and (unify! x (PhiT-t1 y))
           (unify! x (PhiT-t2 y))))
     (else
      #f)))
  
  (define s-x (subst! h x))
  (define s-y (subst! h y))
  (f s-x s-y))

;;; 
;;; VarT removal
;;; 

;; Ensures that no VarT type expressions remain in type expression
;; 't'. It is an error for any appearing VarT to not have an entry in
;; 'var-h', as that means that the program's (concrete) types cannot
;; be fully determined. Simplifies PhiT type expressions where it is
;; possible to do so. 'ctx-ast' is the expression or definition whose
;; type is being simplified; it is only used for error reporting.
(define (type-rm-VarT var-h t ctx-ast)
  (define f
    (innermost-rewriter
     (lambda (ast)
       (match ast
        [(? VarT?)
         (define n-ast (subst! var-h ast))
         (when (VarT? n-ast)
           (raise-language-error/ast
            "cannot resolve concrete type"
            #:continued "program is not fully typed"
            ctx-ast t))
         n-ast]
        [(PhiT _ t u)
         #:when (equal? t u)
         (assert (not (VarT? t)))
         t]
        [_
         #f]))))
  (f t))

;; Uses the 'var-h' table to substitute any VarT nodes in the type
;; fields and annotations of the program tree 'ast' with concrete type
;; expressions. For nodes that end up with the unit type, checks that
;; they are allowed to have such a type.
(define (ast-rm-VarT var-h ast)
  (define (rw ast t)
    (define n-t (type-rm-VarT var-h t ast))
    (when (and (Void-type? n-t) 
               (or (Var? ast) (Literal? ast)
                   (DefVar? ast) (Param? ast)))
      (raise-language-error/ast
       "illegal type for a variable or literal"
       ast n-t))
    n-t)

  (ast-map-type-expr rw ast))

;;; 
;;; API
;;; 

;; Takes a definition table containing just the program, and
;; checks/infers its types. 'defs' itself is used as the type
;; environment. The input may contain AnyT values, long as their
;; meaning can be inferred. Returns a fully typed program, with
;; definitions having resolved type fields (where appropriate), and
;; expressions having resolved 'type' annotations.
(define-with-contract*
  (-> hash? hash?)
  (defs-type-infer defs)
  
  ;;(pretty-print (dict->list defs))

  (define (lookup x) 
    (lookup-type-from-defs defs x))

  (define (lookup-use x) 
    (lookup-use-type-from-defs defs x))

  ;; ApplyExpr `uid`s as keys, ApplyInfo objects as values. Only
  ;; contains entries for return type overloaded calls.
  (define apply-h (make-hasheq))
  
  ;; A mutable fact database of sorts, with VarT symbols as keys, and
  ;; (possibly incomplete) type expressions as values.
  (define var-h (make-hasheq))

  ;; Possibly adds constraints between types 'x' and 'y'. Returns #t
  ;; if the constraints are possibly solvable, and #f otherwise.
  (define (type-unifies!? x y) ;; Type? Type? -> boolean?
    (unify! var-h x y))

  (define (type-unify! x y)
    (unify! var-h x y)
    y)
  
  (define (expr-unify! e t)
    (define e-t (Expr-type e))
    (assert e-t)
    (unless (type-unifies!? e-t t)
      (raise-language-error/ast
       "expression's type does not match its context"
       #:fields (list (list "type of expression"
                            (ast-displayable/datum e-t))
                      (list "type required for context"
                            (ast-displayable/datum t)))
       e))
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
         (unless (type-unifies!? r-t b-t)
           (raise-language-error/ast
            "function return type does not match body expression"
            #:fields (list
                      (list "function" id)
                      (list "declared return type"
                            (ast-displayable/datum r-t))
                      (list "actual return type"
                            (ast-displayable/datum b-t)))
            ast b)))
       (void))
      
      ((DefVar _ id t v)
       (define v-t (ti-expr v))
       (unless (type-unifies!? t v-t)
         (raise-language-error/ast
          "declared variable type does not match value expression"
          #:fields (list (list "declared type"
                               (ast-displayable/datum t))
                         (list "actual type of value"
                               (ast-displayable/datum v-t)))
            ast v))
       (void))
      
      (else
       (raise-argument-error
        'ti-def "supported Def?" ast))))

  (define (ti-stat ast) ;; Stat? -> void?
    (match ast
      ((SeqStat _ ss)
       (for-each ti-stat ss))
      
      ((LetStat _ b ss)
       (ti-def b)
       (for-each ti-stat ss))
      
      ((IfStat _ c t e)
       (define c-t (ti-expr c))
       (unless (type-unifies!? the-Bool-type c-t)
         (raise-language-error/ast
          (format "expected type '~a' for conditional"
                  (Id-name the-Bool-id))
          ast c
          #:fields (list (list "actual type"
                               (ast-displayable/datum c-t)))))
       (ti-stat t)
       (ti-stat e))
      
      ((AssignStat _ lhs rhs)
       (define lhs-t (ti-expr lhs))
       (define rhs-t (ti-expr rhs))
       (unless (type-unifies!? lhs-t rhs-t)
         (raise-language-error/ast
          "assignment between different types"
          ast
          #:fields (list (list "lvalue type" (ast-displayable/datum lhs-t))
                         (list "rvalue type" (ast-displayable/datum rhs-t)))))
       (void))
      
      ((? VoidStat?)
       (void))
      
      (else
       (raise-argument-error
        'ti-stat "supported Stat?" ast))))
      
  (define (ti-expr-seq ast-lst) ;; (listof Ast?) -> Type?
    (define len (length ast-lst))
    (assert (> len 0))
    (let-values (((heads tail) (split-at ast-lst (- len 1))))
      (for-each ti-expr heads)
      (let ((t (ti-expr (car tail))))
        t)))
  
  (define (ti-expr ast) ;; Ast? -> Type?
    (match ast
      ((SeqExpr _ es)
       (define t (ti-expr-seq es))
       (expr-unify! ast t))
      
      ((LetExpr _ b es)
       (ti-def b)
       (define t (ti-expr-seq es))
       (expr-unify! ast t))
      
      ((? Var?)
       (define t (lookup ast))
       (when (FunT? t)
         (raise-language-error/ast
          "reference to a function as a value"
          #:fields (list (list "type" (ast-displayable t)))
          ast))
       (expr-unify! ast t))

      ((ApplyExpr a f as)
       ;; We bypass invoking (ti-expr f) here, as we only want to
       ;; allow FunT typed expressions in this context. We must still
       ;; be sure to set up a constraint for the expression 'f', lest
       ;; its type be left unresolved.
       (define-values (info f-t) (lookup-use f))
       (when info
         (hash-set! apply-h (hash-ref a 'uid) info))
       ;;(writeln (list f bind->sym))
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
       ;; to 'unify!', but we get better error messages by doing some
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
         (unless (type-unifies!? p-t e-t)
           (raise-language-error/ast
            "parameter type does not match that of argument"
            #:fields (list (list "parameter type"
                                 (ast-displayable p-t))
                           (list "argument type"
                                 (ast-displayable e-t))
                           (list "argument type (AST)"
                                 e-t))
            ast e)))

       ;; The type of the ApplyExpr expression must unify with the
       ;; return type of the function.
       (define t (FunT-rt f-t))
       ;;(writeln (type-concretely var-h t))
       (expr-unify! ast t))

      ((Literal _ dat)
       ;; May have an explicit type annotation, instead of an
       ;; auto-assigned type variable. In any case, we cannot learn
       ;; anything new here, not unless the literal is of the built-in
       ;; boolean type.
       (cond
        ((boolean? dat)
         (expr-unify! ast the-Bool-type))
        (else
         (define l-t (Expr-type ast))
         (assert l-t)
         l-t)))

      ((IfExpr _ c t e)
       (define c-t (ti-expr c))
       (unless (type-unifies!? the-Bool-type c-t)
         (raise-language-error/ast
          (format "expected type '~a' for conditional"
                  (Id-name the-Bool-id))
          ast c
          #:fields (list (list "actual type"
                               (ast-displayable/datum 
                                (type-concretely var-h c-t))))))
       (define t-t (ti-expr t))
       (define e-t (ti-expr e))
       (define discarded? (get-result-discarded ast))
       (define ast-t (Expr-type ast))
       (cond
        [(and discarded?
              (cond
               ((VarT? ast-t) 
                ;; Overall IfExpr type does not matter, and branch
                ;; expression types need not unify.
                #t)
               ((NameT? ast-t) 
                ;; A type for the IfExpr has been given explicitly,
                ;; and branches must also be of that type.
                #f)
               (else
                (raise-language-error/ast
                 "unexpected type for conditional"
                 ast ast-t))))
         (define n-t (annoless PhiT t-t e-t))
         (type-unify! ast-t n-t)]
        [else
         (unless (type-unifies!? t-t e-t)
           (raise-language-error/ast
            "expected same type for both 'if' branches"
            ast
            #:fields (list
                      (list "THEN branch type" (ast-displayable/datum t-t))
                      (list "ELSE branch type" (ast-displayable/datum e-t))
                      )))
         (type-unify! ast-t t-t)]))

      ;; Statements can appear in an expression position. They are
      ;; always of the unit type.
      ((? Stat?)
       (ti-stat ast)
       the-Void-type)

      ((? RacketExpr?)
       (Expr-type ast))
      
      (else
       (raise-argument-error
        'ti-expr "supported ExprLike?" ast))))

  (set! defs (defs-add-VarT defs))
  (defs-for-each/bind ti-def defs)
  (set! defs (defs-map/bind (curry ast-rm-VarT var-h) defs))
  (unless (hash-empty? apply-h)
    (set! defs (defs-add-type<> var-h apply-h defs)))
  defs)
