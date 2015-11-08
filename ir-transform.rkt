#lang racket/base

#|

|#

(require racket/contract/base
         racket/dict
         racket/function racket/list
         racket/match
         racket/pretty
         racket/set
         "ir-ast.rkt"
         "strategy.rkt"
         "strategy-stratego.rkt"
         "strategy-term.rkt"
         "util.rkt" "util/field.rkt")

;;; 
;;; definition table management
;;;

(define* (local-Defun? ast)
  (and (Defun? ast)
       (not (ast-anno-maybe ast 'top))))

;; In IR we do not allow global DefVars.
(define* (ast-local-def? ast)
  (any-pred-holds
   Param? DefVar? local-Defun?
   ast))

;; This pass may be used to synchronize the definitions table with
;; updated local definitions (such as Param) after changes have been
;; made within the global definitions. The 'ast-identifier-put'
;; operation shall be used to update the table - its abstract
;; signature is (-> table id Def? table).
(define* (defs-table-update-locals/Id defs)
  (define f
    (topdown-visitor
     (lambda (ast)
       (when (ast-local-def? ast)
         (define id (Def-id ast))
         (set! defs (ast-identifier-put defs id ast))))))
  
  (for (((id def) (in-dict defs)))
    (unless (ast-local-def? def)
      (f def)))

  defs)

(define* (build-full-defs-table tl-def-lst
                           #:init [defs #hasheq()]
                           #:put [put ast-identifier-put])
  (define (put! def)
    (set! defs (put defs (Def-id def) def)))
  
  (define f
    (topdown-visitor
     (lambda (ast)
       (when (Def? ast)
         (put! ast)))))
  
  (for-each f tl-def-lst)

  defs)

(define* (build-tl-defs-table tl-def-lst)
  (for/hasheq ([def tl-def-lst])
    (values (Id-bind (Def-id def)) def)))

;;; 
;;; names
;;; 

;; If 'ast' is a definition or a name reference, returns the
;; identifier being bound or referenced.
(define* (binding-or-use-id ast)
  (cond
   ((Def? ast) (Def-id ast))
   ((Var? ast) (Var-id ast))
   ((NameT? ast) (NameT-id ast))
   (else #f)))

(define-with-contract*
  (-> Ast? (or/c Id? #f))
  (name-ref-id/maybe ast)
  (cond
   ((Var? ast) (Var-id ast))
   ((NameT? ast) (NameT-id ast))
   (else #f)))

;;; 
;;; foreign names
;;; 

(define* (get-export-name x)
  (cond
   ((hash? x) (hash-ref x 'export #f))
   ((Def? x) (ast-anno-maybe x 'export))
   (else
    (raise-argument-error
     'get-export-name
     "(or/c hash? Def?)" x))))

(define* (get-foreign-name x)
  (cond
   ((hash? x) (hash-ref x 'foreign #f))
   ((Def? x) (ast-anno-maybe x 'foreign))
   (else
    (raise-argument-error
     'get-foreign-name
     "(or/c hash? Def?)" x))))

;;; 
;;; types
;;; 

(define (NameT-from-id id)
  (ast-annotated id NameT id))

(define* (def-get-type def)
  (match def
    ((DefVar _ _ t _)
     t)
    ((Param _ _ t)
     t)
    ((Defun _ _ t _ _)
     t)
    ((ForeignTypeDecl _ id _)
     (NameT-from-id id))))

(define* (def-set-type def t)
  (match def
    ((DefVar a id _ v)
     (DefVar a id t v))
    ((Param a id _)
     (Param a id t))
    ((Defun a id _ ps b)
     (Defun a id t ps b))
    ((? ForeignTypeDecl?)
     def)))

;; Applies `f` to each type expression in `ast`, once, in an
;; unspecified order (skips nodes that have no type, or whose type
;; cannot be modified). Also passes typed node to `f`, as information
;; only. Variations of this may be implemented in terms of
;; ast-get-nonfixed-type and ast-set-nonfixed-type as necessary.
(define* (ast-map-type-expr f ast)
  (let rw ((ast ast))
    (match ast
      ((DefVar a id t v)
       (DefVar a id (f ast t) (rw v)))
      ((Param a id t)
       (Param a id (f ast t)))
      ((Defun a id t ps b)
       (Defun a id (f ast t) (map rw ps) (rw b)))
      ((Expr t)
       #:when t
       (Expr-copy (term-rewrite-all rw ast) (f ast t)))
      (_
       (term-rewrite-all rw ast)))))

;; If the type expression `ast` specifies a function type, returns its
;; function type without any enclosing type parameter declarations.
(define-with-contract*
  (-> Type? (or/c FunT? #f))
  (type-expr-FunT-type ast)

  (cond
    ((FunT? ast)
     ast)
    ((ForAllT? ast)
     (type-expr-FunT-type (ForAllT-t ast)))
    ((ExistsT? ast)
     (type-expr-FunT-type (ExistsT-t ast)))
    (else
     #f)))

;;; 
;;; existential types
;;; 

;; The `t-expr` argument must be a full type expression. This is
;; required so that all the type variables associated with any ExistsT
;; within `t-expr` get processed here.
(define-with-contract*
  (-> Type? (values hash? Type?))
  (type-expr-rm-ExistsT t-expr)
  
  (define bind->sym (make-hasheq))

  (define n-ast
    (let loop ((ast t-expr))
      (match ast
        ((NameT a (Id _ _ bind))
         ;;(writeln `(,ast when ,bind->sym))
         (define sym (hash-ref bind->sym bind #f))
         (if sym
             (VarT a sym)
             ast))
        ((ExistsT _ ns t)
         (for ((n ns))
           (match-define (NameT _ (Id _ name bind)) n)
           (assert (not (hash-has-key? bind->sym bind)))
           (hash-set! bind->sym bind (gensym name)))
         ;;(writeln bind->sym)
         (loop t))
        (_
         (term-rewrite-all loop ast)))))

  (values bind->sym n-ast))

(define* (ast-rm-ExistsT def)
  (define (f dummy t-expr)
    (define-values (bind->sym n-ast)
      (type-expr-rm-ExistsT t-expr))
    n-ast)
  (ast-map-type-expr f def))

;;; 
;;; universal types
;;; 

;; Whether the type expression `in-t` is a function type whose return
;; type contains universal types not appearing in formal argument
;; types.
(define-with-contract*
  (-> Type? boolean?)
  (type-expr-return-type-overloaded? in-t)
  
  (define univ-binds (mutable-seteq))

  (define (univ-bind? bind)
    (set-member? univ-binds bind))
  
  (define r-binds (mutable-seteq))
  (define a-binds (mutable-seteq))

  (define (r-visit ast)
    (match ast
      [(NameT _ (Id _ _ (? univ-bind? bind)))
       (set-add! r-binds bind)]
      [(ForAllT _ ns t)
       (for ([n ns])
         (match-define (NameT _ (Id _ _ bind)) n)
         (set-add! univ-binds bind))
       (r-visit t)]
      [_
       (term-rewrite-all r-visit ast)]))
  
  (define (a-visit ast)
    (match ast
      [(NameT _ (Id _ _ (? univ-bind? bind)))
       (set-add! a-binds bind)]
      [_
       (term-rewrite-all a-visit ast)]))

  (define (top-visit ast)
    (match ast
      [(FunT _ ats rt)
       (unless (set-empty? univ-binds)
         (r-visit rt)
         (for-each a-visit ats))]
      [(ForAllT _ ns t)
       (for ([n ns])
         (match-define (NameT _ (Id _ _ bind)) n)
         (set-add! univ-binds bind))
       (top-visit t)]
      [(ExistsT _ _ t)
       (top-visit t)]
      [_
       (void)]))

  (top-visit in-t)
  (set-subtract! r-binds a-binds)
  (not (set-empty? r-binds)))

;; Rewrites the `in-t` type expression to remove ForAllT quantifiers.
;; Also returns a list of appearing universal types, preserving their
;; order.
(define-with-contract*
  (-> Type? (values (listof NameT?) Type?))
  (type-expr-rm-ForAllT/def in-t)
  
  (define univ-names '())
  (define univ-binds (mutable-seteq))
  
  (define (univ-bind? bind)
    (set-member? univ-binds bind))

  (define (flag-type-param a)
    (hash-set a 'type-param #t))
  
  (define n-t
    (let loop ([ast in-t])
      (match ast
        [(NameT a (and (Id _ _ (? univ-bind?)) id))
         (NameT (flag-type-param a) id)]
        [(ForAllT _ ns t)
         (for ([n ns])
           (match-define (NameT a (and (Id _ _ bind) id)) n)
           (set-add! univ-binds bind)
           (set! univ-names (cons (NameT (flag-type-param a) id) univ-names)))
         (loop t)]
        [_
         (term-rewrite-all loop ast)])))
  
  (values (reverse univ-names) n-t))

;; Replaces all universal types in `in-t` with specific types, which
;; in practice will be fresh `VarT` nodes. The appearing `ForAllT`
;; quantifiers are removed. Also returns a NameT `bind` value -> VarT
;; symbol mapping, which can be used to relate the original universal
;; types to their instantiations.
(define-with-contract*
  (-> Type? (values hash? Type?))
  (type-expr-rm-ForAllT/use in-t)
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(ForAllT a ns t)
          (ExistsT a ns t)]
         [_ ast]))))

  (type-expr-rm-ExistsT (rw in-t)))

;;; 
;;; ExprLike annotations
;;; 

(define-Ast-anno-accessors*
  result-discarded
  get-result-discarded
  set-result-discarded)

(define (annos-set-result-discarded h discarded)
  (hash-set h 'result-discarded discarded))

;; Flags expressions whose results are not required by their context,
;; and will thus be discarded. Assumes that sequences have been
;; spliced. It is a syntax error if empty `SeqExpr` nodes appear in
;; the input where a value is required. We also assume that dead code
;; has been dropped, since the results of such code will not be
;; required (of course correct annotations may be pointless in nodes
;; that will subsequently be dropped).
(define* (ast-update-ExprLike-result-annos ast)
  (define (rw-expr-used ast)
    (rw-expr #f ast))
  
  (define (rw-expr-discarded ast)
    (rw-expr #t ast))

  (define (rw-expr-seq d? ast es)
    (match es
      [(list) 
       (unless d?
         (raise-language-error/ast
          "unallowed position for an empty expression sequence" ast))
       (values es 0 #f)]
      [(list xs ... y)
       (define n-xs (map rw-expr-discarded xs))
       (define n-y (rw-expr d? y))
       (values (append n-xs (list n-y))
               (get-result-discarded n-y))]
      [_
       (assert #f)]))
  
  (define (rw-expr d? ast)
    (match ast
      [(or (? Var?) (? Literal?) (? RacketExpr?))
       (set-result-discarded ast d?)]
      [(or (? ApplyExpr?) (? AssignExpr?))
       (define n-ast (term-rewrite-all rw-expr-used ast))
       (set-result-discarded n-ast d?)]
      [(IfExpr a c t e)
       (define n-ast (IfExpr a (rw-expr-used c)
                             (rw-expr d? t)
                             (rw-expr d? e)))
       (set-result-discarded n-ast d?)]
      [(VoidExpr a)
       (VoidExpr (annos-set-result-discarded a d?))]
      [(SeqExpr a es)
       (define-values (n-es di) (rw-expr-seq d? ast es))
       (SeqExpr (annos-set-result-discarded a di) n-es)]
      [(LetExpr a dv es)
       (define-values (n-es di) (rw-expr-seq d? ast es))
       (LetExpr (annos-set-result-discarded a di) (rw-any dv) n-es)]
      [else
       (assert (ExprLike? ast))
       (error 'update-ExprLike-result-annos
              "unimplemented for expression ~s" ast)]))
  
  (define (rw-any ast)
    (if (ExprLike? ast)
        (rw-expr-used ast)
        (term-rewrite-all rw-any ast)))

  (rw-any ast))

;;; 
;;; data-flow analysis
;;; 

;; A Φ value type, as in compiler literature. The `set` is a set of
;; value numbers. Each number is a symbol of the form 'vn* for an
;; actual value, or 'nothing to indicate no value assignment.
(concrete-struct* Phi (set) #:transparent)

;; Sums together value numbers `xs`. Where `xs` are all the same
;; number, returns the number. Otherwise returns Phi(x ...), where `x`
;; are all distinct numbers.
(define* (val-num+ . xs)
  (define vs
    (for/fold ((sum (seteq))) ((x xs))
      (if (Phi? x)
          (set-union sum (Phi-set x))
          (set-add sum x))))
  (if (= (set-count vs) 1)
      (set-first vs)
      (Phi vs)))

(define* (env-val-num-merge h1 h2)
  (define keys (list->mutable-seteq (hash-keys h1)))
  (for ([k (hash-keys h2)])
    (set-add! keys k))
  (for/fold ((h #hasheq())) ((k (in-set keys)))
    (define v1 (hash-ref h1 k 'nothing))
    (define v2 (hash-ref h2 k 'nothing))
    (hash-set h k (val-num+ v1 v2))))
    
;;; 
;;; copy and constant propagation
;;;

(define* (fun-propagate-copies def
                               #:noop [noop the-VoidExpr])
  (define to-examine null) ;; (listof (list/c val-num lv rv))
  
  (define (annos-add-val-num! a lv-id [rv-ast #f])
    (define val-num (gensym 'vn))
    (define n-a (hash-set a 'val-num val-num))
    (when (and rv-ast (any-pred-holds Var? Literal? rv-ast))
      (define item (list val-num lv-id rv-ast))
      (set! to-examine (cons item to-examine)))
    n-a)
  
  ;; Assigns a value number for each "assignment", and adds potentials
  ;; for removal to `to-examine`. Note that: a `DeclVar` has no value;
  ;; and a `Param` has some value, but no expression giving it.
  (define assign-val-nums
    (topdown
     (lambda (ast)
       (match ast
         [(AssignStxp a lv rv)
          (cond
            [(and (Var? rv) (equal? lv rv))
             ;; Special case of `x := x`, so can remove
             ;; unconditionally.
             noop]
            [else
             (define n-a (annos-add-val-num! a (Var-id lv) rv))
             (set-AssignStxp-annos ast n-a)])]
         [(DefVar a id t rv)
          (define n-a (annos-add-val-num! a id rv))
          (DefVar n-a id t rv)]
         [(Param a id t)
          (define n-a (annos-add-val-num! a id))
          (Param n-a id t)]
         [_ ast]))))
  
  ;; Rewrites `def` to remove the assignment identified by
  ;; `examine-item`, or fails returning #f. Traverses `def` in
  ;; execution order in order to do data-flow analysis.
  (define (rw-by-item examine-item def)
    ;;(writeln `(examining ,examine-item))

    (match-define (list tgt-num tgt-lv-id tgt-rv-ast) examine-item)

    (define tgt-rv-id ;; true for Vars, not Literals
      (match tgt-rv-ast
        [(Var _ id) id]
        [(? Literal?) #f]))
    (define tgt-rv-bind (and tgt-rv-id (Id-bind tgt-rv-id)))
    (define tgt-rv-num #f) ;; determined later if `tgt-rv-id`

    (define (maybe-set-rv-num! bind->num rv-ast)
      (when (Var? rv-ast)
        (define rv-bind (Id-bind (Var-id rv-ast)))
        (set! tgt-rv-num (hash-ref bind->num rv-bind))))
    
    ;; For each Goto target (indexed by `bind` value), a sum of their
    ;; bind->num assignments. All Gotos to a LabelDef will have been
    ;; seen before we reach the LabelDef, and hence the set of
    ;; assignments will be complete at that point.
    (define label->bind->num (make-hasheq))

    (define (rw bind->num ast)
      ;;(writeln `(rw of ,ast when ,bind->num))
      (match ast
        [(AssignStxp a lv rv)
         (define this-num (hash-ref a 'val-num))
         (assert (Var? lv))
         (define lv-id (Var-id lv))
         (define lv-bind (Id-bind lv-id))
         (values (hash-set bind->num lv-bind this-num)
                 (cond
                  [(and (eq? this-num tgt-num)
                        (Id-bind=? tgt-lv-id lv-id))
                   ;;(writeln `(deleting ,this-num : ,lv-id := ,rv))
                   (maybe-set-rv-num! bind->num rv)
                   noop]
                  [else
                   (define n-rv (rw-discard bind->num rv)) 
                   (and n-rv (set-AssignStxp-rv ast n-rv))]))]
        [(DefVar a lv-id t rv)
         (define this-num (hash-ref a 'val-num))
         (define lv-bind (Id-bind lv-id))
         (values (hash-set bind->num lv-bind this-num)
                 (cond
                   [(eq? this-num tgt-num)
                    ;;(writeln `(deleting ,this-num : ,lv-id := ,rv))
                    (maybe-set-rv-num! bind->num rv)
                    (DeclVar a lv-id t)]
                   [else
                    (define n-rv (rw-discard bind->num rv))
                    (and n-rv (DefVar a lv-id t n-rv))]))]
        [(Param a id t)
         (define this-num (hash-ref a 'val-num))
         (assert (not (eq? this-num tgt-num)))
         (define lv-bind (Id-bind id))
         (values (hash-set bind->num lv-bind this-num) ast)]
        [(IfStxp a c t e)
         (define n-c (rw-discard bind->num c))
         (cond
           [(not n-c)
            (values bind->num #f)]
           [else
            (define-values (t-st n-t) (rw bind->num t))
            (cond 
              [(not n-t)
               (values bind->num #f)]
              [else
               (define-values (e-st n-e) (rw bind->num e))
               (cond
                 [(not n-e)
                  (values bind->num #f)]
                 [else
                  (values (env-val-num-merge t-st e-st)
                          (IfStxp-copy ast a n-c n-t n-e))])])])]
        [(Var a (? (fix Id-bind=? tgt-lv-id) id))
         (define this-bind (Id-bind id))
         (define this-num (hash-ref bind->num this-bind))
         ;;(writeln `(examining Var id= ,id num= ,this-num rv= ,tgt-rv-ast num= ,(hash-ref bind->num tgt-rv-bind #f)))
         (assert (not (eq? this-num 'nothing))) ;; undefined, cannot use
         (values bind->num
                 (cond
                   ;; The target assignment is in effect here, so we
                   ;; must substitute the R-value variable or literal,
                   ;; but this only works if it still has the same
                   ;; R-value (literals obviously do).
                   [(eq? this-num tgt-num)
                    (cond
                      [tgt-rv-bind ;; Var
                       (assert tgt-rv-num)
                       (define this-rv-num (hash-ref bind->num tgt-rv-bind))
                       (unless this-rv-num
                         (raise-assertion-error
                          'fun-propagate-copies
                          "no value binding for R-value: ~a := ~a (~a)"
                          tgt-lv-id tgt-rv-id this-num))
                       (and (eq? this-rv-num tgt-rv-num) tgt-rv-ast)]
                      [else ;; Literal
                       tgt-rv-ast])]
                   ;; The target assignment might be in effect here,
                   ;; but we don't know if it is, and hence cannot make
                   ;; this optimization.
                   [(and (Phi? this-num)
                         (set-member? (Phi-set this-num) tgt-num))
                    #f]
                   ;; The target assignment is not in effect here, so
                   ;; this use is unaffected.
                   [else
                    ast]))]
        [(Goto _ id)
         (define bind (Id-bind id))
         (define lbl-nums 
           (env-val-num-merge
            (hash-ref label->bind->num bind #hasheq()) bind->num))
         (hash-set! label->bind->num bind lbl-nums)
         (values bind->num ast)]
        [(LabelDef _ id)
         (define bind (Id-bind id))
         (define lbl-nums 
           (env-val-num-merge 
            ;; any value assignments from Gotos to this label
            (hash-ref label->bind->num bind #hasheq()) 
            ;; the "fall-in" value assignments
            bind->num))
         (values lbl-nums ast)]
        [_
         ;;(writeln `(no special action for ,ast))
         (rw-all bind->num ast)]))

    ;; Rewrites `ast` and returns the modified AST (or #f), discarding
    ;; changes to assignment table.
    (define (rw-discard bind->num ast)
      (define-values (r n-ast) (rw bind->num ast))
      n-ast)
    
    (define (rw-all bind->num ast)
      (term-rewrite-all/stateful rw bind->num ast))

    (rw-discard #hasheq() def)) ;; end rw-by-item
  
  ;; Tries to rewrite `ast` to remove each of the assignments in
  ;; `to-examine`, but preserves `ast` where removal is not possible.
  (define (examine-all ast)
    ;;(pretty-print to-examine)
    (for/fold ([ast ast]) ([item to-examine])
      (define res (rw-by-item item ast))
      ;;(writeln (list 'TRIED item res))
      (or res ast)))
  
  (examine-all (assign-val-nums def)))

;;; 
;;; dead-code elimination
;;; 

;; Removes unused local definitions. Assumes that there are no type
;; definitions present.
(define-with-contract*
  (-> Def? Def?)
  (def-drop-unused-local-Defs def)

  (define refs (mutable-seteq)) ;; set of bind

  ((topdown-visitor
    (match-lambda
      [(Var _ id)
       (set-add! refs (Id-bind id))]
      [_ (void)]))
   def)

  (define (unreferenced? id)
    (not (set-member? refs (Id-bind id))))

  (define rw
    (alltd
     (lambda (ast)
       (match ast
         [(LetExpr a (fields Def d [id (? unreferenced?)]) body)
          (if (DefVar? d)
              (SeqExpr a (map rw (cons (DefVar-body d) body)))
              (SeqExpr a (map rw body)))]
         [_ #f]))))
  
  (rw def))

;; Removes unused local function definitions.
(define-with-contract*
  (-> Def? Def?)
  (def-drop-unused-local-Defuns def)

  (define refs (mutable-seteq)) ;; set of bind

  ((topdown-visitor
    (match-lambda
      [(fields ApplyExpr [f e])
       (assert (Var? e))
       (define id (Var-id e))
       (set-add! refs (Id-bind id))]
      [_ (void)]))
   def)

  (define (unreferenced? id)
    (not (set-member? refs (Id-bind id))))

  (define rw
    (alltd
     (lambda (ast)
       (match ast
         [(LetExpr a (fields Defun [id (? unreferenced?)]) body)
          (SeqExpr a (map rw body))]
         [_ #f]))))
  
  (rw def))

;; Optimizes by removing some dead constant expressions, i.e. ones
;; that have no side effects, and whose results are not required.
;; Relies of 'result-discarded annotations. May remove useful type
;; information.
(define* (ast-rm-result-discarded-constants in-ast)
  (define a-noop
    (SeqExpr #hasheq((result-discarded . #t)) '()))
  
  (define dead->SeqExpr
    (alltd
     (lambda (ast)
       (cond
        [(and (or (Var? ast) (Literal? ast) 
                  (RacketExpr? ast) (VoidExpr? ast))
              (get-result-discarded ast))
         a-noop]
        [else
         #f]))))
  
  (define optimize
    (alltd
     (lambda (ast)
       (match ast
         [(IfExpr _ c (? empty-SeqExpr?) (? empty-SeqExpr?))
          (optimize c)]
         [else
          #f]))))

  (define voidify
    (topdown
     (lambda (ast)
       (cond
        [(empty-SeqExpr? ast) the-VoidExpr]
        [else ast]))))
  
  (voidify
   (optimize
    (ast-splice-SeqExpr
     (dead->SeqExpr in-ast)))))

;; Removes obviously useless constant expressions, i.e., ones that
;; directly appear in a result-discarded position, are side-effect
;; free, and have no useful type information.
(define* (ast-trim-useless-constants ast)
  (define dead?
    (lambda (e)
      (or (Literal? e)
          (RacketExpr? e)
          (VoidExpr? e)
          (and (Var? e) (not (Expr-typed? e))))))
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(SeqCont (list es ... e))
          #:when (ormap dead? es)
          (define n-es (filter (negate dead?) es))
          (set-SeqCont-ss ast (append n-es (list e)))]
         [else
          ast]))))
  
  (rw (ast-splice-SeqExpr ast)))

(define* (ast-trim-VoidExpr ast)
  ;; Does shallow splicing, too.
  (define (rw-lst ast-lst)
    (append-map
     (lambda (ast)
       (match (rw #f ast)
         [(SeqExpr _ lst) lst]
         [(? VoidExpr?) null]
         [e (list e)]))
     ast-lst))

  (define (fail-at ast)
    (error 'ast-trim-VoidExpr
           "not a value-giving expression: ~a" (ast-~a ast)))

  (define (chk-lst ctx vc? lst)
    (when (and vc? (null? lst))
      (fail-at ctx))
    lst)

  (define (chk ctx vc? ast)
    (when (and vc? (empty-SeqExpr? ast))
      (fail-at ctx))
    ast)
  
  (define (rw-all ast) ;; (-> Ast? Ast?)
    (term-rewrite-all (fix rw #t) ast))
  
  ;; `vc?` = value-expecting context
  (define (rw vc? ast) ;; (-> boolean? Ast? Ast?)
    (define n-ast
      (match ast
        [(SeqExpr a lst)
         (let ((lst (rw-lst lst)))
           (chk-lst ast vc? lst)
           (match lst
             [(list e) e]
             [_ (SeqExpr a lst)]))]
        [(LetExpr a def lst)
         (let ((lst (rw-lst lst)))
           (chk-lst ast vc? lst)
           (LetExpr a (rw #t def) lst))]
        [(IfExpr a c t e)
         (let ((c (rw #t c))
               (t (rw #t t))
               (e (rw #t e)))
           (if (equal? t e)
               (SeqExpr a (list c t))
               (IfExpr a c t e)))]
        [else
         (rw-all ast)]))
    (chk ast vc? n-ast))

  (rw #t ast))

;;; 
;;; simplification
;;; 

;; Where a SeqExpr appears within another SeqCont of some kind,
;; inlines the SeqExpr contents within the outer container.
(define* ast-splice-SeqExpr
  (topdown
   (repeat
    (lambda (ast)
      (match ast
        [(SeqExpr _ (list e))
         e]
        [(SeqCont ss)
         #:when (ormap SeqExpr? ss)
         (define n-ss
           (apply append (for/list ((s ss))
                           (if (SeqExpr? s)
                               (SeqExpr-ss s)
                               (list s)))))
         (set-SeqCont-ss ast n-ss)]
        [else
         #f])))))
       
(define* ast-simplify-multi-innermost
  (innermost-rewriter
   (match-lambda
    [(IfExpr a c t e)
     #:when (equal? t e)
     (SeqExpr a (list c t))]
    [else #f])))

;;; 
;;; type-specific literal formatting
;;;

;; The `defs-t` argument is a definition lookup table, for looking up
;; type (and formatting) information.
(define-with-contract*
  (-> hash? list? list?)
  (defs-set-formats-to-Literals defs-t def-lst)
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(Literal (and (app (lambda (h) (hash-ref h 'type))
                             (? NameT? t-ref)) a) dat)
          (define bind (Id-bind (NameT-id t-ref)))
          (define t-def (hash-ref defs-t bind #f))
          (unless t-def
            (raise-language-error/ast
             "reference to undefined type" ast t-ref))
          (define lit (ast-anno-maybe t-def 'literal))
          (if lit
              (Literal (hash-set a 'literal lit) dat)
              ast)]
         [_ ast]))))
  
  (map rw def-lst))

;;; 
;;; AST dumping
;;;

(define* (ast->list ast (annos null))
  (define lst null)
  ((topdown-visitor
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
