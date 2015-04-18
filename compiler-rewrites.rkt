#lang racket

#|

|#

(require "ast-magnolisp.rkt" "ast-view.rkt"
         "app-util.rkt" "strategy.rkt" "util.rkt"
         syntax/id-table)

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
    (topdown-visit
     (lambda (ast)
       (when (ast-local-def? ast)
         (define id (Def-id ast))
         (set! defs (ast-identifier-put defs id ast))))))
  
  (for (((id def) (in-dict defs)))
    (unless (ast-local-def? def)
      (f def)))

  defs)

(define* (build-defs-table tl-def-lst
                           #:init [defs #hasheq()]
                           #:put [put ast-identifier-put])
  (define (put! def)
    (set! defs (put defs (Def-id def) def)))
  
  (define f
    (topdown-visit
     (lambda (ast)
       (when (Def? ast)
         (put! ast)))))
  
  (for-each f tl-def-lst)

  defs)

(define* (build-global-defs-table tl-def-lst)
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
;;; exports
;;; 

(define* (get-export-name x)
  (cond
   ((hash? x) (hash-ref x 'export #f))
   ((Def? x) (ast-anno-maybe x 'export))
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
;;; types
;;; 

(define* (NameT-from-id id)
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
       (Expr-copy (all-rw-term rw ast) (f ast t)))
      (_
       (all-rw-term rw ast)))))

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
         (all-rw-term loop ast)))))

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
       (all-rw-term r-visit ast)]))
  
  (define (a-visit ast)
    (match ast
      [(NameT _ (Id _ _ (? univ-bind? bind)))
       (set-add! a-binds bind)]
      [_
       (all-rw-term a-visit ast)]))

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
         (all-rw-term loop ast)])))
  
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

;; Flags expression whose results are not required by their context,
;; and will thus be discarded. Assumes that sequences have been
;; spliced. It is a syntax error if empty SeqExpr nodes appear in the
;; input where a value is required. We also assume that dead code
;; (particularly after AppLocalEc) has been dropped, since the results
;; of such code will not be required (of course correct annotations
;; may be pointless in nodes that will subsequently be dropped).
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
      [(or (? ApplyExpr?) (? AssignStat?))
       (define n-ast (all-rw-term rw-expr-used ast))
       (set-result-discarded n-ast d?)]
      [(IfExpr a c t e)
       (define n-ast (IfExpr a (rw-expr-used c)
                             (rw-expr d? t)
                             (rw-expr d? e)))
       (set-result-discarded n-ast d?)]
      [(VoidStat a)
       (VoidStat (annos-set-result-discarded a d?))]
      [(SeqExpr a es)
       (define-values (n-es di) (rw-expr-seq d? ast es))
       (SeqExpr (annos-set-result-discarded a di) n-es)]
      [(LetExpr a dv es)
       (define-values (n-es di) (rw-expr-seq d? ast es))
       (LetExpr (annos-set-result-discarded a di) (rw-any dv) n-es)]
      [(LetLocalEc a k es)
       (define-values (n-es di) (rw-expr-seq d? ast es))
       (LetLocalEc (annos-set-result-discarded a d?)
                   (rw-expr-used k) n-es)]
      [(AppLocalEc a k e)
       (AppLocalEc (annos-set-result-discarded a #t)
                   (rw-expr-used k) (rw-expr-used e))]
      [else
       (assert (ExprLike? ast))
       (error 'update-ExprLike-result-annos
              "unimplemented for expression ~s" ast)]))
  
  (define (rw-any ast)
    (if (ExprLike? ast)
        (rw-expr-used ast)
        (all-rw-term rw-any ast)))

  (rw-any ast))

;; Optimizes by removing some dead constant expressions, i.e. ones
;; that have no side effects, and whose results are not required.
;; Relies of 'result-discarded annotations.
(define* (ast-rm-dead-constants in-ast)
  (define a-noop
    (SeqExpr #hasheq((result-discarded . #t)) '()))
  
  ;; We assume `in-ast` had none of these.
  (define (noop? ast)
    (matches? ast (SeqExpr _ (list))))
  
  (define dead->SeqExpr
    (topdown
     (lambda (ast)
       (cond
        ((and (or (Var? ast) (Literal? ast) 
                  (RacketExpr? ast) (VoidStat? ast))
              (get-result-discarded ast))
         a-noop)
        (else
         ast)))))
  
  (define optimize
    (topdown
     (lambda (ast)
       (match ast
         [(IfExpr _ c (? noop?) (? noop?))
          c]
         [else
          ast]))))

  (define voidify
    (topdown
     (lambda (ast)
       (cond
        [(noop? ast) (annoless VoidStat)]
        [else ast]))))
  
  (voidify
   (optimize
    (ast-splice-SeqExpr
     (dead->SeqExpr in-ast)))))

(define* (ast-trim-dead-constants ast)
  (define dead?
    (lambda (e)
      (or (Literal? e)
          (RacketExpr? e)
          (VoidStat? e)
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

;;; 
;;; simplification
;;; 

(define* (ast-normalize-LetLocalEc in-ast)
  (define (rw-lst ast-lst)
    (define target #f)
    (define r '())
    (let loop ((ast-lst ast-lst))
      (unless (null? ast-lst)
        (define ast (car ast-lst))
        (define-values (st n-ast) (rw ast))
        (set! r (cons n-ast r))
        (if st
            (set! target st)
            (loop (cdr ast-lst)))))
    (values target (reverse r)))
  
  (define scopes (make-parameter '()))
  
  (define (target-sum x y)
    (cond
     ((and x y)
      (let/ec k
        (for ((s (scopes)))
          (when (or (eq? s x) (eq? s y))
            (k s)))
        (error 'ast-normalize-LetLocalEc
               "escapes ~a and ~a beyond context ~a"
               x y (scopes))))
     (else
      #f)))
  
  (define (rw ast)
    (match ast
      [(or (? Var?) (? Literal?) (? RacketExpr?) (? VoidStat?))
       (values #f ast)]
      [(AssignStat a lv rv)
       (let-values ([(target rv) (rw rv)])
         (values target
                 (if target 
                     rv 
                     (AssignStat a lv rv))))]
      [(ApplyExpr a f as)
       (define-values (target n-as) (rw-lst as))
       (values target
               (if target
                   (SeqExpr (annos-remove-type a) n-as)
                   (ApplyExpr a f n-as)))]
      [(IfExpr a c t e)
       (define-values (c-target c-ast) (rw c))
       (if c-target
           (values c-target c-ast)
           (let ()
             (define-values (t-target t-ast) (rw t))
             (define-values (e-target e-ast) (rw e))
             (values (target-sum t-target e-target)
                     (IfExpr a c-ast t-ast e-ast))))]
      [(SeqExpr a es)
       (let-values (((target es) (rw-lst es)))
         (when target (set! a (annos-remove-type a)))
         (values target (SeqExpr a es)))]
      [(LetExpr a (and dv (DefVar dv-a dv-id dv-t dv-e)) es)
       (let-values (((dv-target dv-ast) (rw dv-e)))
         (if dv-target
             (let ()
               (unless (AnyT? dv-t)
                 (set! dv-e (set-ExprLike-type dv-e dv-t)))
               (values dv-target dv-e))
             (let-values (((es-target es) (rw-lst es)))
               (values es-target
                       (LetExpr (annos-remove-type a) dv es)))))]
      [(LetExpr a (? Defun? dv) es)
       (define n-dv (ast-normalize-LetLocalEc dv))
       (let-values (((target es) (rw-lst es)))
         (when target (set! a (annos-remove-type a)))
         (values target (LetExpr a n-dv es)))]
      [(LetLocalEc a k es)
       ;; We normalize so that if `es` does not already end with a
       ;; definite escape to `k` or beyond, we add an escape to `k` as
       ;; the last thing to evaluate within `es`. It follows that if
       ;; there is no escape to `k`, than the corresponding
       ;; `LetLocalEc` block has no value at all.
       (define k-id (Var-id k))
       (define target (Id-bind k-id))
       (parameterize ((scopes (cons target (scopes))))
         (let-values (((es-target es) (rw-lst es)))
           (cond
            (es-target
             (cond
              ((not (memq es-target (scopes)))
               (error 'ast-normalize-LetLocalEc
                      "escapes ~a beyond context ~a"
                      es-target (scopes)))
              ((eq? es-target target)
               (values #f (LetLocalEc a k es)))
              (else ;; always escapes beyond this block
               (values es-target (SeqExpr a es)))))
            (else
             (define n-es (list-map-last
                           (lambda (e) (annoless AppLocalEc k e))
                           es))
             (values #f (LetLocalEc a k n-es))))))]
      [(AppLocalEc a (Var _ k-id) e)
       (let-values (((e-target e-ast) (rw e)))
         (if e-target
             (values e-target e-ast)
             (let ((target (Id-bind k-id)))
               (values target ast))))]
      [else
       (assert (ExprLike? ast))
       (error 'ast-normalize-LetLocalEc
              "unimplemented for expression ~s" ast)]))
  
  (define (rw-any-LetLocalEc ast)
    (cond
     ((LetLocalEc? ast)
      (define-values (target n-ast) (rw ast))
      n-ast)
     (else
      (all-rw-term rw-any-LetLocalEc ast))))
          
  (ast-splice-SeqExpr (rw-any-LetLocalEc in-ast)))

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
  (innermost
   (match-lambda
    [(LetLocalEc _ (Var _ k1) (list (AppLocalEc _ (Var _ k2) e)))
     #:when (ast-identifier=? k1 k2)
     e]
    [(IfExpr a c t e)
     #:when (equal? t e)
     (SeqExpr a (list c t))]
    [else #f])))

;;; 
;;; type-specific literal formatting
;;;

(define* (defs-set-formats-to-Literals defs)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(Literal (and (app (lambda (h) (hash-ref h 'type))
                             (? NameT? t-ref)) a) dat)
          (define bind (Id-bind (NameT-id t-ref)))
          (define t-def (hash-ref defs bind #f))
          (unless t-def
            (raise-language-error/ast
             "reference to undefined type" ast t-ref))
          (define lit (ast-anno-maybe t-def 'literal))
          (if lit
              (Literal (hash-set a 'literal lit) dat)
              ast)]
         [_ ast]))))
  
  (defs-map/bind rw defs))

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
