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
       ;; definite escape to `k` or beyond, we add such an escape as
       ;; the last thing to evaluate within `es`.
       (define k-id (Var-id k))
       (define target (Id-bind k-id))
       (parameterize ((scopes (cons target (scopes))))
         (let-values (((es-target es) (rw-lst es)))
           (cond
            (es-target
             (unless (memq es-target (scopes))
               (error 'ast-normalize-LetLocalEc
                      "escapes ~a beyond context ~a"
                      es-target (scopes)))
             (values (and (not (eq? es-target target)) es-target)
                     (LetLocalEc a k es)))
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
          
  (rw-any-LetLocalEc in-ast))

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
     (ast-identifier=? k1 k2)
     e]
    [(IfExpr a c t e)
     #:when (equal? t e)
     (SeqExpr a (list c t))]
    [else #f])))

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
