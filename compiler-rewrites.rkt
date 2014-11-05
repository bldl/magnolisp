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
;; identifier being bound or referenced. No 'def-id' lookup is done.
(define* (binding-or-use-id ast)
  (cond
   ((Def? ast) (Def-id ast))
   ((Var? ast) (Var-id ast))
   ((Label? ast) (Label-id ast))
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
;;; types
;;; 

(define* (maybe-Expr-type ast)
  (and (Expr? ast) (Expr-type ast)))

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
;;; simplification
;;; 

(define ast-nested-SeqStat->SeqStat
  (topdown
   (repeat
    (lambda (ast)
      (define ss (and (StatCont? ast) (StatCont-ss ast)))
      ;;(when ss (pretty-print `(considering ,ast)))
      (cond
       [(and ss (ormap SeqStat? ss))
        ;;(pretty-print `(simplifying ,ast))
        (define n-ss
          (apply append (for/list ((s ss))
                          (if (SeqStat? s)
                              (SeqStat-ss s)
                              (list s)))))
        (set-StatCont-ss ast n-ss)]
       [else
        ;; Signifies failed strategy.
        #f])))))
       
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
     (define ss (and (StatCont? ast) (StatCont-ss ast)))
     (cond
      [(and ss (ormap Return? ss))
       (define n-ss (take-until/inclusive Return? ss))
       (set-StatCont-ss ast n-ss)]
      [else
       ast]))))

(define ast-simplify-multi-innermost
  (innermost
   (match-lambda
    [(BlockExpr _ (list (Return a e)))
     e]
    [(BlockExpr _ (list (IfStat a c (Return t-a t-e) (Return e-a e-e))))
     (IfExpr a c t-e e-e)]
    [(SeqStat _ (list s))
     s]
    [else #f])))

(define* ast-simplify
  (compose1->
   ast-nested-SeqStat->SeqStat
   ast-rm-dead-code
   ast-simplify-multi-innermost
   ))

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
