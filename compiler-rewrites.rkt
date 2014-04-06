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
       (not (ast-anno-must ast 'top))))

;; In IR we do not allow global DefVars.
(define* (ast-local-def? ast)
  (any-pred-holds
   Param? DefVar? local-Defun?
   ast))

;; This pass may be used to synchronize the definitions table with
;; updated local definitions (such as Param) after changes have been
;; made within the global definitions. The 'put' operation shall be
;; used to update the table - its abstract signature is (-> table id
;; Def? table).
(define (defs-table-update-locals put defs)
  (define f
    (topdown-visit
     (lambda (ast)
       (when (ast-local-def? ast)
         (define id (Def-id ast))
         (set! defs (put defs id ast))))))
  
  (for (((id def) (in-dict defs)))
    (unless (ast-local-def? def)
      (f def)))

  defs)

(define* defs-table-update-locals/stx
  (fix defs-table-update-locals dict-set))

(define* defs-table-update-locals/Id
  (fix defs-table-update-locals ast-identifier-put))

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

(define-with-contract*
  (-> list? immutable-free-id-table?)
  (build-defs-table/stx tl-def-lst)
  (build-defs-table tl-def-lst
                    #:init (make-immutable-free-id-table #:phase 0)
                    #:put dict-set))

(define* (build-global-defs-table tl-def-lst)
  (for/hasheq ([def tl-def-lst])
    (values (Id-bind (Def-id def)) def)))

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
    (Def-copy x id))
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

;; If 'ast' is a definition or a name reference, returns the
;; identifier being bound or referenced. No 'def-id' lookup is done.
(define* (binding-or-use-id ast)
  (cond
   ((Def? ast) (Def-id ast))
   ((Var? ast) (Var-id ast))
   ((Label? ast) (Label-id ast))
   ((NameT? ast) (NameT-id ast))
   (else #f)))

(define* (name-ref? ast)
  (or (Var? ast) (NameT? ast)))

(define-with-contract*
  (-> Ast? identifier?)
  (name-ref-id ast)
  (cond
   ((Var? ast) (Var-id ast))
   ((NameT? ast) (NameT-id ast))
   (else (unsupported ast))))

(define-with-contract*
  (-> Ast? (or/c Id? #f))
  (name-ref-id/maybe ast)
  (cond
   ((Var? ast) (Var-id ast))
   ((NameT? ast) (NameT-id ast))
   (else #f)))

;;; 
;;; expressions
;;; 

(define* (ast-expr? ast)
  (any-pred-holds
   Apply?
   BlockExpr?
   IfExpr?
   Literal?
   RacketExpr?
   Var?
   ast))

;;; 
;;; statement containers
;;;

(define* (StatCont? ast)
  (any-pred-holds BlockExpr? BlockStat? LetStat? ast))

(define-match-expander* StatCont
  (syntax-rules ()
    [(_ a ss)
     (or (BlockStat a ss)
         (BlockExpr a ss)
         (LetStat a _ ss))]))

(define* (StatCont-ss ast)
  (match ast
    ((BlockStat _ ss) ss)
    ((BlockExpr _ ss) ss)
    ((LetStat _ _ ss) ss)
    (_ #f)))

(define* (set-StatCont-ss ast n-ss)
  (match ast
    ((BlockExpr a ss)
     (BlockExpr a n-ss))
    ((BlockStat a ss)
     (BlockStat a n-ss))
    ((LetStat a bs ss)
     (LetStat a bs n-ss))))

(define* (StatCont-copy ast n-a n-ss)
  (match ast
    ((BlockExpr a ss)
     (BlockExpr n-a n-ss))
    ((BlockStat a ss)
     (BlockStat n-a n-ss))
    ((LetStat a bs ss)
     (LetStat n-a bs n-ss))))

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
   ((Id? x) (Id-name x))
   (else (unsupported x))))

(define (?->symbol x)
  (and x (->symbol x)))

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
    ((LetStat a ds bs)
     (define n (hash-ref a 'let-kind 'let))
     `(,n (,(ast->sexp ds))
          ,@(map ast->sexp bs)))
    ((BlockStat _ ss)
     `(block-stat ,@(map ast->sexp ss)))
    ((BlockExpr _ ss)
     `(block-expr ,@(map ast->sexp ss)))
    ((Return _ e)
     `(return ,(ast->sexp e)))
    ((Var _ id)
     (->symbol id))
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
     (->symbol id))
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
