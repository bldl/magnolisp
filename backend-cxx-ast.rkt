#lang racket/base

#|
|#

(require "ir-ast.rkt"
         "ast-repr.rkt"
         "util.rkt")

;;; 
;;; type expressions
;;;

(define-ast* ConstT (Ast Type) 
  ((#:none annos) (#:just t)))

(define-ast* RefT (Ast Type) 
  ((#:none annos) (#:just t)))

;;; 
;;; declarations and directives
;;;

;; kind is either 'user or 'system.
(define-ast* Include (Ast) ((#:none annos) (#:none kind) (#:none s)))

;; `rtype` is the return type, only. `s` is the body statement, which
;; should be a `CxxBlockStat` for printing, or it can be `NoBody` also.
(define-ast* CxxDefun (Ast Def) ((#:none annos) (#:none id)
                                 (#:none modifs) (#:just rtype)
                                 (#:many params) (#:just s)))

;; A C++ function prototype declaration. No body, and some modifiers
;; may have to be different compared to the function definition.
(define-ast* Proto (Ast Def) ((#:none annos) (#:none id)
                              (#:none modifs) (#:just rtype)
                              (#:many params)))

;; Top-level verbatim string.
(define-ast* TlVerbatim (Ast) 
  ((#:none annos) (#:none s)))

;;; 
;;; statements
;;; 

;; Sequence of statements.
(define-ast* CxxBlockStat (Ast Stat SeqCont) 
  ((#:none annos) (#:many ss)))

(define-ast* ReturnStat (Ast Stat) ((#:none annos) (#:just e)))

(define-ast* PpCxxIfStat (Ast) ((#:none annos) (#:just c)
                                (#:many ts) (#:many es)))

;;; 
;;; expressions
;;; 

;; An expression whose value is given by variable `id`, and assigned
;; to by the statement sequence `ss`, except where the expression has
;; unit type. The variable will be automatically declared upon
;; lifting, and the statements will be lifted to a suitable context.
;; The result should always get assigned to by the statements, at
;; least if the lifted expression is ever to be evaluated.
(define-ast* LiftStatExpr (Ast Expr SeqCont) 
  ((#:none annos) (#:none id) (#:many ss)))
