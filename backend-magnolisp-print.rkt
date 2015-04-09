#lang racket/base

#|

A routine for dumping IR in a textual format resembling Magnolisp
source code.

|#

(require "ast-magnolisp.rkt" "backend-util.rkt" 
         (rename-in "pp-yield.rkt" [pp pp-y])
         "util.rkt"
         racket/format racket/list racket/match 
         racket/port racket/pretty)

(define (pp-id id) ;; Id? -> spec
  (symbol->string (Id-name id)))

(define (pp-let-def ast) ;; DefVar? -> spec
  (match ast
    ((DefVar _ id t v)
     `("(" (in (gr ,(pp-id id) sp ,(pp-expr v))) ")"))

    (else
     (~s ast))))

(define (pp-expr ast) ;; Ast? -> spec
  (match ast
    ((Var _ id)
     (pp-id id))

    ((Literal _ dat)
     (~s dat))

    ((IfExpr _ c t e)
     `("(if " (in (gr ,(pp-expr c) sp 
                      ,(pp-expr t) sp 
                      ,(pp-expr e))) ")"))

    ((LetExpr _ def es)
     `("(let (" ,(pp-let-def def) ")" 
       (in (gr ,(for/list ((e es))
                  `(sp ,(pp-expr e))))) ")"))

    ((ApplyExpr _ f as)
     `("(" (in (gr ,(pp-expr f)
                   ,(for/list ((e as))
                      `(sp ,(pp-expr e))))) ")"))
    
    (else
     (~s ast))))

(define (pp-def ast) ;; Def? -> spec
  (match ast
    ((Param a id t)
     (pp-id id))
    
    ((Defun a id t ps b)
     (list "(define" 'sp
           "(" (add-between 
                (cons (pp-id id)
                      (map pp-def ps))
                'sp) ")"
                (and (not (NoBody? b)) 
                     `(in (br ,(pp-expr b))))
                ")"))
    
    ((DefVar _ id t v)
     `("(define " ,(pp-id id) " " ,(pp-expr v) ")"))
    
    (else
     (~s ast))))

(define* (pp-mgl ast-lst)
  (define spec
    (add-between (map pp-def ast-lst) '(br br)))
  ;;(pretty-print spec)
  (call-with-output-string
   (lambda (out)
     (define (yield s)
       (display s out))
     (pp-y #:yield yield spec))))
