#lang racket/base

#|

A routine for dumping IR in a textual format resembling Magnolisp
source code.

|#

(require "ast-ir.rkt" "backend-util.rkt" "compiler-rewrites.rkt"
         (rename-in "pp-yield.rkt" [pp pp-y])
         "util.rkt"
         racket/contract racket/format racket/list 
         racket/match racket/port racket/pretty)

;;; 
;;; pretty printing
;;;

(define (pp-id id) ;; Id? -> spec
  (symbol->string (Id-name id)))

(define (pp-let-def ast) ;; DefVar? -> spec
  (match ast
    ((DefVar _ id t v)
     `("(" (in (gr ,(pp-id id) sp ,(pp-expr v))) ")"))

    ((DeclVar _ id t)
     `("(" (in (gr ,(pp-id id) sp "#<undefined>")) ")"))

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

    ((SeqExpr _ es)
     `("(begin"
       (in (gr ,(for/list ((e es))
                  `(sp ,(pp-expr e))))) ")"))

    ((ApplyExpr _ f as)
     `("(" (in (gr ,(pp-expr f)
                   ,(for/list ((e as))
                      `(sp ,(pp-expr e))))) ")"))

    ((AssignStat _ lv rv)
     `("(set! " (in (gr ,(pp-expr lv) sp 
                        ,(pp-expr rv))) ")"))

    ((VoidStat _)
     "(void)")

    (else
     (~s ast))))

(define (pp-def ast) ;; Def? -> spec
  (match ast
    ((Param a id t)
     (pp-id id))
    
    ((Defun a id t ps b)
     `("(define" sp
       "(" ,(add-between 
             (cons (pp-id id)
                   (map pp-def ps))
             'sp) ")"
       ,(and (not (NoBody? b)) 
             `(in (br ,(pp-expr b))))
       ")"))
    
    ((DefVar _ id t v)
     `("(define " ,(pp-id id) " " ,(pp-expr v) ")"))

    ((ForeignTypeDecl _ id _)
     `("(define #:type " ,(pp-id id) ")"))
    
    (else
     (~s ast))))

;;; 
;;; copy propagation
;;; 

;; Assumes that there are no local functions. Preserves any type
;; annotations.
(define (defs-propagate-copies def-lst)
  (define (fun-optimize def)
    ;;(pretty-print `(BEFORE ,def))
    (set! def (fun-propagate-copies def))
    ;;(pretty-print `(AFTER ,def))
    (set! def (def-drop-dead-local-Defs def))
    (set! def (ast-trim-VoidStat def))
    def)

  (map (lambda (def)
         (if (Defun? def)
             (fun-optimize def)
             def)) 
       def-lst))

;;; 
;;; API
;;;

(define* (pp-mgl ast-lst #:yield [yield display])
  (define spec
    (add-between (map pp-def ast-lst) '(br br)))
  ;;(pretty-print spec)
  (pp-y #:yield yield spec))

(define* (pp-mgl-to-string ast-lst)
  (call-with-output-string
   (lambda (out)
     (define (yield s)
       (display s out))
     (pp-mgl ast-lst #:yield yield))))

(define-with-contract*
  (-> (listof Def?) (or/c #f output-port?) path-string? boolean? void?)
  (generate-mgl-file ast-lst out path banner?)

  (define filename (path-basename-as-string path))

  (write-generated-output
   path out
   (lambda ()
    (when banner?
      (display-banner ";;" filename))
    (pp-mgl (defs-propagate-copies ast-lst))
    (newline)))

  (void))
