#lang racket

#|

For more compact printing, we do not make annotations transparent.

|#

(require "util.rkt")
(require (for-syntax racket/syntax))

;;; 
;;; location info
;;; 

(define-struct* loc (source line column position span) #:transparent)

(define* (stx-loc stx)
  (loc
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

;;; 
;;; syntax-derived annotations
;;; 

(define* (stx-annos stx)
  (let ((h (for/hasheq ((k (syntax-property-symbol-keys stx)))
                       (values k (syntax-property stx k)))))
    (set! h (hash-set h 'loc (stx-loc stx)))
    (set! h (hash-set h 'stx stx))
    h))

;;; 
;;; abstract node
;;; 

;; E.g. for 'struct', can say #:property prop:subterm-all identity
(define-values* (prop:subterm-all subterm-all? subterm-all)
  (make-struct-type-property 'subterm-all))

(define-struct* Ast (annos))

(define-syntax (define-ast* stx)
  (syntax-case stx ()
    ((_ name (field ...) all-op)
     #`(begin
         (define-struct* name Ast (field ...)
           ;;#:property prop:subterm-all all-op
           #:transparent)
         (define* #,(format-id stx "new-~a" (syntax-e #'name))
           (lambda (stx . args)
             (apply name (stx-annos stx) args)))))))

;;; 
;;; concrete nodes
;;; 

(define-ast* Var (name) all-identity)
(define-ast* Module (body) Module-all)
(define-ast* Pass () all-identity)
(define-ast* Call (proc) Call-all)
(define-ast* Define (name kind body) Define-all)

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

;;; 
;;; "all" strategies
;;; 

(define (all-identity f ast) ast)

(define (Module-all f ast)
  (let-and body (map-while f (Module-body ast))
           (struct-copy Module ast (body body))))

(define (Call-all f ast)
  (let-and proc (f (Call-proc ast))
           (struct-copy Call ast (proc proc))))

(define (Define-all f ast)
  (let-and name (f (Define-name ast))
           body (map-while f (Define-body ast))
           (struct-copy Define ast (name name) (body body))))

