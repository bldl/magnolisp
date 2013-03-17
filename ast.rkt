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

(define-struct* Ast (annos))

(define-syntax (define-ast* stx)
  (syntax-case stx ()
    ((_ name (field ...))
     #`(begin
         (define-struct* name Ast (field ...) #:transparent)
         (define* #,(format-id stx "new-~a" (syntax-e #'name))
           (lambda (stx . args)
             (apply name (stx-annos stx) args)))))))

;;; 
;;; concrete nodes
;;; 

(define-ast* Var (name))
(define-ast* Module (body))
(define-ast* Pass ())
(define-ast* Call (proc))
(define-ast* Define (name kind body))

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))
