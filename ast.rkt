#lang racket

#|
|#

(require "util.rkt")

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

(define-syntax-rule
  (define-ast* name (field ...))
  (define-struct* name Ast (field ...) #:transparent))

;;; 
;;; concrete nodes
;;; 

(define-ast* Module (body))
(define-ast* Pass ())
(define-ast* Call (proc))
(define-ast* Define (name body))

