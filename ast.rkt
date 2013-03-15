#lang racket

#|
|#

(require "util.rkt")

(define-struct* loc (source line column position span) #:transparent)

(define* (stx-loc stx)
  (loc
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define-struct* Ast (annos))

(define* (stx-annos stx)
  (cons
   (cons 'loc (stx-loc stx))
   (map
    (lambda (k) (cons k (syntax-property stx k)))
    (syntax-property-symbol-keys stx))))

(define-syntax-rule
  (define-ast* name (field ...))
  (define-struct* name Ast (field ...) #:transparent))

(define-ast* Module (body))
(define-ast* Pass ())
(define-ast* Call (proc))
(define-ast* Define (name body))

