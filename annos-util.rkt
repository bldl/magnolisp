#lang racket/base

#|

We store all annotations into the 'annos' syntax property, as an
immutable hasheq.

|#

(require "compiler-util.rkt" "util.rkt" racket/contract)

(define-with-contract*
  (-> any/c boolean?)
  (syntax-with-annos? x)
  (and (syntax? x)
       (let ((h (syntax-property x 'annos)))
         (and h
              (hash? h)
              (hash-eq? h)
              (immutable? h)))))

(define-with-contract*
  (-> syntax? (and/c hash? hash-eq? immutable?))
  (syntax-get-annos stx)
  (define h (syntax-property stx 'annos))
  (cond
   ((not h) #hasheq())
   ((hash? h) h)
   ;; It seems that the macro expander, in its origin tracking
   ;; presumably, sometimes cons'es together original and new syntax
   ;; property values. The newer one would appear to be first. Do not
   ;; know if here is any limit to this tracking. The DrRacket macro
   ;; expander can show syntax properties of syntax objects, which is
   ;; handy for observing this behavior.
   ((pair? h) (car h))
   (else
    (error 'syntax-get-annos "unexpected annos ~s" h))))

(define-with-contract*
  (-> syntax? (and/c hash? hash-eq? immutable?) syntax-with-annos?)
  (syntax-set-annos stx h)
  (syntax-property stx 'annos h))

(define-with-contract*
  (-> syntax? (listof syntax?) #:from syntax? syntax-with-annos?)
  (syntax-add-annos/stx-list stx anno-lst #:from ctx)
  (define h
    (for/hasheq ((a anno-lst))
      (define name (form-get-name a))
      (unless name
        (raise-syntax-error
         #f
         "annotation form"
         a ctx))
      (values name a)))
  (define old-h (syntax-get-annos stx))
  (define new-h (hash-merge-2 old-h h))
  (syntax-set-annos stx new-h))
