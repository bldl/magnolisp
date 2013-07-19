#lang racket

#|
|#

(require "util.rkt")

(begin-for-syntax

 (require racket/contract)

 (define (syntax-with-annos? x)
   (and (syntax? x)
        (let ((h (syntax-property x 'annos)))
          (and h
               (hash? h)
               (immutable? h)))))

 (define (get-annos stx)
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
     (error 'get-annos "unexpected annos ~s" h))))

 (define (set-annos stx h)
   (syntax-property stx 'annos h))

 (define (add-anno stx k v #:from (from stx))
   (set-annos stx (hash-set (get-annos from) k v)))

 (provide/contract
  [syntax-with-annos? (-> any/c boolean?)]
  [get-annos (-> syntax? (and/c hash? immutable?))]
  [set-annos (-> syntax? (and/c hash? immutable?) syntax-with-annos?)]
  [add-anno (->* (syntax? symbol? syntax?)
                 (#:from syntax?) syntax-with-annos?)])

 ) ; end begin-for-syntax
