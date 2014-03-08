#lang racket

#|

Utilities for authoring manual.scrbl.

|#

(require "util.rkt" scribble/manual)

(define-syntax* (subscript-var stx)
  (syntax-case stx ()
    ((_ nt s)
     #`(elem (racketvarfont #,(symbol->string (syntax->datum #'nt)))
             (subscript s)))))

(define-syntax-rule
  (define-subscript-var* n s)
  (define-syntax* (n stx)
    (syntax-case stx ()
      ((_ nt)
       #`(elem (racketvarfont #,(symbol->string (syntax->datum #'nt)))
               (subscript s))))))

(define-subscript-var* rkt-nt "rkt")
(define-subscript-var* ign-nt "ign")
(define-subscript-var* rkt-ign-nt "rkt,ign")
