#lang at-exp racket

#|

Utilities for authoring manual.scrbl.

|#

(require "../util.rkt" scribble/manual)

(define* (warning . str)
  (list "(" (italic "Warning: ") str ")"))

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

(define-syntax-rule* (ign form)
  (elem form (subscript "ign")))

(define-syntax* (indirect-id stx)
  (syntax-case stx ()
    ((_ id)
     #`(elem (racket id)
             (subscript (italic "id-expr"))))))

;; An equation expressing a syntax property condition.
(define-syntax (stxprop-equ stx)
  (syntax-case stx ()
    ((_ id)
     #`(racket 'id ≠ #f))
    ((_ id cmp-op val)
     #`(racket 'id cmp-op #,(syntax->datum #'val)))
    ))

;; To use as the first element in a form, or as a suffix for a form.
(define-syntax* (stxprop-elem stx)
  (syntax-case stx ()
    ((_ id)
     #'(subscript (stxprop-equ id)))
    ((_ id cmp-op val)
     #'(subscript (stxprop-equ id cmp-op val)))
    ))

;; To associate with a form as a whole.
(define-syntax* (stxpropped stx)
  (syntax-case stx ()
    ((_ form pname)
     #'(elem form (stxprop-elem pname)))
    ((_ form pname cmp-op pval)
     #'(elem form (stxprop-elem pname cmp-op pval)))
    ))

(define-syntax-rule* (ign-flag name form)
  (elem form (subscript (elem (stxprop-equ name) ",ign"))))

(define-syntax-rule*
  (harnessed form)
  (racket (#,racket-if #,(ign-nt Racket-expr) form #,(ign-nt Racket-expr))))

;;; 
;;; Racket syntax
;;; 

(module Racket-m racket
  (require scribble/manual (for-label racket/base))
  (provide racket/Racket)
  (define-syntax (racket/Racket stx)
    (syntax-case stx ()
      [(_ datum)
       #`(racket #,(datum->syntax #'here (syntax->datum #'datum)))])))

(require (submod "." Racket-m))
(provide racket/Racket)

(define-syntax-rule* (Racket-racket datum)
  @elem{Racket's @racket/Racket[datum]})

(define* racket-if (racket/Racket if))
(define* racket-module-begin (racket/Racket #%module-begin))
