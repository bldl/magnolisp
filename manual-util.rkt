#lang racket

#|

Utilities for authoring manual.scrbl.

|#

(require "util.rkt" scribble/manual)

(begin ;; trick from Racket docs
  (define-syntax-rule (bind id-2 id-3)
    (begin
      (require (for-label racket/base))
      (define* id-2 (racket #%module-begin))
      (define* id-3 (racket if))))
  (bind racket-module-begin racket-if))

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

(define-syntax (flag stx)
  (syntax-case stx ()
    ((_ id)
     #`(racket '#,(syntax->datum #'id) ≠ #f))))

(define-syntax* (sub-flag stx)
  (syntax-case stx ()
    ((_ id)
     #`(subscript (flag id)))))

(define-syntax-rule* (ign-flag name form)
  (elem form (subscript (elem (flag name) ",ign"))))

(define-syntax-rule* 
  (flagged flag form)
  (elem form (sub-flag flag)))

(define-syntax-rule*
  (harnessed form)
  (racket (#,racket-if #,(ign-nt Racket-expr) form #,(ign-nt Racket-expr))))
