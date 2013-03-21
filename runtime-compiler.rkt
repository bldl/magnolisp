#lang racket/base

#|

We cannot introduce new core language into Racket, and so we must be
able to express foreign syntax in terms of Racket syntax, without
using macros or runtime values. This is not really a problem since
unique identifiers for functions can be given here, and our syntax can
be expressed in terms of application of such functions. We cannot do
much quoting to make sure that we retain binding information, and to
make sure that macros get expanded. Again, no problem, as we can use
'lambda' as a container for code.

|#

(require "util.rkt")
(require (for-syntax "util.rkt" racket/base syntax/context))

;; racket/base exports nothing for-syntax, so let us export something.
(provide (for-syntax (all-from-out racket/base)))

;; Only required if we actually wrap the read code into a 'module'
;; before expansion.
(provide (rename-out (#%plain-module-begin #%module-begin)))

;; It is not really appropriate to export 'provide' and 'require'
;; without additional restrictions, but we do it for now.
(provide begin-for-syntax
         define-for-syntax
         define-syntax
         define-syntax-rule
         provide
         require
         #%datum)

;; Provide this so the compiler can compare syntax of ID against this
;; one to see if it is the same one.
(define* %core list)

;;; 
;;; declarations
;;; 

(define-syntax* (procedure stx)
  (syntax-case stx ()
    ((_ (n) body ...)
     #'(define n
         (%core 'procedure
                (lambda () body ...))))))

;;; 
;;; statements
;;; 

(define-syntax-rule*
  (pass)
  (%core 'pass #f))

(define-syntax-rule*
  (call n)
  (%core 'call n))
