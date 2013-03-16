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

;; racket/base exports nothing for-syntax
(provide (for-syntax (all-from-out racket/base)))
(provide (rename-out (my-module-begin #%module-begin)))
;(provide (rename-out (my-app #%app)))
;(provide (rename-out (#%app %app)))

;; For now this is easier. We want to be more selective, though. We
;; want at least 'module' and '#%app', but we could define our own
;; version of the latter.
;;(provide (except-out (all-from-out racket/base) #%module-begin))

(provide lambda)

;; Trying to make 'expand' insert this particular identifier.
(provide #%app)

;; Problem is we should use this here, too, in macros like 'pass', but
;; if we defined it as a function, it would not be treated the same by
;; expander, and another #%app would get inserted. But what now is a
;; macro going to do for us.
(define-syntax-rule (my-app x ...)
  (#%app x ...))

;; Only required if we actually wrap the read code into a 'module'
;; before expansion.
(define-syntax-rule (my-module-begin form ...)
  (#%plain-module-begin form ...))

;; Provide this so the compiler can compare syntax of ID against this
;; one to see if it is the same one.
(define* %core list)

(define-syntax-rule*
  (pass)
  (%core 'pass #f))

(define-syntax-rule*
  (call n)
  (%core 'call n))

(define-syntax* (procedure stx)
  (syntax-case stx ()
    ((_ (n) body ...)
     #'(define n
         (%core 'procedure
                (lambda () body ...))))))
