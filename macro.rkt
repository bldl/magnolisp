#lang racket

#|

Defines a data type for syntactic closures. A macro system with
various APIs can be built on top.

The API of this module should be exposed to the Racket namespace in
effect for macro expansion. This should probably be done
using "standard" names, excluding any clashing names that may already
be defined for Racket. This makes it easier to port over existing
macro constructs defined in Scheme.

|#

(require "form.rkt" "util.rkt")

(struct synclo (env names exp) #:transparent)

(provide (rename-out (synclo make-syntactic-closure)))

;; Does not strip from annotations.
(define* (strip-syntactic-closures e)
  (cond
   ((form? e)
    (form-on-datum strip-syntactic-closures e))
   ((synclo? e)
    (strip-syntactic-closures (synclo-exp e)))
   ((list? e)
    (map strip-syntactic-closures e))
   (else
    e)))
