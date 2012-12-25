#lang racket

#|

Defines a data type for syntactic closures. A macro system with
various APIs can be built on top.

The API of this module should be included in the Racket namespace that
is in effect for macro expansion. This should probably be done
using "standard" names, by not including any clashing names that are
already defined in the "racket" language. Using typical names makes it
easier to port over existing macro constructs defined in Scheme.

|#

(require "env.rkt" "form.rkt" "util.rkt")

(struct synclo (env names exp) #:transparent)

(provide (rename-out
          (synclo make-syntactic-closure)))

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

;; If a symbol (possibly a 'form') or syntactic closure enclosed
;; symbol. Note that 'racket/base' includes 'identifier?'.
(define* (my-identifier? x)
  (or (symbol? x)
      (and (form? x) (my-identifier? (form-datum x)))
      (and (synclo? x) (my-identifier? (synclo-exp x)))))

(define* (identifier->symbol x)
  (cond
   ((symbol? x) x)
   ((form? x) (identifier->symbol (form-datum x)))
   ((synclo? x) (identifier->symbol (synclo-exp x)))
   (else (error 'identifier->symbol "not an identifier: ~s" x))))

;; We assign unique identifiers to all declared names, which makes
;; identifier comparisons straightforward.
(define* (identifier=? env-1 id-1 env-2 id-2)
  (let ((item-1 (env-get env-1 id-1))
	(item-2 (env-get env-2 id-2)))
    (eq? (binding-id item-1)
         (binding-id item-2))))
