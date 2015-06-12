#lang racket/base

#|

A Stratego-inspired strategic-term-rewriting API.

|#

(require "strategy.rkt" "util.rkt")

;; Identity strategy. Named `id` in Stratego.
(define* (id-rw ast) ast)

;; Failure strategy. Named `fail` in Stratego.
(define* (fail-rw ast) #f)

;; Sequential composition.
;; Uses Kiama naming rather than |;| of Stratego.
(define-syntax-rule* (<* s ...)
  (lambda (ast)
    (and (begin
           (set! ast (s ast))
           ast) ...
           ast)))

;; Deterministic choice.
(define-syntax-rule* (<+ s ...)
  (lambda (ast)
    (or (s ast) ...)))

;; Tries a rewrite, restoring original term on failure.
(define* (try s)
  (lambda (ast)
    (or (s ast) ast)))

;; Test strategy.
;; Tries a rewrite, but restores original term on success.
(define* (where s)
  (lambda (ast)
    (and (s ast) ast)))

;; Negated test strategy.
(define* (where-not s)
  (lambda (ast)
    (if (s ast) #f ast)))

(define* (when-rw c s)
  (lambda (ast)
    (if (c ast)
        (s ast)
        ast)))

;; Recursive closure, like "rec x(s)" in Stratego.
(define-syntax-rule* (rec x s)
  (lambda (ast)
    (letrec ([x s])
      (x ast))))

(define* (repeat s)
  (rec x (try (<* s x))))

;; Turns a function `f` with a signature of the form (f s ast) into a
;; strategy combinator.
(define* (((make-strategy f) s) ast)
  (f s ast))

;; Strategies for sub-term rewrites.
(define* all (make-strategy term-rewrite-all))
(define* some (make-strategy term-rewrite-some))
(define* one (make-strategy term-rewrite-one))

(define* (topdown s [all all])
  (rec x (<* s (all x))))

(define* (bottomup s [all all])
  (rec x (<* (all x) s)))

(define* (downup s [all all])
  (rec x (<* s (all x) s)))

(define* (onebu s [one one])
  (rec x (<+ (one x) s)))

(define* (downup2 s1 s2 [all all])
  (rec x (<* s1 (all x) s2)))

(define* (alltd s [all all])
  (rec x (<+ s (all x))))

(define* (oncetd s [one one])
  (rec x (<+ s (one x))))

(define* (sometd s [some some])
  (rec x (<+ s (some x))))

(define* (somebu s [some some])
  (rec x (<+ (some x) s)))
