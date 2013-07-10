#lang racket

#|

The evaluator is implemented in the normal Racket style, through macro
transformation into the Racket core language.

|#

(require "util.rkt")

(define-syntax-rule* (pass)
  (void))

;; Evaluate twice, presumably for side effects.
(define-syntax-rule* (twice x)
  (begin x x))
