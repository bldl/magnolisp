#lang racket

#|

When compiling to C++, make sure to set the environment appropriately.
The environment influences more or less the entire compilation
process.

|#

(require "util.rkt")

(define* compile? (and (getenv "COMPILE_IT") #t))

(define-syntax-rule* (if-compiling t e)
  (if compile? t e))

(define-syntax-rule* (if-not-compiling t e)
  (if (not compile?) t e))
