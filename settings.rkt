#lang racket

#|

When compiling to C++, make sure to set the environment appropriately.
The environment influences more or less the entire compilation
process.

|#

(require "util.rkt")

(define* compile? (and (getenv "COMPILE_IT") #t))
