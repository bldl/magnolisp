#lang magnolisp/base

#|

A library of built-in types and functions in Magnolisp.

|#

(require "surface.rkt")

(provide Bool Void)

(typedef Bool #:: ([foreign bool]))
(typedef Void #:: ([foreign void]))
