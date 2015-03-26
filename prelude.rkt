#lang magnolisp/base

#|

A library of built-in types and functions in Magnolisp.

|#

(require "core.rkt" "surface.rkt")

(declare #:type Bool #:: ([foreign bool]))
(declare #:type Void #:: ([foreign void]))
