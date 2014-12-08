#lang magnolisp/2014

(require "lib-modules-1.rkt")

(typedef int (#:annos foreign))

(function (f x) #an(^(fn int int) export)
  (int->self x))

(f 1)
