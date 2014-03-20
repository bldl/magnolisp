#lang magnolisp

(require "lib-modules-3.rkt")

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  (eight-m))

(f 100)
