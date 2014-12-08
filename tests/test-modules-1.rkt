#lang magnolisp/2014

(require "lib-modules-1.rkt")

(typedef Int (#:annos foreign))

(function (foo-g x)
  (int->self x))

(function (priv!ate/g x)
  (foo-g x))

(function (pri!vate?g x)
  x)

(function (nice-h!) #an(^(fn Int))
  5)

(function (f x) #an(^(fn Int Int) export)
  (pri!vate?g (priv!ate/g (nice-h!))))
