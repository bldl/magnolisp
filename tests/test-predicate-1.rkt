#lang magnolisp

(function (f)
  #:: (export (type (-> Bool)))
  #t)

(f)

(function (g)
  #:: (export (type (-> Bool)))
  (let ()
    (var r #f)
    r))

(g)

(function (h)
  #:: (export (type (-> Bool)))
  (var r #f)
  (var x r)
  x)

(h)
