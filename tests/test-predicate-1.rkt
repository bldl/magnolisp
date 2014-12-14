#lang magnolisp

(function (f)
  #:: (export (type (-> Bool)))
  #t)

(f)

(function (g)
  #:: (export (type (-> Bool)))
  (begin-return
    (var r #f)
    (return r)))

(g)

(function (h)
  #:: (export (type (-> Bool)))
  (begin-return
    (var r #f)
    (var x r)
    (return x)))

(h)
