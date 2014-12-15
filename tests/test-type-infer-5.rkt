#lang magnolisp

(typedef long #:: (foreign))

(function (id x) x)

(function (f x) #:: (export)
  (cast long (let ((x x)) (id x))))

(f 8)
