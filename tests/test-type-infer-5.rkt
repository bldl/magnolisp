#lang magnolisp

(typedef long #an(foreign))

(function (id x) x)

(function (f x) #an(export)
  (cast long (let ((x x)) (id x))))

(f 8)
