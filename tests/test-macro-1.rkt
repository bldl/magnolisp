#lang magnolisp

(typedef int (#:annos foreign))

(function (equal x y)
  (#:annos (type (fn int int predicate)) foreign)
  (equal? x y))

(function (compute x)
  x)

(define-syntax-rule
  (default e f d)
  (do
    (var ret e)
    (return
     (if (equal ret f)
         d
         ret))))

(function (f x)
  (#:annos export)
  (default (compute x) 0 7))

(default 1 1 7)
(default 2 1 7)
