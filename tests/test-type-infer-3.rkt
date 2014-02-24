#lang magnolisp

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (define (local-g x) x)
    (return (local-g x))))

(f 1)
