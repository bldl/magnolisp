#lang magnolisp

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  (do
    (function (g x) #an(^(fn int int)) x)
    (return
     (do
       (function (g x) #an(^(fn int int)) x)
       (return (g x))))))

(f 1)
