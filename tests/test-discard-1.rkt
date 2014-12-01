#lang magnolisp

(typedef int (#:annos foreign))

(function (f x)
  (#:annos export (type (fn int int)))
  (let () (cast int 1) x (cast int 2) 3))

(f 5)
