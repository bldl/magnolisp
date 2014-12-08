#lang magnolisp/2014

(typedef Elem (#:annos foreign))
(typedef List (#:annos foreign))

(define-syntax-rule (declare-List-op [n t] ...)
  (begin
    (function (n lst) (#:annos [type (fn List t)] foreign))
    ...))

(declare-List-op 
 [empty? Bool]
 [head Elem]
 [tail List])

(function (zero) (#:annos [type (fn Elem)] foreign))
(function (add x y) (#:annos [type (fn Elem Elem Elem)] foreign))

(function (sum-2 lst) (#:annos export)
  (if (empty? lst)
      (zero)
      (do (var h (head lst))
          (var t (tail lst))
          (if (empty? t)
              (return h)
              (return (add h (head t)))))))
