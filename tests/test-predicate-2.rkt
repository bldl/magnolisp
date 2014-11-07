#lang magnolisp

;; we use `predicate` values, but do not explicitly refer to the type
(function (f) (#:annos export)
  (let ((x #f))
    x))

(f)
