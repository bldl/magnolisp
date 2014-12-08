#lang magnolisp/2014

;; we use `Bool` values, but do not explicitly refer to the type
(function (f) (#:annos export)
  (let ((x #f))
    x))

(f)
