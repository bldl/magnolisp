#lang magnolisp/2014

#|

Macro for declaring (foreign) accessor functions for a specific field
of an abstract data type.

|#

(require (for-syntax racket/syntax))

(typedef Obj (#:annos foreign))
(typedef int (#:annos foreign))

(define-syntax (declare-accessors stx)
  (syntax-case stx ()
    [(_ cls fld t)
     (with-syntax
         ([get (format-id stx "~a-get-~a" #'cls #'fld)]
          [set (format-id stx "~a-set-~a" #'cls #'fld)])
       #'(begin
           (function (get obj)
             (#:annos (type (fn cls t)) foreign))
           (function (set obj v)
             (#:annos (type (fn cls t cls)) foreign))))]))

(function (inc x)
  (#:annos (type (fn int int)) foreign)
  (add1 x))

(declare-accessors Obj x int)

(function (f obj)
  #an(export ^(fn Obj Obj))
  (Obj-set-x obj (inc (Obj-get-x obj))))
