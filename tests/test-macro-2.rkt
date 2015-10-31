#lang magnolisp/2014

#|

Here we try a set! transformer. This test is related to
test-macro-3.rkt, in that together declaring and using accessor
functions can become quite convenient.

|#

(require (for-syntax racket/syntax))

(typedef Obj (#:annos foreign))
(typedef int (#:annos foreign))

(begin-for-racket
 (struct MyObj (x y) #:transparent))

(function (Obj-new x y)
  (#:annos (type (fn Obj int int)) foreign)
  (begin-racket (MyObj x y)))

(function (Obj-get-x obj)
  (#:annos (type (fn Obj int)) foreign)
  (begin-racket (MyObj-x obj)))

(function (Obj-set-x obj v)
  (#:annos (type (fn Obj int Obj)) foreign)
  (begin-racket (struct-copy MyObj obj [x v])))

(function (inc x)
  (#:annos (type (fn int int)) foreign)
  (add1 x))

(define-syntax-rule
  (bind-accessors name obj get set)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! _ v) (set! obj (set obj v))]
      [_ (get obj)])))

(function (f in-obj)
  #an(export ^(fn Obj Obj))
  (var obj in-obj)
  (bind-accessors x obj Obj-get-x Obj-set-x)
  (set! x (inc x))
  obj)
    
(f (MyObj 0 0))
