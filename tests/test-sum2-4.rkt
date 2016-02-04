#lang magnolisp

(require magnolisp/std/list)

(primitives [#:type Elem]
  [#:function (add x y) :: (-> Elem Elem Elem)])

(define-syntax-rule
  (if-let-car [x v] e t)
  (let ([lst v])
    (if (empty? lst)
        e
        (let ([x (head lst)])
          t))))

(define (sum-2 lst) #:: (export)
  (if-let-car [e0 lst] 0
    (if-let-car [e1 (tail lst)] e0 (add e0 e1))))
