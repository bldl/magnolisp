#lang magnolisp

(primitives [#:type List] [#:type Elem]
  [#:function (empty? lst) :: (-> List Bool)]
  [#:function (head lst) :: (-> List Elem)]
  [#:function (tail lst) :: (-> List List)]
  [#:function (add x y) :: (-> Elem Elem Elem)])

(define (sum-2 lst) #:: (export)
  (if (empty? lst)
      0
      (let ([t (tail lst)])
        (if (empty? t)
            (head lst)
            (add (head lst) (head t))))))
