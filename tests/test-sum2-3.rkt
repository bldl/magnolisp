#lang magnolisp

(primitives [#:type List] [#:type Elem]
  [#:function (empty? lst) :: (-> List Bool)]
  [#:function (head lst) :: (-> List Elem)]
  [#:function (tail lst) :: (-> List List)]
  [#:function (add x y) :: (-> Elem Elem Elem)])

(define-syntax (mfold stx)
  (syntax-case stx ()
    [(_ op init lst n)
     (let ([i (syntax-e #'n)])
       (if (= i 0)
           #'init
           #`(let ([tmp lst])
              (if (empty? tmp)
                  init
                  (op (head tmp) 
                      (mfold op init (tail tmp) 
                             #,(sub1 i)))))))]))
           
(define (sum-2/2 lst) #:: (export)
  (mfold add 0 lst 2))
