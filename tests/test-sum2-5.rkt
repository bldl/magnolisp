#lang magnolisp
(require magnolisp/std/list
         (prefix-in rkt. racket/base))

(typedef Int #:: ([foreign int]))

(define (add x y) #:: (foreign [type (-> Int Int Int)])
  (rkt.+ x y))

(define-syntax-rule (if-empty lst t e)
  (if (empty? lst) t e))

(define (sum-2 lst) #:: (export)
  (if-empty lst 0
    (let ([t (tail lst)])
      (if-empty t (head lst)
        (add (head lst) (head t))))))

(empty? empty)
(sum-2 empty)
(sum-2 (list))
(sum-2 (cons 2 empty))
(sum-2 (list 4))
(sum-2 (list 3 4))
(sum-2 (list 1 2 3 4))
