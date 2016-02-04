#lang magnolisp

#|

A basic list library for Magnolisp. Note that `list` is a macro, not a
function, to work around not having varargs. Also, `empty` is a macro,
and not a value, since we do not refer to C++ values.

|#

(require (prefix-in rkt. racket/base))

(provide List
         empty empty? head tail cons
         list)

(define #:type List #:: (foreign))

(define (make-empty)
  #:: (foreign ^(for-all E (-> (<> List E))))
  rkt.null)

(define-syntax empty
  (syntax-id-rules ()
    [_ (make-empty)]))

(define empty?
  #:: (foreign ^(for-all E (-> (<> List E) Bool)))
  rkt.null?)

(define head
  #:: (foreign ^(for-all E (-> (<> List E) E)))
  rkt.car)

(define tail
  #:: (foreign ^(for-all E (-> (<> List E) (<> List E))))
  rkt.cdr)

(define cons
  #:: (foreign ^(for-all E (-> E (<> List E) (<> List E))))
  rkt.cons)

(define-syntax list
  (syntax-rules ()
    [(_) empty]
    [(_ e . rest) (cons e (list . rest))]))
