#lang racket

#|

Racket appears to have no convenient facility to apply a function over
the elements of a map, set, etc., so that the result is of the
original type. This module attempts to address that issue.

An ad-hoc generic 'map' function. Supports the more common Racket
collection and container datatypes, with the exception of 'struct'
types. Mutability/immutability is taken into account. (Note that a
'set' is always immutable, and hence there is no specific immutable
variant.) Mutable pairs are not supported.

|#

(require "util.rkt")

(define* (coll? x)
  (or (list? x)
      (pair? x)
      (vector? x)
      (box? x)
      (hash? x)
      (set? x)))

;; Note that it makes little sense to pass more than one collection in
;; the case of unordered collections.
(define* (coll-map f . coll)
  (when (null? coll)
    (error 'coll-map "no collections given"))
  (let ((c1 (car coll)))
    (when (and (not (null? (cdr coll)))
               (or (hash? c1) (set? c1)))
      (error 'coll-map "only one unordered collection allowed"))
    (cond
     ((list? c1)
      (apply map f coll))
     ((pair? c1)
      (cons
       (apply f (map car coll))
       (apply f (map cdr coll))))
     ((vector? c1)
      (apply (if (immutable? c1) vector-immutable vector)
             (apply map f (map vector->list coll))))
     ((box? c1)
      ((if (immutable? c1) box-immutable box)
       (apply f (map unbox coll))))
     ((hash? c1)
      (let* ((i? (immutable? c1))
             (ctor
              (cond
               ((hash-eq? c1)
                (if i? make-immutable-hasheq make-hasheq))
               ((hash-eqv? c1)
                (if i? make-immutable-hasheqv make-hasheqv))
               ((hash-equal? c1)
                (if i? make-immutable-hash make-hash))
               (else (error 'coll-map "unexcepted hash type")))))
        (apply ctor (hash-map c1 f))))
     ((set? c1)
      (let ((ctor
             (cond
              ((set-eq? c1) seteq)
              ((set-eqv? c1) seteqv)
              ((set-equal? c1) set)
              (else (error 'coll-map "unexcepted set type")))))
        (apply ctor (set-map c1 f))))
     (else
      (error 'coll-map "unsupported collection type")))))
