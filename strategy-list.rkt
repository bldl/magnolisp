#lang racket/base

#|
|#

(require "strategy.rkt" "util.rkt")

;;; 
;;; List access operations.
;;;

;; Implementations of gen:strategic operations for lists. We do not
;; actually include the list type into gen:strategic, but these
;; operations may be useful in implementing gen:strategic operations
;; for user-defined types.

;; Like `for-each`, except that does not accept multiple list
;; arguments.
(define* (all-visit-list s lst)
  (for-each s lst))

;; Like `map`, except that: does not accept multiple list arguments;
;; and if `s` returns #f, then stops mapping and returns #f. Returns
;; unmodified `in-lst` if `s` returns each element unmodified.
(define* (all-rw-list s in-lst)
  (define changed? #f)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        (if changed?
            (reverse res-lst)
            in-lst)
        (let* ((x (car lst))
               (res (s x)))
          (and res
               (let ()
                 (unless (eq? x res)
                   (set! changed? #t))
                 (next (cons res res-lst) (cdr lst))))))))

;; Like `map`, except that: does not accept multiple list arguments;
;; does not change elements for which `s` returns #f; and if `s`
;; returns #f for all elements, then returns #f. Returns unmodified
;; `lst` if `s` does not change any elements (i.e., `eq?`uivalence
;; holds).
(define* (some-rw-list s lst)
  (define changed? #f)
  (define some? #f)
  (define res (map (lambda (x)
                     (define y (s x))
                     (if y
                         (begin
                           (unless (eq? x y)
                             (set! changed? #t))
                           (set! some? #t)
                           y)
                         x))
                   lst))
  (and some? (if changed? res lst)))

;; Like `map`, but stops transforming elements in `lst` as soon as `s`
;; has produced a true value for an element. Does not change elements
;; for which `s` returns #f. If `s` returns #f for all elements, the
;; overall result will also be #f. Returns unmodified `lst` if `s`
;; does not change any elements.
(define* (one-rw-list s in-lst)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        #f
        (let* ((x (car lst))
               (xs (cdr lst))
               (res (s x)))
          (if res
              (if (eq? x res)
                  in-lst
                  (append (reverse res-lst) (cons res xs)))
              (next (cons x res-lst) xs))))))

;;; 
;;; Primitive traversal operators for lists.
;;; 

;; These subterm traversals may be invoked for immediate "local"
;; traversals within terms containing list data. We could later
;; provide operations for vectors, boxes, and immutable hash tables,
;; for instance.

(module+ test
  (require rackunit))

(define-strategy-combinator* one/list one-rw-list)

(module+ test
  (check-equal?
   (list
    ((one/list number?) '())
    ((one/list number?) '(x y z))
    ((one/list number?) '(x 2 y 4)))
   '(#f #f (x #t y 4))))

(define-strategy-combinator* some/list some-rw-list)

(module+ test
  (check-equal?
   (list
    ((some/list number?) '())
    ((some/list number?) '(x y z))
    ((some/list number?) '(x 2 y 4)))
   '(#f #f (x #t y #t))))

;; This is an `all` for lists, where elements are "subterms".
(define-strategy-combinator* all/list all-rw-list)

(module+ test
  (check-equal?
   (list
    ((all/list number?) '())
    ((all/list number?) '(1 2 3))
    ((all/list number?) '(x 2 y 4)))
   '(() (#t #t #t) #f)))

(define-strategy-combinator* all-visit/list all-visit-list)

(module+ test
  (check-equal?
   '(#f #f #t)
   (let ()
     (define lst null)
     ((all-visit/list
       (lambda (x)
         (set! lst (cons x lst))))
      '(#t #f #f))
     lst)))
