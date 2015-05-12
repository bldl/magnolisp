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
(define* (list-visit-all s lst)
  (for-each s lst))

;; Like `map`, except that: does not accept multiple list arguments;
;; and if `s` returns #f, then stops mapping and returns #f. Returns
;; unmodified `in-lst` if `s` returns each element unmodified.
(define* (list-rewrite-all s in-lst)
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

(require*-only-in [(submod "strategy.rkt" private)
                   list-rewrite-some list-rewrite-one])

;;; 
;;; Primitive traversal operators for lists.
;;; 

;; These subterm traversals may be invoked for immediate "local"
;; traversals within terms containing list data. We could later
;; provide operations for vectors, boxes, and immutable hash tables,
;; for instance.

(module+ test
  (require rackunit))

(define-strategy-combinator* one/list list-rewrite-one)

(module+ test
  (check-equal?
   (list
    ((one/list number?) '())
    ((one/list number?) '(x y z))
    ((one/list number?) '(x 2 y 4)))
   '(#f #f (x #t y 4))))

(define-strategy-combinator* some/list list-rewrite-some)

(module+ test
  (check-equal?
   (list
    ((some/list number?) '())
    ((some/list number?) '(x y z))
    ((some/list number?) '(x 2 y 4)))
   '(#f #f (x #t y #t))))

;; This is an `all` for lists, where elements are "subterms".
(define-strategy-combinator* all/list list-rewrite-all)

(module+ test
  (check-equal?
   (list
    ((all/list number?) '())
    ((all/list number?) '(1 2 3))
    ((all/list number?) '(x 2 y 4)))
   '(() (#t #t #t) #f)))

(define-strategy-combinator* all-visit/list list-visit-all)

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
