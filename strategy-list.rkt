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

(define* strategic-list-accessors
  (make-strategic-data-accessors
   (lambda (obj) obj) (lambda (obj lst) lst)
   #:visit-all list-visit-all
   #:rewrite-all list-rewrite-all
   #:rewrite-some list-rewrite-some
   #:rewrite-one list-rewrite-one))

;;; 
;;; Primitive strategies.
;;; 

(module+ test
  (require rackunit))

(define-specific-data-strategy* list-all-visitor list-visit-all)

(module+ test
  (check-equal?
   '(#f #f #t)
   (let ()
     (define lst null)
     ((list-all-visitor
       (lambda (x)
         (set! lst (cons x lst))))
      '(#t #f #f))
     lst)))

;; This is an `all` for lists, where elements are "subterms".
(define-specific-data-strategy* list-all-rewriter list-rewrite-all)

(module+ test
  (check-equal?
   (list
    ((list-all-rewriter number?) '())
    ((list-all-rewriter number?) '(1 2 3))
    ((list-all-rewriter number?) '(x 2 y 4)))
   '(() (#t #t #t) #f)))

(define-specific-data-strategy* list-some-rewriter list-rewrite-some)

(module+ test
  (check-equal?
   (list
    ((list-some-rewriter number?) '())
    ((list-some-rewriter number?) '(x y z))
    ((list-some-rewriter number?) '(x 2 y 4)))
   '(#f #f (x #t y #t))))

(define-specific-data-strategy* list-one-rewriter list-rewrite-one)

(module+ test
  (check-equal?
   (list
    ((list-one-rewriter number?) '())
    ((list-one-rewriter number?) '(x y z))
    ((list-one-rewriter number?) '(x 2 y 4)))
   '(#f #f (x #t y 4))))

(module+ test
  (let ()
    (define rw
      (with-strategic-data-accessors
        strategic-list-accessors
        (all-rewriter
         (lambda (v)
           (add1 v)))))
    (define lst '(1 2 3))
    (check-equal? (map add1 lst) (rw lst))))

;;; 
;;; Strategy combinators.
;;; 

(module+ test
  (let ()
    (define rw
      (topdown-rewriter
       (lambda (v)
         (cond
           [(number? v) (add1 v)]
           [else v]))
       #:rewrite-all (lambda (f ast)
                       (if (list? ast)
                           (list-rewrite-all f ast)
                           ast))))

    (define (test lst expect)
      (check-equal? (rw lst) expect))
    
    (for ([lst-expect (list
                       '(() ())
                       '((1) (2)))])
      (apply test lst-expect))))
