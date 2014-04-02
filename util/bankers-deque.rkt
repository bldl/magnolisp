#lang racket/base

#|

Implements a banker's deque, as per Okasaki. Uses Racket lists as the
stream type. The invariant maintained for [lenf f lenr r] is (and (<=
lenf (+ (* (dq-factor) lenr) 1)) (<= lenr (+ (* (dq-factor) lenf)
1))), for some constant '(dq-factor)', in order to maintain balance.
'f' means "front" and 'r' means "rear". The algorithm supports every
deque operation in O(1) amortized time, with the exception of
concatenation and iteration.

|#

(require "module.rkt" racket/function racket/list)

(define* dq-factor (make-parameter 7))

(define (dq-print x out mode)
  (write-string "#dq" out)
  (write (append (Deque-f x)
		 (reverse (Deque-r x))) out))

(struct Deque (lenf f lenr r) 
	#:transparent
	#:property prop:custom-write dq-print)

;; An empty double-ended queue.
(define* dq-null
  (Deque 0 '() 0 '()))

(provide (rename-out [Deque? dq?]))

;; Splits given integer 'n' into two integer halves whose sum is 'n'.
(define (div2 n)
  (let ((x (round (/ n 2))))
    (values x (- n x))))

;; Constructs a queue with the specified content, balancing first if
;; necessary to meet the invariant.
(define (check lenf f lenr r)
  (cond
   ((> lenf (+ (* (dq-factor) lenr) 1))
    (let*-values (([lenf+ lenr+] (div2 (+ lenf lenr)))
		  ([f+ f-rest] (split-at f lenf+))
		  ([r+] (append r (reverse f-rest))))
      (Deque lenf+ f+ lenr+ r+)))

   ((> lenr (+ (* (dq-factor) lenf) 1))
    (let*-values (([lenf+ lenr+] (div2 (+ lenf lenr)))
		  ([r+ r-rest] (split-at r lenr+))
		  ([f+] (append f (reverse r-rest))))
      (Deque lenf+ f+ lenr+ r+)))

   (else
    (Deque lenf f lenr r))))

;; Whether empty.
(define* (dq-null? q)
  (and (= (Deque-lenf q) 0) (= (Deque-lenr q) 0)))

(define* (dq-length q)
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (+ lenf lenr)))

(define* (dq . es)
  (check (length es) es 0 '()))

;; Push element to front.
(define* (dq-cons-f q e)
  (check (+ (Deque-lenf q) 1) (cons e (Deque-f q))
	 (Deque-lenr q) (Deque-r q)))

;; Push element to rear.
(define* (dq-cons-r q e)
  (check (Deque-lenf q) (Deque-f q) 
	 (+ (Deque-lenr q) 1) (cons e (Deque-r q))))

;; Push two elements, first to front, second to rear.
(define* (dq-cons-f-r q e-f e-r)
  (check (+ (Deque-lenf q) 1) (cons e-f (Deque-f q))
	 (+ (Deque-lenr q) 1) (cons e-r (Deque-r q))))

(define-syntax-rule*
  (define-with-default* (f q p ...) fail b ...)
  (define* (f q p ... [default
                       (thunk (raise-argument-error
                               (quote f)
                               "non-empty dq" q))])
    (define (fail)
      (if (procedure? default)
          (default)
          default))
    b ...))

;; Returns the first element from the front, or failure-result if
;; none.
(define-with-default* (dq-car-f q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0))
      (was-empty))

     ;; Note that the invariant guarantees that both 'f' and 'r' are
     ;; non-empty if there are at least two elements in the queue. It
     ;; would perhaps be clearer to write (last r) instead of (first r)
     ;; here, but it's the same either way since we know the length of
     ;; 'r' must be exactly 1 here.
     ((= lenf 0) 
      (first (Deque-r q)))

     (else 
      (first (Deque-f q))))))

;; Returns the first element from the rear, or failure-result if none.
(define-with-default* (dq-car-r q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0))
      (was-empty))

     ((= lenr 0) 
      (first (Deque-f q)))

     (else 
      (first (Deque-r q))))))

;; Drops the first element from the front.
(define-with-default* (dq-cdr-f q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) (was-empty))
     ((= lenr 0) dq-null)
     (else (check (- lenf 1) (rest (Deque-f q)) lenr (Deque-r q))))))

;; Drops the first element from the rear.
(define-with-default* (dq-cdr-r q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) (was-empty))
     ((= lenf 0) dq-null)
     (else (check lenf (Deque-f q) (- lenr 1) (rest (Deque-r q)))))))

;; Return first element from the front, plus all but the first element
;; from the front. Roughly the same semantics as for (values
;; (dq-car-f q) (dq-cdr-f q)), but more efficient. Note that here
;; the failure result should be two values.
(define-with-default* (dq-pop-f q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) 
      (was-empty))
     ((= lenf 0) 
      (values (first (Deque-r q)) dq-null))
     (else 
      (let ((f (Deque-f q)))
	(values (first f)
		(check (- lenf 1) (rest f) lenr (Deque-r q))))))))

;; Return first element from the rear, plus all but the first element
;; from the rear.
(define-with-default* (dq-pop-r q) was-empty
  (let ((lenf (Deque-lenf q))
        (lenr (Deque-lenr q)))
    (cond
     ((and (= lenf 0) (= lenr 0)) 
      (was-empty))
     ((= lenr 0) 
      (values (first (Deque-f q)) dq-null))
     (else 
      (let ((r (Deque-r q)))
	(values (first r)
		(check lenf (Deque-f q) (- lenr 1) (rest r))))))))

;; Appends the given deques into a single one. Certainly not a
;; constant-time operation.
;; E.g., (dq-append (dq 1 2 3) (dq 4 5) (dq 6))
(define* (dq-append . qs)
  (for/fold ((q dq-null)) ((qa qs))
    (let ((lenf (Deque-lenf q))
          (lenr (Deque-lenr q))
          (qa-lenf (Deque-lenf qa))
          (qa-lenr (Deque-lenr qa)))
      (cond
       ((and (= lenf 0) (= lenr 0)) qa)
       ((and (= qa-lenf 0) (= qa-lenr 0)) q)
       (else 
        (check lenf
               (Deque-f q)
               (+ lenr qa-lenf qa-lenr)
               (append (Deque-r qa) (reverse (Deque-f qa)) (Deque-r q))))))))

;; This should still be a linear time operation despite the 'reverse'.
;; But linear space is also required.
(define* (dq-for-each q f)
  (for-each f (Deque-f q))
  (for-each f (reverse (Deque-r q))))

;; A constant-time operation.
(define* (dq-reverse q)
  (Deque (Deque-lenr q) (Deque-r q)
         (Deque-lenf q) (Deque-f q)))

#|

Except where otherwise noted, all code is authored by Tero Hasu,
copyright University of Bergen, and the following license applies.

Copyright (C) 2013 University of Bergen.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
