#lang racket

#|

A pretty printer library derived from code in Pretty Good Formatter.

https://github.com/nuthatchery/pgf

|#

(require "util.rkt")

;;; 
;;; indentation levels
;;; 

(abstract-struct* Lv () #:transparent)

(define-syntax-rule (define-level* n fld-spec ...)
  (concrete-struct* n Lv (fld-spec ...) #:transparent))

(define-level* LvInc n) ;; integer -> Lv
(define-level* LvStr s) ;; string -> Lv
(define-level* LvAbs n) ;; integer -> Lv
(define-level* LvRel n) ;; integer -> Lv
(define-level* LvPop) ;; -> Lv

;;; 
;;; tokens
;;; 

(abstract-struct* Token () #:transparent)

(define-syntax-rule (define-token* n fld-spec ...)
  (concrete-struct* n Token (fld-spec ...) #:transparent))

(define-token* Text s) ;; unbreakable text
(define-token* Line) ;; forced line break
(define-token* Nest lv) ;; nest open or close
(define-token* Union l r) ;; choice
(define-token* Width w) ;; 'w' is a rational
(define-token* Flush) ;; forces flushing of buffers

(define* Flush-instance (Flush))
(define* Line-instance (Line))
(define* LvPop-instance (Nest (LvPop)))

;;; 
;;; token sequences
;;; 

(require "util/bankers-deque.rkt")

(define* (tseq? x)
  (or (dq? x)
      (null? x) (pair? x)
      (Token? x) (string? x)
      (promise? x)))

(define* tseq-null dq-null)

;; Optimizes for the common, performance critical case, namely
;; prepending or appending a token into a sequence using the
;; constructor.
(define* tseq
  (case-lambda
    [() tseq-null]
    [(s) (dq s)]
    [(s1 s2) (cond
              ((and (dq? s1) (dq? s2))
               (dq-append s1 s2))
              ((dq? s1)
               (dq-cons-r s1 s2))
              ((dq? s2)
               (dq-cons-f s2 s1))
              ((null? s1)
               (dq s2))
              ((null? s2)
               (dq s1))
              (else
               (dq s1 s2)))]
    [ss (apply dq ss)]))

(define* (tseq-cons e s)
  (cond
   ((dq? s) (dq-cons-f s e))
   ((null? s) (dq e))
   (else (dq e s))))

(define* (tseq-put s e)
  (cond
   ((dq? s) (dq-cons-r s e))
   ((null? s) (dq e))
   (else (dq s e))))

(define-syntax-rule* (tseq/lazy s ...)
  (tseq (lazy s) ...))

;; Must account for all tseq constructors.
(define* (tseq-get s)
  (define (f h t)
    (if (not h)
        (tseq-get t) ;; allow #f within a dq or list
        (let-values (((hh ht) (tseq-get h)))
          (if (not hh)
              (tseq-get t)
              (values hh (cons ht t))))))
  
  (cond
   ((pair? s) (f (car s) (cdr s)))
   ((dq? s) (if (dq-null? s)
                (values #f tseq-null)
                (call-with-values (thunk (dq-pop-f s)) f)))
   ((null? s) (values #f tseq-null))
   ((Token? s) (values s tseq-null))
   ((string? s) (values (Text s) tseq-null))
   ((promise? s) (tseq-get (force s)))
   (else
    (raise-argument-error 'tseq-get "tseq" s))))

(define* (tseq-first s)
  (let-values (((e r) (tseq-get s))) e))

(define* (tseq-rest s)
  (let-values (((e r) (tseq-get s)))
    (unless e
      (raise-argument-error 'tseq-rest "non-empty tseq" s))
    r))

(define* (tseq-null? s)
  (not (tseq-first s)))

(define* (tseq->list s)
  (let loop ((r '()) (s s))
    (let-values (((h t) (tseq-get s)))
      (if (not h) (reverse r)
          (loop (cons h r) t)))))

(define* (tseq-for-each s f)
  (let-values (((h t) (tseq-get s)))
    (when h
      (f h)
      (tseq-for-each t f))))

;; E.g., (for/list ((t (in-tseq '("a" "b" "c")))) t)
(define* (in-tseq s)
  (make-do-sequence
   (thunk
    (values tseq-first ;; current position -> current element
            tseq-rest ;; current position -> next position
            s ;; initial position
            (negate tseq-null?) ;; current position -> whether at end
            #f
            #f))))

;; E.g., (for/list ((t (in-tseq (tseq-add-between '("a" "b" "c") ",")))) t)
(define* (tseq-add-between s e)
  (let next ((s s))
    (lazy
     (let-values (((h t) (tseq-get s)))
       (if (not h)
           tseq-null
           (if (tseq-null? t)
               h
               (tseq h e (next t))))))))

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
