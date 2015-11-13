#lang racket/base

#|
|#

(require "module.rkt" data/order racket/bool
         (for-syntax racket/base syntax/parse))

(define* number-order
  (order 'number-order number? = <))

(define* string-order ;; from Racket docs
  (order 'string-order string? string=? string<?))

(define (symbol-comparator x y)
  (define x-s (symbol->string x))
  (define y-s (symbol->string y))
  (cond
   ((string=? x-s y-s) '=)
   ((string<? x-s y-s) '<)
   (else '>)))

(define* symbol-order
  (order 'symbol-order symbol? symbol-comparator))

(define boolean<?
  (lambda (x y)
    (and (not x) y)))

(define* boolean-order
  (order 'boolean-order boolean? boolean=? boolean<?))

(define-syntax* (define-comparator stx)
  (syntax-parse stx
    [(_ n:id [k:expr e:expr] ...)
     (with-syntax ([(key ...) (generate-temporaries #'(k ...))]
                   [(cmp ...) (generate-temporaries #'(e ...))])
       #'(define n
           (let ([cmp e] ...
                 [key k] ...)
             (lambda (x y)
               (cond
                 [(let ([r (cmp (key x) (key y))])
                    (and (not (eq? r '=)) r)) => (lambda (x) x)]
                 ...
                 [else '=])))))]))
