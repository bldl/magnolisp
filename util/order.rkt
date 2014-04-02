#lang racket/base

#|
|#

(require "module.rkt" data/order racket/bool)

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



