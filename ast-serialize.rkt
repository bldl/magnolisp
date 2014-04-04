#lang racket/base

#|
|#

(require "util.rkt"
         (for-syntax racket/base
                     "syntax-quote.rkt" "syntax-quote-extras.rkt"))

(define-syntax* (quote-syntax/keep-properties stx)
  (syntax-case stx ()
    [(_ e #:listed (sym ...))
     (syntax-preserve/loc+listed (syntax->datum #'(sym ...)) #'e)]
    [(_ e #:all)
     (syntax-preserve/loc+all #'e)]
    [(_ e)
     (syntax-preserve/loc+none #'e)]))

(module* test #f
  (require racket rackunit)

  (define y 7)
  (define stx-for-y (quote-syntax/keep-properties y))
  (check-pred syntax? stx-for-y)
  (check-pred syntax-position stx-for-y)
  (check-false (syntax-property stx-for-y 'paren-shape))
  (check-eqv? (eval-syntax stx-for-y) y)

  (define stx-for-p
    (quote-syntax/keep-properties [y] #:listed (paren-shape)))
  (check-pred syntax? stx-for-p)
  (check-pred syntax-position stx-for-p)
  (check-not-false (syntax-property stx-for-p 'paren-shape))

  (define stx-for-a
    (quote-syntax/keep-properties [y] #:all))
  (check-pred syntax? stx-for-a)
  (check-pred syntax-position stx-for-a)
  (check-not-false (syntax-property stx-for-a 'paren-shape)))

  
