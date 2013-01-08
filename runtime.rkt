#lang racket/base

#|
|#

(require "util.rkt")

(define-syntax-rule* (cool-return x)
  (my-return x))

(define (my-return . xs)
  `(ast:return ,xs))

(provide (rename-out (my-return return)))


