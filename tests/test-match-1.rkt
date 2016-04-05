#lang magnolisp

#|

So `match` just happens to work to some extent, depending on what kind
of patterns are used.

|#

(require racket/match (only-in racket/bool boolean=?))

(declare (boolean=? x y)
  #:: (foreign ^(-> Bool Bool Bool)))

(define (nice? x)
  (boolean=? x #t))

(define (main-match x) #:: (export ^(-> Bool Bool))
  (match x
    [(? nice?) #t]
    [_ #f]))

(main-match #f)
(main-match #t)
