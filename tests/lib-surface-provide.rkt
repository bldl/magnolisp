#lang racket/base

#|
|#

(require 
 (relative-in magnolisp 
              "surface.rkt" "util.rkt"
              (for-syntax "util.rkt"
                          racket/base racket/syntax 
                          syntax/parse)))

(define-syntax-rule* (typedef* n rest ...)
  (begin
    (typedef n rest ...)
    (provide n)))

(define-syntax-rule* (function* (f p ...) rest ...)
  (begin
    (function (f p ...) rest ...)
    (provide f)))

(define-syntax* primitives*
  (syntax-rules (::)
    [(_) (begin)]
    [(_ [#:type t] . more)
     (begin 
       (typedef* t #:: (foreign))
       (primitives* . more))]
    [(_ [#:function form :: t] . more)
     (begin 
       (function* form #:: ([type t] foreign))
       (primitives* . more))]))
