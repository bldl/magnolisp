#lang racket

#|

Environment. Based directly on Rackets functional-update hash with eq?
comparisons, but we nonetheless treat it as an abstract data type.

|#

(require "util.rkt")

(provide (rename-out
          (hasheq env-new)
          (hash? env?)
          (hash-set env-set)
          (hash-ref env-ref)
          (hash-has-key? env-has?)
          (hash-remove env-remove)
          (hash-keys env-names)))

(define* (env-get env name)
  (hash-ref env name #f))
  
(define* (env-must-get env name)
  (hash-ref env name
            (thunk (error 'env-get "undefined name: ~a" name))))

;; 'id' is a 'gensym'ed unique name for the named thing, which is (1)
;; a special form, (2) a macro, or (3) a variable. Top-level names
;; must be unique, and have their name as their 'id'.
(define-struct* binding (id) #:transparent)
(define-struct* expandable binding (expand? expand) #:transparent)
(define-struct* special-binding expandable (eval compile) #:transparent)
(define-struct* macro-binding expandable () #:transparent)
(define-struct* var-binding binding (decl) #:transparent)
