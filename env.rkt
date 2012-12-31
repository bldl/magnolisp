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
  (hash-ref env name
            (thunk (error 'env-get "undefined name: ~a" name))))

;; 'id' is a 'gensym'ed unique name for the named thing, which is (1)
;; a special form, (2) a macro, or (3) a variable. Top-level names
;; must be unique, and have their name as their 'id'.
(define-struct* binding (id) #:transparent)
(define-struct* special-b binding (eval compile) #:transparent)
(define-struct* macro-b binding (expand mode) #:transparent)
(define-struct* var-b binding (decl) #:transparent)