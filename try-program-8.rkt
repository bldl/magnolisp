#lang magnolisp

#|

A macro for declaring a pair of procedures.

|#

(require/racket (prefix-in r. racket/base))
(require (for-syntax racket/function racket/list racket/syntax))

(primitive (getter)
           {(r.#%app r.displayln "get")}
           {"get_it();"})

(primitive (setter)
           {(r.#%app r.displayln "set")}
           {"set_it();"})

(define-syntax (accessor stx)
  (syntax-case stx ()
    ((_ n)
     (let ((get-n (format-id stx "get-~a" (syntax-e #'n)))
           (set-n (format-id stx "set-~a" (syntax-e #'n))))
       #`(begin
           (procedure (#,get-n) (call getter))
           (procedure (#,set-n) (call setter)))))))

(accessor x)

(call get-x)
(call set-x)
(call get-x)

(procedure (#^export main)
           (call set-x))
