#lang racket/base

#|

The variable(s) encoding Magnolisp core syntax are here. They must be
shared across all surface languages, as the Magnolisp parser must have
a consistent understanding of the identifier(s) across language
variants.

|#

(provide #%magnolisp CORE Bool Void)

;;; 
;;; core syntax
;;; 

(define #%magnolisp #f)

;; If is not okay to use `(and #f ...)` here, as `and` may insert an
;; `#%expression` form in the middle, which our parser does not
;; recognize as the particular core syntax.
(define-syntax-rule (CORE kind arg ...)
  (if #f (#%magnolisp kind arg ...) #f))

;;; 
;;; compiler built-ins
;;; 

(define Bool #f)
(define Void #f)
