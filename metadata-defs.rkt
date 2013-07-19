#lang racket

#|

Information about definitions. The frontend creates such values in
phase 1, whereas the backend uses the values in phase 0.

|#

(require "util.rkt")

;;; 
;;; Type annotations.
;;; 

(struct Type ())
(provide Type?)

(define-values (struct:AnyT make-AnyT AnyT? AnyT-ref AnyT-set!)
  (make-struct-type 'AnyT struct:Type 0 0))
(define AnyT (make-AnyT))
(provide AnyT AnyT?)

;; n is a symbol
(struct TypeName Type (n) #:transparent)
(provide (struct-out TypeName))

