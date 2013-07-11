#lang racket

#|

The intention is for this module to be always required for-syntax, as
the information is only accessed at macro expansion time.

Note quite sure if we should specify any #:phase for the identifier
tables.

|#

(require "util.rkt")
(require syntax/id-table)

;;; 
;;; Type annotations.
;;; 

(define type-table (make-free-id-table))
(provide type-table)

(struct Type ())
(provide Type?)

(define-values (struct:AnyT make-AnyT AnyT? AnyT-ref AnyT-set!)
  (make-struct-type 'AnyT struct:Type 0 0))
(define AnyT (make-AnyT))
(provide AnyT AnyT?)

;; n is a symbol
(struct TypeName Type (n) #:transparent)
(provide (struct-out TypeName))

(define* (record-type! id-stx t)
  (free-id-table-set! type-table id-stx t))
