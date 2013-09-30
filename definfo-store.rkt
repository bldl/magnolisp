#lang racket

#|

The definitions in this module are mostly exported for-syntax, as the
information is only accessed at macro expansion time.

|#

(require "annos.rkt" "util.rkt"
         (for-syntax "util.rkt" racket/pretty syntax/id-table))

;;; 
;;; DefInfo recording
;;; 

(begin-for-syntax

 (define* definfo-table (make-bound-id-table #:phase 0))
 
 (define* (record-definfo! id-stx def-stx)
   ;; hasheq(name-symbol -> value-syntax)
   (define info (get-annos def-stx))
   (bound-id-table-set! definfo-table id-stx info))

 ) ; end begin-for-syntax
