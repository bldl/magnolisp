#lang racket

#|

The definitions in this module are mostly exported for-syntax, as the
information is only accessed at macro expansion time.

|#

(require "annos-util.rkt" "util.rkt"
         (for-syntax "util.rkt"
                     racket/dict racket/pretty syntax/id-table))

;;; 
;;; DefInfo recording
;;; 

(begin-for-syntax

 (define* definfo-table (make-bound-id-table #:phase 0))

 ;; Copies definition annotations from the syntax properties of the
 ;; given syntax object. If not provided, the annotations are taken
 ;; from the identifier itself.
 (define* (record-definfo! id-stx [def-stx id-stx])
   ;; hasheq(name-symbol -> value-syntax)
   (define info (get-annos def-stx))
   (dict-update! definfo-table id-stx
                 (lambda (h)
                   (if h (hash-merge h info) info)) #f))

 ;; Adds the specified annotations for the specified binding.
 ;; The keys must be symbols, and the values must be syntax.
 ;; E.g., (definfo! #'x 'a #'1 'b #'2)
 (define* (definfo! id-stx . k-v)
   (dict-update! definfo-table id-stx
                 (lambda (h)
                   (apply hash-set* h k-v)) #hasheq()))
 
 ) ; end begin-for-syntax
