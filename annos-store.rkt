#lang racket

#|

The definitions in this module are mostly exported for-syntax, as the
information is only accessed at macro expansion time.

|#

(require (for-syntax "annos-util.rkt" "compiler-util.rkt" "util.rkt"
                     racket/contract racket/dict racket/pretty
                     syntax/id-table))

;;; 
;;; DefInfo recording
;;; 

(begin-for-syntax

 (define* definfo-table (make-bound-id-table #:phase 0))

 ;; Copies definition annotations from the syntax properties of the
 ;; given syntax object. If not provided, the annotations are taken
 ;; from the identifier itself.
 (define* (set-definfo-from-stx! id-stx [def-stx id-stx])
   ;; hasheq(name-symbol -> value-syntax)
   (define info (syntax-get-annos def-stx))
   (dict-update! definfo-table id-stx
                 (lambda (h)
                   (if h (hash-merge h info) info)) #f))

 ;; Adds the specified annotations for the specified binding. Full
 ;; annotation forms must be given. The name of each annotation must
 ;; be extractable from the respective form.
 ;;
 ;; E.g., (set-definfo! #'x (list #'(a 1) #'(b 2 3)))
 (define-with-contract*
   (-> identifier? (listof syntax?) void?)
   (set-definfo! id-stx stx-lst)
   (define assocs (map
                   (lambda (stx)
                     (define n (form-get-name stx))
                     (unless n
                       (raise-language-error
                        (syntax-e id-stx)
                        "no name in annotation form"
                        stx))
                     (cons n stx))
                   stx-lst))
   (dict-update! definfo-table id-stx
                 (lambda (h)
                   (if h
                       (hash-set/assocs h assocs)
                       (make-immutable-hasheq assocs)))
                 #f))

 ) ; end begin-for-syntax
