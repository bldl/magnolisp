#lang racket/base

#|

The definitions in this module are mostly exported for-syntax, as the
information is only accessed at macro expansion time.

|#

(require (for-syntax "annos-util.rkt" "compiler-util.rkt" "util.rkt"
                     racket/base racket/contract racket/dict racket/pretty
                     syntax/id-table))

;;; 
;;; DefInfo recording
;;; 

(begin-for-syntax

 (define* definfo-table-f (make-free-id-table #:phase 0))
 (define* definfo-table-b (make-bound-id-table #:phase 0))

 ;; Adds the specified annotations for the specified binding. Full
 ;; annotation forms must be given. The name of each annotation must
 ;; be extractable from the respective form.
 ;;
 ;; E.g., (set-definfo! #'x (list #'(a 1) #'(b 2 3)))
 (define-with-contract*
   (->* (identifier? (listof syntax?)) (#:bound? boolean?) void?)
   (set-definfo! id-stx stx-lst #:bound? [bound? #f])
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
   (dict-update! (if bound? definfo-table-b definfo-table-f)
                 id-stx
                 (lambda (h)
                   (if h
                       (hash-set/assocs h assocs)
                       (make-immutable-hasheq assocs)))
                 #f))

 ) ; end begin-for-syntax
