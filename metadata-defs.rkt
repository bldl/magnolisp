#lang racket

#|

Data types used for annotations. The frontend creates (syntax for)
such values in phase 1, whereas the backend uses the values in phase
0.

|#

(require "util.rkt")

;;; 
;;; Information about definitions.
;;; 

#|

We use a bound-id-table for storing information about identifiers. The
key being the ID, and the value being a hasheq, keyed by the
following:

  - 'type :: a Type for each declared typed construct

  - 'entry-point :: a #t value for each library entry point

The 'verbatim and 'external property of an operation body is encoded
in the AST, and no table is required for that information. This really
is a property of the body, and not of the signature.

  - 'external means an externally defined function (in C++)

  - 'verbatim means that body is foreign language (C++)

The reason we are using a /bound/ id-table is that whenever we are
adding a metadata record it is for a freshly bound definition - we
never add a new metadata record for the same identifier.

Note quite sure if we should specify any #:phase for the identifier
tables, but phase 0 would seem appropriate as all Magnolisp names are
such.

|#

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

(struct FunT Type (ats rt) #:transparent)
(provide (struct-out FunT))
