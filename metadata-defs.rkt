#lang racket

#|

Information about definitions. The frontend creates such values in
phase 1, whereas the backend uses the values in phase 0.

|#

(require "util.rkt")

;;; 
;;; Information about definitions.
;;; 

#|

name is a symbol specifying the original name.

type is a Type, or #f for untyped constructs.

modifs is a list of {external, verbatim}
where
  - 'external means an externally defined function (in C++)
  - 'verbatim means that body is foreign language (C++)

doc is a docstring for the defined name.

emacs-indent is symbol? or integer?
where
  - a symbol? names an Emacs Lisp function, except
    for 'defun, which is special
(see 'scheme-indent-function in Emacs)

emacs-highlight is a list of (symbol? . symbol?) where the first symbol
is a symbol to highlight, whereas the second symbol specifies an Emacs
highlighting style, or some abstract alias thereof.

emacs-dictionary is a list of (string? . (or/c string? #f)) where the
first string is a dictionary word for auto completion, and the
second (if any) is a hover help string.

|#
(struct DefInfo 
	(name type modifs doc 
	      emacs-indent emacs-highlight emacs-dictionary)
	#:transparent #:mutable)
(provide (struct-out DefInfo))

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
