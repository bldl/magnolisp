#lang racket/base

#|

Defines a language for the implementation of Magnolisp. The only
addition to "racket", really, is support for annotations that enable
implementation of "IDE support".

TODO We do require some new 'provide' forms, but do not know how to
make them available here. We can hopefully reuse the existing metadata
parsing routines to implement the new forms.

|#

(require racket)
(provide (except-out (all-from-out racket) #%module-begin))

(require "implementation-modbeg.rkt")
(provide (rename-out (module-begin #%module-begin)))

;; this should already be in "racket"
;;(require (for-syntax racket/base))
;;(provide (for-syntax (all-from-out racket/base)))

(require "util.rkt")
(provide (all-from-out "util.rkt"))
