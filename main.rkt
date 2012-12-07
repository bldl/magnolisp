#lang racket

#|

Defines the language for Magnolisp. This really just grabs the input
programs as syntax objects, and then transforms and evaluates those to
implement language semantics. Racket is a foreign language to an input
program.

|#

(require "util.rkt")
(require (for-syntax racket "util.rkt"))

(provide (rename-out (my-module-begin #%module-begin)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
      ((_ body ...)
       (begin
         (for-each
          pretty-print
          (syntax->datum #'(body ...)))
         #`(#%module-begin
            )))))
