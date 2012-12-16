#lang racket

#|

Defines the language for Magnolisp. This really just grabs the input
programs as syntax objects, and then transforms and evaluates those to
implement language semantics. Racket is a foreign language to an input
program.

|#

(require (for-syntax racket "form.rkt" "syntax.rkt" "util.rkt"))

(provide (rename-out (my-module-begin #%module-begin)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
      ((_ body ...)
       (begin
         (let ((f (let ((lst (syntax->list #'(body ...))))
                    (syntax-list->form lst (annos/stx stx)))))
           (pretty-print f)
           (parameterize ((print-annos? #t))
             (pretty-print f))
           (pretty-print (form->datum f)))
         #`(#%module-begin)))))
