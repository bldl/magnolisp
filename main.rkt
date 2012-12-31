#lang racket

#|

Defines a Racket module language for Magnolisp. This really just grabs
the input programs as syntax objects, and then transforms then into
our internal representation. Currently the forms are merely displayed.

We will have a separate API for compiling/evaluating/etc., and we
might consider hooking up our #lang with the evaluator, for convenient
testing.

|#

(require (for-syntax racket racket/stxparam
                     "form.rkt" "syntax.rkt" "util.rkt"))

(provide (rename-out (printing-module-begin #%module-begin)))

;; For debugging. Produces and prints IR.
(define-syntax (printing-module-begin stx)
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
