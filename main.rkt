#lang racket

#|

Defines a Racket module language for Magnolisp. This really just grabs
the input programs as syntax objects, and then transforms them into
our internal representation. Currently the forms are merely displayed.

We will have a separate API for compiling/evaluating/etc., and we
might consider hooking up our #lang with the evaluator, for convenient
testing.

|#

(require (for-syntax racket/syntax
                     syntax/toplevel
                     "util.rkt"))

;;; 
;;; exports (only needed for %code export)
;;; 

(require racket/base)
(provide (rename-out (my-module-begin #%module-begin)))
(provide (except-out (all-from-out racket/base) #%module-begin))

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

;;; 
;;; transformations
;;; 

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ body ...)
     (let ((code
            (let ((ns (make-base-empty-namespace)))
              (parameterize ((current-namespace ns))
                (namespace-require '(for-syntax racket))
                (namespace-require 'racket/base) ;; xxx we do not really want to do this, but may need to have some of it for macro expansion to work -- but does not appear to make it happen -- we only appear to be getting partial results, though -- forms such as 'unless' do expand, but all macro applications
                (namespace-require '"runtime.rkt")
                (expand-syntax-top-level-with-compile-time-evals
                 ;; namespace-syntax-introduce seems to make no difference -- xxx how do we get our macros to expand, the ones given in runtime.rkt
                 (namespace-syntax-introduce #'(begin body ...))))))
           (%code (format-id stx "%code")))
       #`(#%plain-module-begin
          (define #,%code '#,code) ;;xxx store expanded form -- note that expansion requires "runtime" library (for anno macros etc.) -- we must also be able to access some configuration information for compiler options (do not know how yet)
          (provide #,%code))))))
