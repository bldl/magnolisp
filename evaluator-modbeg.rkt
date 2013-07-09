#lang racket

#|

In our custom #%module-begin we do not macro expand evaluator
language, as that language is different. We cannot easily share work
between macro expanding evaluated and compiled language. All that we
do is macro expand and then store compiled language.

'local-expand' uses the lexical context of the expression currently
being expanded. In here the outermost expansion is that of
#%module-begin and happens in the lexical context of said module. So
that is not the language we want. We instead wrap into a different
module definition and just expand the whole thing using
'expand-syntax'.

|#

(provide my-module-begin)

(begin-for-syntax
 (require "util.rkt" syntax/strip-context)

 (define (make-ast-submodule modbeg-stx stx-lst)
   (define stx
     (parameterize ([current-namespace (make-base-namespace)])
       (expand-syntax
        (namespace-syntax-introduce
         (strip-context
          #`(module some "compiler-language.rkt"
              #,@stx-lst))))))

   (pretty-println (syntax->datum stx))
    
   ;; Note that we use racket/base here as this is simply a Racket
   ;; module containing annotations. It is not Magnolisp. The data
   ;; structures containing the annotations will be in Racket.
   #`(module ast racket/base
       (begin))))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ x ...)
     #`(#%module-begin
        x ...
        #,(make-ast-submodule stx (syntax->list #'(x ...)))))))
