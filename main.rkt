#lang racket/base

#|

Defines a Racket module language for Magnolisp.

|#

(provide 
 ;; `require` forms
 require only-in except-in prefix-in rename-in combine-in
 relative-in only-meta-in
 local-require
         
 ;; `provide` forms
 provide all-defined-out all-from-out rename-out except-out
 prefix-out struct-out combine-out protect-out
         
 ;; `require` and `provide` forms
 for-syntax for-template for-label for-meta
 
 ;; top-level syntax
 begin-for-syntax define-syntax define-syntaxes define-syntax-rule
 
 ;; top-level definitions
 define-values
 
 ;; quotation
 quote quasiquote unquote
 
 ;; expressions
 #%top #%expression #%datum #%top-interaction
 #%plain-app (rename-out [#%plain-app #%app])
 begin begin0
 let let* letrec 
 let-values let*-values letrec-values 
 let-syntax let-syntaxes letrec-syntax letrec-syntaxes
 letrec-syntaxes+values
 set! set!-values
 #%plain-lambda (rename-out [#%plain-lambda lambda] [#%plain-lambda λ])
 if when unless
 values void)

(require (for-syntax racket/base))
(provide (for-syntax (all-from-out racket/base)))

(require "core.rkt")
(provide (all-from-out "core.rkt"))

(require "modbeg.rkt")
(provide (rename-out [module-begin #%module-begin]))

(require "surface.rkt")
(provide (all-from-out "surface.rkt"))
