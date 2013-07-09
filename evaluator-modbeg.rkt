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

As we store the AST as code, the challenge is in making sure we do not
lose binding information in the process. We must still be able to
compare names for equality. We also do not want to lose location
information.

Note the interesting thing about separate namespaces for the two
Magnolisp implementations. Any macro expansion time language should
automatically use the evaluatable language, even if it is require
for-syntax or whatever, since requiring #lang magnolisp module will
make exactly that happen. Only the compiler explicitly loads the
submodule that contains the compile-time language. This then also
means that we must somehow be able to resolve module paths in the
compiler. This may not be such a big problem provided we make sure
that we can get macro expansion done fully without such explicit work.

|#

(provide my-module-begin)

(begin-for-syntax
 (require "util.rkt" syntax/strip-context)

 (define (make-ast-submodule modbeg-stx stx-lst)
   ;; Note that we use racket/base here as this is simply a Racket
   ;; module containing annotations. It is not Magnolisp. The data
   ;; structures containing the annotations will be in Racket.
   #`(module ast racket/base
       (provide src-sexp-lst)
       ;; xxx otherwise good but we lose location info - should instead generate code that creates syntax objects with location information
       (define src-sexp-lst '(#,@stx-lst)))))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ x ...)
     #`(#%module-begin
        x ...
        #,(make-ast-submodule stx (syntax->list #'(x ...)))))))
