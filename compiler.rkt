#lang racket

#|

One problem here is that 'syntax-local-make-definition-context' can
only be called at transformation time. The 'syntax/toplevel' can help
us get a transformation going.

We could choose to have a more sophisticated wrapper than 'begin' to
for the read list of syntax objects. We could use
'syntax-local-make-definition-context', for example, to have all
top-level names resolved for us. We could then also invoke
'local-expand' with a stoplist specifying our core language, in
addition to any parts of Racket that we choose to include in our core
language.

|#

(require "reader-ext.rkt")
(require "util.rkt")
(require racket/require-transform)
(require syntax/modcode syntax/moddep syntax/modresolve) 
(require syntax/toplevel)

;; cannot use unless transforming
;(expand-import #'"util.rkt")

;(resolve-module-path "util.rkt" #f)

(define* (compile-file pn)
  (define stx-lst (load-as-syntaxes pn))
  (let ((ns (make-base-empty-namespace)))
    (parameterize ((current-namespace ns))
      (namespace-require '(for-syntax racket/base))
      (namespace-require '"runtime-compiler.rkt")
      ;;(namespace-require '"runtime-evaluator.rkt")
      (let ((core-stx
             (expand-syntax-top-level-with-compile-time-evals
              (namespace-syntax-introduce
               #`(begin #,@stx-lst)))))
        (syntax->datum core-stx)))))

(define* (compile-module mn)
  (compile-file (resolve-module-path mn #f)))

(compile-module "try-program-4.rkt")

#;
(define-syntax (my-module-begin stx)
  (syntax-case stx ()
    ((_ body ...)
     (let ((code
            (let ((ns (make-base-empty-namespace)))
              (parameterize ((current-namespace ns))
                (namespace-require '(for-syntax racket))
                (namespace-require 'racket/base) ;; xxx we do not really want to do this, but may need to have some of it for macro expansion to work -- but does not appear to make it happen -- we only appear to be getting partial results, though -- forms such as 'unless' do expand, but all macro applications
                (namespace-require '"runtime-compiler.rkt")
                (expand-syntax-top-level-with-compile-time-evals
                 ;; namespace-syntax-introduce seems to make no difference -- xxx how do we get our macros to expand, the ones given in runtime.rkt
                 (namespace-syntax-introduce #'(begin body ...))))))
           (%code (format-id stx "%code")))
       #`(#%plain-module-begin
          (define #,%code '#,code) ;;xxx store expanded form -- note that expansion requires "runtime" library (for anno macros etc.) -- we must also be able to access some configuration information for compiler options (do not know how yet)
          (provide #,%code))))))
