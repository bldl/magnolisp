#lang racket

#|

One problem here is that 'syntax-local-make-definition-context' can
only be called at transformation time. The 'syntax/toplevel' can help
us get a transformation going.

We could choose to have a more sophisticated wrapper than 'begin'
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

;; Need to import this so that gets correct binding.
(require (only-in "runtime-compiler.rkt" %compilation-unit))

;; cannot use unless transforming
;(expand-import #'"util.rkt")

;(resolve-module-path "util.rkt" #f)

;; (expand-syntax-top-level-with-compile-time-evals
;;  (namespace-syntax-introduce
;;   #`(%compilation-unit #,@stx-lst)))

;;(module main '#%kernel body ...)
  
(define* (compile-file pn)
  (define stx-lst (load-as-syntaxes pn))
  (let ((ns (make-base-empty-namespace)))
    (parameterize ((current-namespace ns))
      ;;(namespace-require '(for-syntax racket/base))
      ;;(namespace-require '"runtime-compiler.rkt")
      ;;(namespace-require '"runtime-evaluator.rkt")
      (let ((core-stx
             (expand
              #`(module main "runtime-compiler-lang.rkt"
                  #,@stx-lst))))
        (pretty-println (syntax->datum core-stx))))))

(define* (compile-module mn)
  (compile-file (resolve-module-path mn #f)))

(compile-module "try-program-3.rkt")
