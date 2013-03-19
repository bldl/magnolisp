#lang racket

#|

One problem here is that 'syntax-local-make-definition-context' can
only be called at transformation time. The 'syntax/toplevel' module
can help us get a transformation going. Or we may just use 'expand'.

We could choose to have a more sophisticated wrapper than 'begin' or
'module' for the input list of syntax objects. We could use
'syntax-local-make-definition-context', for example, to have all
top-level names resolved for us. We could then also invoke
'local-expand' with a stoplist specifying our core language, in
addition to any parts of Racket that we choose to include in our core
language. '%compilation-unit' has this idea, but we start with
something simpler.

A 'module' appears to be self-contained so that anything in outer
scope does not affect it, and hence 'namespace-require' does not
affect it.

See "Fully Expanded Programs" in Racket reference, as well as our
runtime library to see what we can expect to remain in the syntax tree
to be compiled to C++.

Note that if we want the 'expand' namespace to have the same module
instances, for sharing data or such, we must take care of appropriate
module attaching.

|#

(require "parse.rkt")
(require "reader-ext.rkt")
(require "util.rkt")
(require racket/require-transform)
(require syntax/modcode syntax/moddep syntax/modresolve) 
(require syntax/strip-context)
(require syntax/toplevel)

;; Need to import this so that gets correct binding.
;(require (only-in "runtime-compiler.rkt" %compilation-unit))

;; cannot use unless transforming
;(expand-import #'"util.rkt")

;(resolve-module-path "util.rkt" #f)

;; (expand-syntax-top-level-with-compile-time-evals
;;  (namespace-syntax-introduce
;;   #`(%compilation-unit #,@stx-lst)))

;;(namespace-require '(for-syntax racket/base))
;;(namespace-require '"runtime-compiler.rkt")
;;(namespace-require '"runtime-evaluator.rkt")
;;(namespace-require '(only racket/base module))
;;(namespace-require '"runtime-compiler-lang.rkt")

(define* (compile-file pn)
  (define stx-lst (load-as-syntaxes pn))
  (define in-stx
    (strip-context
     ;; Initial bindings for 'module' body forms come from the
     ;; language specified here. The binding for the 'module' form
     ;; itself must come from elsewhere.
     #`(module main "runtime-compiler-lang.rkt"
         #,@stx-lst)))
  ;;(print-stx-with-bindings in-stx)
  (let ((this-ns (current-namespace))
        (ns (make-empty-namespace)))
    ;; Cannot do this unless import into this-ns (possibly renamed).
    ;;(namespace-attach-module this-ns '"runtime-compiler-lang.rkt" ns)
    (parameterize ((current-namespace ns))
      (namespace-attach-module this-ns 'racket/base)
      (namespace-require '(only racket/base module))
      ;;(namespace-require '"runtime-compiler-lang.rkt")
      (let ((in-stx (namespace-syntax-introduce in-stx)))
        (let ((core-stx (expand-syntax in-stx)))
          (pretty-println (syntax->datum core-stx))
          ;;(print-stx-with-bindings core-stx)
          (let ((core-ast (parse core-stx)))
            (pretty-println core-ast)
            ))))))

(define* (compile-module mn)
  (compile-file (resolve-module-path mn #f)))

(compile-module "try-program-4.rkt")