#lang racket

#|

Implements a compiler for Magnolisp. Loads the code to be compiled
from Racket module metadata, included as submodules by the Racket
'magnolisp' language implementation. Only bytecode for submodules
requires loading for compilation.

The compiler ignores top-level expressions, which is not the case for
the evaluator.

The compiler requires a fully typed program (although not all types
have to be written out explicitly -- think 'auto' in C++).

|#

(require "util.rkt")
(require racket/require-transform)
(require syntax/modcode syntax/moddep syntax/modresolve) 
(require syntax/strip-context)
(require syntax/toplevel)

(define (my-expand-macros/syntax stx)
  (parameterize ([current-namespace (make-base-namespace)])
    (expand-syntax
     (namespace-syntax-introduce
      (strip-context stx)))))

(define (my-expand-macros/sexp sexp)
  (parameterize ([current-namespace (make-base-namespace)])
    (expand sexp)))

(define (to-module-syntax sexp-lst)
  (datum->syntax #f 
                 `(module some "compiler-language.rkt"
                    ,@sexp-lst)))

(define* (compile-module mp)
  ;; xxx we may need to specify the rel-to-path-v argument for anything that this module might depend upon
  ;;(compile-file (resolve-module-path mp #f))
  (define sexp-lst 
    (dynamic-require `(submod ,mp ast) 'src-sexp-lst))
  (define stx (to-module-syntax sexp-lst))
  (pretty-println (syntax->datum stx))
  (set! stx (my-expand-macros/syntax stx))
  (pretty-println (syntax->datum stx))
  )

(compile-module "test-1.rkt")




#|

(define (read-file pn)
  (call-with-input-file pn
    (lambda (in)
      (read-string (file-size pn) in))))

(define* (compile-file pn)
  (displayln (read-file pn))
  (define stx-lst (load-as-syntaxes pn))
  (define in-stx
    (strip-context
     ;; Initial bindings for 'module' body forms come from the
     ;; language specified here. The binding for the 'module' form
     ;; itself must come from elsewhere.
     #`(module main "runtime-compiler.rkt"
         #,@stx-lst)))
  ;;(print-stx-with-bindings in-stx)
  (let ((this-ns (current-namespace))
        (ns (make-empty-namespace)))
    ;; Cannot do this unless import into this-ns (possibly renamed).
    ;;(namespace-attach-module this-ns '"runtime-compiler.rkt" ns)
    (parameterize ((current-namespace ns))
      (namespace-attach-module this-ns 'racket/base)
      (namespace-require '(only racket/base module))
      ;;(namespace-require '"runtime-compiler.rkt")
      (let ((in-stx (namespace-syntax-introduce in-stx)))
        (let ((core-stx (expand-syntax in-stx)))
          ;;(pretty-println (syntax->datum core-stx))
          ;;(print-stx-with-bindings core-stx)
          (let ((core-ast (parse core-stx)))
            (pretty-println core-ast)
            (display (to-cxx-text core-ast))
            ))))))

|#
