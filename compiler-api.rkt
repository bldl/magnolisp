#lang racket

#|

Implements a compiler for Magnolisp. Loads the code to be compiled
from Racket module metadata, included as submodules by the Racket
'magnolisp' language implementation.

The compiler ignores module top-level expressions.

The compiler requires a fully typed program (although not all types
have to be written out explicitly -- think 'auto' in C++).

Compiles only the 'entry-point' operations of the specified modules,
and their dependencies. This essentially means full program/library
optimization.

Generates a single .cpp, .hpp, and .mk output file. The header file
only declares the entry-point operations, and the abstract types they
depend upon. The implementation file has an non-entry-points declared
as internal (e.g., static). The GNU Make include file lists the
external dependencies for the program/library, as well as the .cpp and
.hpp files.

|#

(require "compiler-util.rkt" "util.rkt"
         syntax/id-table syntax/moddep)

;;; 
;;; parameters
;;; 

;; Any module paths are resolved relative to this path. May be #f, in
;; which case the current directory is used.
(define* mp-root-path (make-parameter #f))

;;; 
;;; other
;;; 

;; Compilation state.
(struct St () #:transparent)

;; Returns an empty compilation state.
(define* (new-state)
  (St))

(define (get-exports-and-imports mp)
  (define path
    (if (resolved-module-path? mp)
        (resolved-module-path-name mp)
        (resolve-module-path mp (mp-root-path))))
  (define c-exp (get-module-code path))
  (let-values (((vals stxs) (module-compiled-exports c-exp)))
    (let ((imports (module-compiled-imports c-exp)))
      (values vals stxs imports))))

;; Updates compilation state with all the entry points in the
;; specified module, and all dependencies thereof. Returns an updated
;; compilation state.
(define* (compile-module st mp)
  (define-values [vals stxs imports]
    ;; The information we get with this is not very helpful in
    ;; resolving globals. We know what is imported, and what names
    ;; said modules export, but require renames and such are
    ;; presumably not accounted for. Said information may be better
    ;; visible in the AST itself.
    (get-exports-and-imports mp))
  (define m-annos
    (dynamic-require `(submod ,mp magnolisp-info) 'm-annos))
  (define m-ast
    (dynamic-require `(submod ,mp magnolisp-info) 'm-ast))
  (define (resolve is)
    (map
     (lambda (p)
       (define phase (car p))
       (define mpi-lst (cdr p))
       (map
        (lambda (mpi)
          (define r-mp (module-path-index-resolve mpi))
          (list phase r-mp (apply-values list
                            (get-exports-and-imports r-mp))))
        mpi-lst))
     is))
  (pretty-print
   (list vals stxs
         ;;(resolve imports)
         m-ast))
  st)

;; Compiles the module defined in the specified file. Returns an
;; updated compilation state.
(define* (compile-file st fn)
  (define s (if (path? fn) (path->string fn) fn))
  (define mp `(file ,s))
  (compile-module st mp))

;;; 
;;; code generation
;;; 

(define* (write-cpp-file st cpp-file)
  (void)) ;;xxx

(define* (write-hpp-file st hpp-file)
  (void)) ;;xxx

(define* (write-mk-file st mk-file)
  (void)) ;;xxx

;;; 
;;; testing
;;; 

(module* main #f
  (define st (new-state))
  (set! st (compile-module st "test-6-prog.rkt"))
  )
