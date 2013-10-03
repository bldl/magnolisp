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

(require "annos-parse.rkt" "compiler-util.rkt" "util.rkt"
         syntax/id-table syntax/moddep)

;;; 
;;; parameters
;;; 

;; Any module paths are resolved relative to this path. May be #f, in
;; which case the current directory is used.
(define* mp-root-path (make-parameter #f))

;;; 
;;; utilities
;;; 

;; mp is (or/c module-path? resolved-module-path?).
(define (get-exports-and-imports mp)
  (define path
    (if (resolved-module-path? mp)
        (resolved-module-path-name mp)
        (resolve-module-path mp (mp-root-path))))
  (define c-exp (get-module-code path))
  (let-values (((vals stxs) (module-compiled-exports c-exp)))
    (let ((imports (module-compiled-imports c-exp)))
      (values vals stxs imports))))

;;; 
;;; other
;;; 

;; [pt syntax?] is the parse tree, as loaded from the submodule.
;; [annos bound-id-table?] are the annotations. A non-Magnolisp module
;; is simply represented by the value #t, since such modules are not
;; processed.
(struct Mod (pt annos) #:transparent)

;; (-> module-path? (or/c Mod? #t)) Loads the specified module. It is
;; an error if the module path does not specify an existing module.
(define (load-module mp)
  (define annos
    (dynamic-require `(submod ,mp magnolisp-info) 'm-annos (thunk #f)))
  (if (not annos)
      #t
      (let ((pt (dynamic-require `(submod ,mp magnolisp-info) 'm-ast)))
        (Mod pt annos))))  

(define (set-entry-points! eps annos)
  (bound-id-table-for-each
   annos
   (lambda (id h)
     (void)))) ;; xxx need parsing support for entry point annotation

;; Compilation state. 'mods' maps resolved module paths to Mod or #t
;; objects. 'eps' is a bound-id-table?, with entry points as keys, and
;; #t values.
(struct St (mods eps) #:transparent)

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof.
(define* (compile-modules . mp-lst)
  (define mods (make-hash))
  (define eps (make-bound-id-table #:phase 0))

  ;; Load all the modules.
  (for ((mp mp-lst))
    (define r-mp (resolve-module-path mp (mp-root-path)))
    (define mod (hash-ref mods r-mp #f))
    (unless mod
      (set! mod (load-module mp))
      (hash-set! mods r-mp mod)

      ;; Use annotations to build a set of entry points.
      (when (Mod? mod)
        (define annos (Mod-annos mod))
        (set-entry-points! eps annos))))

  (St mods eps))

;; Compiles the modules defined in the specified files. Returns a
;; compilation state with a full IR for the entire program.
(define* (compile-files . fn-lst)
  (define mp-lst
    (map
     (lambda (fn)
       (define s (if (path? fn) (path->string fn) fn))
       (define mp `(file ,s))
       mp)
     fn-lst))
  (apply compile-modules mp-lst))

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
  (define st (compile-modules "test-6-prog.rkt")))
