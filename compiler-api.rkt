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

(require "annos-parse.rkt" "ast-magnolisp.rkt" "compiler-util.rkt"
         "parse.rkt" "strategy.rkt" "util.rkt"
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
;; [annos bound-id-table?] are the annotations, as loaded from the
;; submodule. A non-Magnolisp module is simply represented by the
;; value #t, since such modules are not processed. [defs (or/c
;; bound-id-table? #f)] contains Def objects for parsed modules.
(struct Mod (pt annos defs provs reqs) #:transparent)

;; (-> module-path? (or/c Mod? #t)) Loads the specified module. It is
;; an error if the module path does not specify an existing module.
(define (load-module mp)
  (define annos
    (dynamic-require `(submod ,mp magnolisp-info) 'm-annos (thunk #f)))
  (if (not annos)
      #t
      (let ((pt (dynamic-require `(submod ,mp magnolisp-info) 'm-ast)))
        (Mod pt annos #f #f #f))))

(define (list-entry-points annos)
  (define lst null)
  (bound-id-table-for-each
   annos
   (lambda (id h)
     (define ep (parse-entry-point id h))
     (when ep
       (set! lst (cons id lst)))))
  lst)

(define (bound-id-table-merge! t s-t)
  (bound-id-table-for-each
   s-t
   (lambda (id v)
     (bound-id-table-set! t id v))))

(define (bound-id-table-add-lst! t lst)
  (for ((id lst))
    (bound-id-table-set! t id #t)))

;; Compilation state. 'mods' maps resolved module paths to Mod or #t
;; objects. 'eps' is a bound-id-table?, with entry points as keys, and
;; #t values.
(struct St (mods eps) #:transparent)

;; For debugging.
(define (mods-display-Var-bindings mods)
  (for (((r-mp mod) mods))
    (define defs (Mod-defs mod))
    (bound-id-table-for-each
     defs
     (lambda (id def)
       ((topdown-visit
         (lambda (ast)
           (when (Var? ast)
             (define var-id (Var-id ast))
             (writeln (list ast (identifier-binding var-id))))))
        def)))))

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof.
(define* (compile-modules . ep-mp-lst)
  (define mods (make-hash)) ;; r-mp -> Mod
  (define eps (make-bound-id-table #:phase 0))
  (define dep-q null) ;; deps queued for loading

  (define (load mp ep?)
    (define r-mp (resolve-module-path mp (mp-root-path)))
    (define mod (hash-ref mods r-mp #f))
    (unless mod ;; not yet loaded
      (set! mod (load-module mp))

      (when (Mod? mod) ;; is a Magnolisp module
        (define annos (Mod-annos mod))

        ;; For entry point modules, use annotations to build a set of
        ;; entry points. Add these to program entry points.
        (define eps-lst null)
        (when ep?
          (set! eps-lst (list-entry-points annos))
          (bound-id-table-add-lst! eps eps-lst))

        ;; If a module has entry points, or if it is a dependency,
        ;; then collect further information from it.
        (when (or (not ep?) (and ep? (not (null? eps-lst))))
          (define pt (Mod-pt mod)) ;; parse tree
          (define-values (defs provs reqs)
            (parse-defs-from-module pt annos r-mp))
          (set! mod
                (struct-copy Mod mod
                             (defs defs) (provs provs) (reqs reqs)))
          (define raw-mp-lst
            (req-specs->module-paths reqs))
          ;;(pretty-print (bound-id-table-map defs cons))
          ;;(pretty-print (list 'provided-ids (dict-map provs list)))
          ;;(pretty-print (list 'raw-module-paths raw-mp-lst))
          (set! dep-q (append dep-q (map syntax->datum raw-mp-lst)))))

      (hash-set! mods r-mp mod)))

  ;; Load all the "entry" modules.
  (for ((mp ep-mp-lst))
    (load mp #t))

  ;; Keep loading dependencies until all loaded.
  (let loop ()
    (unless (null? dep-q)
      (define mp-lst dep-q)
      (set! dep-q null)
      (for ((mp mp-lst))
        (load mp #f))
      (loop)))

  ;;(mods-display-Var-bindings mods)
  ;;(pretty-print (bound-id-table-map eps (compose car cons)))

  (for (([k v] mods)) (pretty-print (list 'loaded k v)))
  
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
