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

(define-syntax-rule (may-fail b ...)
  (with-handlers
      ((exn:fail? (lambda (e) #f)))
    b ...))

;; (-> module-path? (or/c Mod? #t)) Loads the specified module. It is
;; an error if the module path does not specify an existing module.
;; Only sets 'pt' and 'annos' fields.
(define (load-mod-from-submod mp)
  (define annos
    (may-fail
     (dynamic-require `(submod ,mp magnolisp-info) 'm-annos (thunk #f))))
  (define pt
    (if annos
        (dynamic-require `(submod ,mp magnolisp-info) 'm-ast)
        #'(#%module-begin)))
  (unless annos
    (set! annos (make-immutable-bound-id-table #:phase 0)))
  (Mod pt annos #f #f #f #f))

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

;; Compilation state. [mods hash?] maps resolved module paths to Mod
;; objects. [eps bound-id-table?] has entry points as keys, and #t
;; values.
(struct St (mods defs eps) #:transparent)

(define (merge-defs mods)
  (define all-defs (make-free-id-table #:phase 0))
  (for (([r-mp mod] mods))
    (define defs (Mod-defs mod))
    (dict-for-each
     defs
     (lambda (id def)
       (when (dict-has-key? all-defs id)
         (error 'merge-defs
                "ID ~s bound (for runtime) in more than one module" id))
       (dict-set! all-defs id def))))
  all-defs)

(define (def-display-Var-bindings def)
  ((topdown-visit
    (lambda (ast)
      (when (Var? ast)
        (define var-id (Var-id ast))
        (define def-id (Ast-anno-ref ast 'def-id #:must #f))
        (writeln (list ast (identifier-binding var-id) def-id)))))
   def)) 

;; For debugging.
(define (mods-display-Var-bindings mods)
  (for (((r-mp mod) mods))
    (writeln `(Vars ,r-mp))
    (define defs (Mod-defs mod))
    (bound-id-table-for-each
     defs
     (lambda (id def)
       (def-display-Var-bindings def)))))

;; For debugging.
(define (all-defs-display-Var-bindings all-defs)
  (free-id-table-for-each
   all-defs
   (lambda (id def)
     (def-display-Var-bindings def))))

(define (syntax-source-resolve-module stx)
  (define src (syntax-source-module stx #f))
  (cond
   ((not src) #f)
   ((resolved-module-path? src) (resolved-module-path-name src))
   ((symbol? src) src)
   ((path? src) src)
   ((module-path-index? src)
    (resolved-module-path-name (module-path-index-resolve src)))
   (else
    (error 'syntax-source-resolve-module
           "unexpected syntax-source-module ~s for ~s" src stx))))

(define (defs-resolve-Vars defs mods)
  ;;(pretty-print mods)
  
  (define (rw-def def)
    (define (get-mod)
      (define r-mp (Ast-anno-ref def 'r-mp))
      (values r-mp (hash-ref mods r-mp)))

    (define (get-mod-for-id stx)
      (define r-mp (syntax-source-resolve-module stx))
      ;;(writeln r-mp)
      (if r-mp
          (values r-mp (hash-ref mods r-mp #f))
          (get-mod)))

    (define (rw-var ast)
      (define id (Var-id ast))
      ;; If 'lexical, we can look up by ID from containing Mod. If #f,
      ;; we should be able to look up by name from source Mod.
      ;; Otherwise there is a module binding, and we can say look it
      ;; up by ID. If the lookup fails for a 'lexical or unbound ID,
      ;; it is an error. If it fails for a module binding, then the ID
      ;; should identify a known builtin that the compiler supports
      ;; (that will be checked later).
      (define b (identifier-binding id))
      (define def-id #f)
      (cond
       ((not b)
        (define-values [mp mod] (get-mod-for-id id))
        (unless mod
          (error 'defs-resolve-Vars
                 "cannot determine module for unbound variable: ~s" id))
        (define syms (Mod-syms mod))
        (define def (hash-ref syms (syntax-e id) #f))
        (unless def
          (error 'defs-resolve-Vars
                 "no definition in ~a for unbound variable: ~s" mp id))
        (set! def-id (Def-id def)))
       ((eq? b 'lexical)
        (define def (dict-ref defs id #f))
        (unless def
          (error 'defs-resolve-Vars
                 "undefined lexical variable reference: ~s" id))
        (set! def-id (Def-id def))
        (assert (free-identifier=? id def-id)))
       ((list? b)
        (define mpi (first b))
        (define r-mp 
          (resolved-module-path-name
           (module-path-index-resolve mpi)))
        (define mod (hash-ref mods r-mp #f))
        (when mod
          (define sym (second b))
          (define syms (Mod-syms mod))
          (define def (hash-ref syms sym #f))
          (unless def
            (error 'defs-resolve-Vars
                   "no definition in ~a for module-level variable ~a: ~s"
                   r-mp sym id))
          (set! def-id (Def-id def))
          (assert (free-identifier=? id def-id))))
       (else
        (error 'defs-resolve-Vars
               "unexpected identifier-binding: ~s" b)))
      ;;(writeln (list 'resolved-var ast 'reference id 'binding b 'module (syntax-source-module id) 'bound-to def-id))
      (when def-id
        (set! ast (Ast-anno-set ast 'def-id def-id))
        ;;(writeln `(def-id ,(Ast-anno-ref ast 'def-id)))
        )
      ast)
  
    (define rw
      (topdown
       (lambda (ast)
         (if (Var? ast) (rw-var ast) ast))))

    (rw def))
  
  (for/dict
   (make-immutable-free-id-table #:phase 0)
   (([id def] (in-dict defs)))
   (values id (rw-def def))))

;; Returns (and/c hash? hash-eq? immutable?).
(define (build-sym-def-for-mod mod)
  (define defs (Mod-defs mod))
  (define sym-def #hasheq())
  (dict-for-each
   defs
   (lambda (id def)
     (when (Ast-anno-ref def 'top #:must #f)
       (set! sym-def (hash-set sym-def (syntax-e id) def)))))
  sym-def)

(define (build-sym-def-for-mods mods)
  (define h (make-hash))
  (for (([r-mp mod] mods))
    (define sym-def (build-sym-def-for-mod mod))
    (hash-set! h r-mp sym-def))
  h)

(define (build-sym-prov-for-mod mod)
  (define provs (Mod-provs mod))
  (define sym-prov (make-hasheq))
  (dict-for-each
   provs
   (lambda (i-id e-id-lst)
     (define i-sym (syntax-e i-id))
     (for ((id e-id-lst))
       (hash-set! sym-prov (syntax-e id) i-sym))))
  sym-prov)

(define (build-sym-prov-for-mods mods)
  (define h (make-hash))
  (for (([r-mp mod] mods))
    (define v (build-sym-prov-for-mod mod))
    (hash-set! h r-mp v))
  h)

(define (build-l-to-mp-i-for-mods sym-prov-for-mods mods)
  (define l-to-mp-i-for-mods (make-hash))
  (for (([r-mp mod] mods))
    (define req-lst (Mod-reqs mod))
    ;;(pretty-print (list 'req-specs r-mp req-lst))
    (define reqs-per-mp
      (req-specs->reqs-per-mp req-lst))
    ;;(pretty-print (list 'reqs-per-mp r-mp reqs-per-mp))
    (define l-to-mp-i (make-hasheq))
    (for (([mp reqs] reqs-per-mp))
      (define r-mp (resolve-module-path mp (mp-root-path)))
      (define mp-syms (hash-keys (hash-ref sym-prov-for-mods r-mp)))
      (for ((req reqs))
        (cond
         ((eq? req 'all)
          (for ((sym mp-syms))
            (hash-set! l-to-mp-i sym (cons r-mp sym))))
         ((list? req)
          (for ((id req))
            (define sym (syntax-e id))
            (hash-set! l-to-mp-i sym (cons r-mp sym))))
         ((pair? req)
          (hash-set! l-to-mp-i (syntax-e (car req))
                     (cons r-mp
                           (syntax-e (cdr req)))))
         (else (assert #f)))))
    (hash-set! l-to-mp-i-for-mods r-mp l-to-mp-i))
  l-to-mp-i-for-mods)

;; 'mods' is a (hash/c resolve-module-path-result? Mod?) of all the
;; modules under consideration. From them we extract all the 'reqs',
;; and resolve, for each module, (hash/c symbol? Def?), where the
;; definitions may come from other modules. The symbol in the Def may
;; be different. Each Def has an 'r-mp' annotation, which may be used
;; to identify the originating module of the definition. This routine
;; also supports re-exports. We collect this information for all
;; top-level Magnolisp bindings that are visible in each module,
;; including syntax bindings. We proceed in topological order. Racket
;; disallows cyclic requires, and we need not worry about them.
(define (mods-fill-in-syms mods)
  ;; For each module, maps each local symbol to a local Def.
  (define sym-def-for-mods (build-sym-def-for-mods mods))
  ;;(pretty-print (list 'sym-def-for-mods sym-def-for-mods))

  ;; For each module, maps each exported symbol to a locally used
  ;; symbol.
  (define sym-prov-for-mods (build-sym-prov-for-mods mods))
  ;;(pretty-print (list 'sym-prov-for-mods sym-prov-for-mods))

  ;; For each module, maps each local, imported symbol to a module and
  ;; one of its exported symbols.
  (define l-to-mp-i-for-mods (build-l-to-mp-i-for-mods
                              sym-prov-for-mods mods))
  ;;(pretty-print (list 'l-to-mp-i-for-mods l-to-mp-i-for-mods))

  ;; For each module, maps each top-level name to its Def, which may
  ;; be in a different module.
  (define syms-for-mods #hash())
  
  ;; Computes symbols for the specified module.
  (define (build-syms-for-mod r-mp)
    (define mod (hash-ref mods r-mp))
    ;; Initialize with local Defs.
    (define l-to-def (hash-ref sym-def-for-mods r-mp))
    ;;(pretty-print (list 'initial 'syms-for-mod r-mp l-to-def))
    ;; Get all required symbols.
    (define l-to-mp-i (hash-ref l-to-mp-i-for-mods r-mp))
    ;; Resolve Def for any symbol that has no local, overriding Def.
    (for (([l-sym mp-i] l-to-mp-i))
      (unless (hash-has-key? l-to-def l-sym)
        (define req-r-mp (car mp-i))
        (define i-sym (cdr mp-i))
        (define i-syms (get-syms-for-mod! req-r-mp))
        (define i-def (hash-ref i-syms i-sym))
        (set! l-to-def (hash-set l-to-def l-sym i-def))))
    ;;(pretty-print (list 'syms-for-mod r-mp l-to-def))
    l-to-def)

  ;; Sets 'syms-for-mods' for the specified module, and possibly also
  ;; for its dependencies.
  (define (get-syms-for-mod! r-mp)
    (define syms (hash-ref syms-for-mods r-mp #f))
    (unless syms ;; not yet computed
      (set! syms (build-syms-for-mod r-mp))
      (set! syms-for-mods (hash-set syms-for-mods r-mp syms)))
    syms)

  (for ((r-mp (hash-keys mods)))
    (get-syms-for-mod! r-mp))

  (for/hash (([r-mp mod] mods))
    (define syms (hash-ref syms-for-mods r-mp))
    (values r-mp (struct-copy Mod mod (syms syms)))))

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
      (set! mod (load-mod-from-submod mp))

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

  (set! mods (mods-fill-in-syms mods))

  (define all-defs (merge-defs mods))
  (set! all-defs (defs-resolve-Vars all-defs mods))

  (all-defs-display-Var-bindings all-defs)
  ;;(mods-display-Var-bindings mods)
  ;;(pretty-print (bound-id-table-map eps (compose car cons)))
  ;;(pretty-print (dict-map all-defs (lambda (x y) y)))
  ;;(for (([k v] mods)) (pretty-print (list 'loaded k v)))

  (St mods all-defs eps))

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
