#lang racket

#|

Import and name resolution.

|#

(require "ast-magnolisp.rkt" "parse.rkt" "strategy.rkt" "util.rkt"
         syntax/id-table syntax/modresolve)

;;; 
;;; utilities
;;; 

(define-syntax-rule (may-fail b ...)
  (with-handlers
      ((exn:fail? (lambda (e) #f)))
    b ...))

;;; 
;;; module paths
;;;

;; Confusingly, 'resolve-module-path' does not appear to (always)
;; return a 'resolved-module-path?'. This predicate may be used
;; instead. It reflects the contract for the return value of
;; 'resolve-module-path'.
(define* (resolve-module-path-result? x)
  (matches? x
   (? path?)
   (? symbol?)
   (list 'submod (or (? path?) (? symbol?)) (? symbol?) ..1)))

;; Turns resolve-module-path result format into
;; make-resolved-module-path result format.
(define-with-contract*
  (-> resolve-module-path-result? resolved-module-path?)
  (r-mp->rr-mp r-mp)
  (define (f path)
    (simplify-path (cleanse-path path)))
  (make-resolved-module-path
   (match r-mp
     ((? path?) (f r-mp))
     ((? symbol?) r-mp)
     ((list 'submod
            (and (or (? path?) (? symbol?)) p)
            (? symbol? subs) ..1)
      (cons (if (path? p) (f p) p) subs)))))
      	
;;; 
;;; module
;;; 

;; [r-mp resolve-module-path-result?] is the paths as used for loading
;; the module. [pt syntax?] is the parse tree, as loaded from the
;; submodule. [annos id-table?] are the annotations, as loaded from
;; the submodule. A non-Magnolisp module simply gets null values for
;; the above, since it contains no Magnolisp syntax. [defs id-table?]
;; contains Def objects for parsed modules. [provs id-table?] maps
;; each internally bound ID to a list of exported IDs. [reqs (listof
;; syntax?)] is a list of #%require specs. [syms (hash/c symbol?
;; Def?)] maps top-level names to definitions.
(concrete-struct* Mod (r-mp pt annos defs provs reqs syms) #:transparent)

;;; 
;;; loading
;;; 

;; (-> module-path? Mod?) Loads the specified module. It is an error
;; if the module path does not specify an existing module. Only sets
;; 'pt' and 'annos' fields.
(define-with-contract*
  (-> resolve-module-path-result? module-path? Mod?)
  (load-mod-from-submod r-mp mp)
  ;; Visit the module to determine if it even exists, and is a valid
  ;; module. This must succeed.
  (dynamic-require r-mp (void)
                   (thunk
                    (error 'load-mod-from-submod
                           "no such module: ~s (~a)" mp r-mp)))
  (define (make-sub-mp mp name)
    (match mp
      ((list 'submod outer sub ..1)
       `(submod ,outer ,@sub ,name))
      (_
       `(submod ,mp ,name))))
  (define sub-mp (make-sub-mp r-mp 'magnolisp-info))
  (define annos
    (may-fail
     (dynamic-require sub-mp 'm-annos (thunk #f))))
  (when annos
    (define original
      (dynamic-require sub-mp 'm-id-count))
    (define current (dict-count annos))
    (unless (= original current)
      (error 'load-mod-from-submod
             "count mismatch (~a): ~a != ~a" mp original current)))
  (define pt
    (if annos
        (dynamic-require sub-mp 'm-ast)
        #'(#%module-begin)))
  (Mod r-mp
       pt
       (or annos (make-immutable-free-id-table #:phase 0))
       (make-immutable-free-id-table #:phase 0)
       (make-immutable-free-id-table #:phase 0)
       null
       #hasheq()))

;;; 
;;; import resolution
;;; 

;; Returns (and/c hash? hash-eq? immutable?), mapping each top-level
;; symbol in the module [Mod? mod] to the corresponding definition
;; Def?.
(define-with-contract
  (-> Mod? (hash/c symbol? Def?))
  (build-sym-def-for-mod mod)
  (define defs (Mod-defs mod))
  (define sym-def #hasheq())
  (dict-for-each
   defs
   (lambda (id def)
     (when (ast-anno-maybe def 'top)
       (set! sym-def (hash-set sym-def (syntax-e id) def)))))
  sym-def)

(define-with-contract
  (-> (hash/c resolved-module-path? Mod?)
      (hash/c resolved-module-path? (hash/c symbol? Def?)))
  (build-sym-def-for-mods mods)
  (define h (make-hasheq))
  (for (([rr-mp mod] mods))
    (define sym-def (build-sym-def-for-mod mod))
    (hash-set! h rr-mp sym-def))
  h)

;; Constructs a map from exported to internal symbols.
(define-with-contract
  (-> Mod? (hash/c symbol? symbol?))
  (build-sym-prov-for-mod mod)
  (define provs (Mod-provs mod)) ;; internal ID -> listof(exported ID)
  (define sym-prov (make-hasheq))
  (dict-for-each
   provs
   (lambda (i-id e-id-lst)
     (define i-sym (syntax-e i-id))
     (for ((id e-id-lst))
       (hash-set! sym-prov (syntax-e id) i-sym))))
  sym-prov)

;; Constructs a map from exported to internal symbols, for each
;; module.
(define-with-contract
  (-> (hash/c resolved-module-path? Mod?)
      (hash/c resolved-module-path? (hash/c symbol? symbol?)))
  (build-sym-prov-for-mods mods)
  (define h (make-hasheq))
  (for (([rr-mp mod] mods))
    (define v (build-sym-prov-for-mod mod))
    (hash-set! h rr-mp v))
  h)

;; sym-prov-for-mods is a map from exported to internal symbols, for
;; each module. Returns a map from local symbols to imports, for each
;; module. Import info includes information about exporting module and
;; the exported symbol.
(define-with-contract
  (-> (hash/c resolved-module-path? (hash/c symbol? symbol?))
      (hash/c resolved-module-path? Mod?)
      (hash/c resolved-module-path?
              (hash/c symbol? (cons/c resolved-module-path? symbol?))))
  (build-l-to-mp-i-for-mods sym-prov-for-mods mods)
  (define l-to-mp-i-for-mods (make-hasheq))
  (for (((rel-rr-mp mod) mods))
    (define rel-r-mp (Mod-r-mp mod))
    (define req-lst (Mod-reqs mod)) ;; listof(require specs)
    ;;(pretty-print (list 'req-specs rel-rr-mp req-lst))
    (define reqs-per-mp
      (req-specs->reqs-per-mp req-lst))
    ;;(pretty-print (list 'reqs-per-mp rel-rr-mp reqs-per-mp))
    (define l-to-mp-i (make-hasheq)) ;; local ID -> import info
    (for (([mp reqs] reqs-per-mp))
      (define r-mp (resolve-module-path mp rel-r-mp))
      (define rr-mp (r-mp->rr-mp r-mp))
      (define mp-syms ;; symbols exported from mp
        (hash-keys (hash-ref sym-prov-for-mods rr-mp)))
      (for ((req reqs))
        (cond
         ((eq? req 'all)
          (for ((sym mp-syms))
            (hash-set! l-to-mp-i sym (cons rr-mp sym))))
         ((list? req)
          (for ((id req))
            (define sym (syntax-e id))
            (hash-set! l-to-mp-i sym (cons rr-mp sym))))
         ((pair? req)
          (hash-set! l-to-mp-i (syntax-e (car req))
                     (cons rr-mp (syntax-e (cdr req)))))
         (else (assert #f)))))
    (hash-set! l-to-mp-i-for-mods rel-rr-mp l-to-mp-i))
  l-to-mp-i-for-mods)

;; 'mods' is a (hash/c resolved-module-path? Mod?) of all the
;; modules under consideration. From them we extract all the 'reqs',
;; and resolve, for each module, (hash/c symbol? Def?), where the
;; definitions may come from other modules. The symbol in the Def may
;; be different. Each Def has an 'rr-mp' annotation, which may be used
;; to identify the originating module of the definition. This routine
;; also supports re-exports. We collect this information for all
;; top-level Magnolisp bindings that are visible in each module,
;; including syntax bindings. We proceed in topological order. Racket
;; disallows cyclic requires, so we need not worry about them.
(define-with-contract*
  (-> (and/c hash? hash-eq?) (and/c hash? hash-eq?))
  (mods-fill-in-syms mods)
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
  (define syms-for-mods #hasheq())
  
  ;; Computes symbols for the specified module.
  (define (build-syms-for-mod rr-mp)
    (define mod (hash-ref mods rr-mp))
    ;; Initialize with local Defs.
    (define l-to-def (hash-ref sym-def-for-mods rr-mp))
    ;;(pretty-print (list 'initial 'syms-for-mod rr-mp l-to-def))
    ;; Get all required symbols.
    (define l-to-mp-i (hash-ref l-to-mp-i-for-mods rr-mp))
    ;; Resolve Def for any symbol that has no local, overriding Def.
    (for (((l-sym mp-i) l-to-mp-i))
      (unless (hash-has-key? l-to-def l-sym)
        (define-values-from-cons (req-rr-mp i-sym) mp-i)
        (define i-syms (get-syms-for-mod! req-rr-mp))
        (define i-def (hash-ref i-syms i-sym))
        (set! l-to-def (hash-set l-to-def l-sym i-def))))
    ;;(pretty-print (list 'syms-for-mod r-mp l-to-def))
    l-to-def)

  ;; Sets 'syms-for-mods' for the specified module, and possibly also
  ;; for its dependencies.
  (define (get-syms-for-mod! rr-mp)
    (define syms (hash-ref syms-for-mods rr-mp #f))
    (unless syms ;; not yet computed
      (set! syms (build-syms-for-mod rr-mp))
      (set! syms-for-mods (hash-set syms-for-mods rr-mp syms)))
    syms)

  (for ((rr-mp (hash-keys mods)))
    (get-syms-for-mod! rr-mp))

  (for/hasheq (([rr-mp mod] mods))
    (define syms (hash-ref syms-for-mods rr-mp))
    (values rr-mp (struct-copy Mod mod (syms syms)))))

;;; 
;;; name resolution
;;; 

(define-with-contract
  (-> syntax? (or/c resolved-module-path? #f))
  (syntax-source-resolve-module stx)
  (define src (syntax-source-module stx #f))
  (define res
    (cond
     ((not src) #f)
     ((resolved-module-path? src) src)
     ((symbol? src) (make-resolved-module-path src))
     ((path? src)
      (unless (complete-path? src)
        (error
         'syntax-source-resolve-module
         "syntax-source-module ~s for ~s: assumed complete-path?"
         src stx))
      (make-resolved-module-path src))
     ((module-path-index? src)
      ;;(writeln `(split mpi for ,(syntax-e stx) : ,@(values-of (module-path-index-split src))))
      (module-path-index-resolve src))
     (else
      (error
       'syntax-source-resolve-module
       "syntax-source-module ~s for ~s: unexpected value type"
       src stx))))
  ;;(writeln `(syntax-source-resolve-module for ,(syntax-e stx) loc ,src resolved ,res : ,stx))
  res)

;; Takes a free-id-table and module information, and returns an
;; immutable-free-id-table with resolved Var and NameT nodes. I.e.,
;; sets 'def-id information into the nodes. This only affects name
;; references that resolve to definitions contained in the specified
;; modules.
(define-with-contract*
  (-> free-id-table? (and/c hash? hash-eq?) immutable-free-id-table?)
  (defs-resolve-names defs mods)
  ;;(pretty-print mods) (exit)
  
  (define (rw-def def)
    ;;(writeln `(resolving ,(Def-id def) in ,(ast-anno-must def 'rr-mp)))
    
    (define (get-mod)
      (define rr-mp (ast-anno-must def 'rr-mp))
      (values rr-mp (hash-ref mods rr-mp)))

    (define (get-mod-for-id stx)
      (define rr-mp (syntax-source-resolve-module stx))
      ;;(writeln `(mod for id ,(syntax-e stx) is ,rr-mp))
      (if rr-mp
          (values rr-mp (hash-ref mods rr-mp #f))
          (get-mod)))

    (define (rw-ref ast)
      (define id (name-ref-id ast))
      ;; If 'lexical, we can look up by ID from containing Mod. If #f,
      ;; we should be able to look up by name from source Mod.
      ;; Otherwise there is a module binding, and we can say look it
      ;; up by ID. If the lookup fails for a 'lexical or unbound ID,
      ;; it is an error. If it fails for a module binding, then the ID
      ;; should identify a known builtin that the compiler supports
      ;; (that will be checked later).
      (define b (identifier-binding id))
      ;;(writeln `(rw-ref ,ast ,b))
      (define def-id #f)
      (cond
       ((not b)
        (define-values (rr-mp mod) (get-mod-for-id id))
        ;;(writeln `(get-mod-for-id ,rr-mp ,mod for ,id))
        (unless mod
          (error 'defs-resolve-names
                 "cannot determine module for unbound reference: ~s" id))
        (define syms (Mod-syms mod))
        (define def (hash-ref syms (syntax-e id) #f))
        (unless def
          (error 'defs-resolve-names
                 "no definition in ~a for unbound reference: ~s" rr-mp id))
        (set! def-id (Def-id def)))
       ((eq? b 'lexical)
        (define def (dict-ref defs id #f))
        (unless def
          (error 'defs-resolve-names
                 "undefined lexical name reference: ~s" id))
        (set! def-id (Def-id def))
        (assert (free-identifier=? id def-id)))
       ((list? b)
        ;;(writeln `(is a list for ,id))
        (define mpi (first b))
        ;;(writeln `(split mpi for ,(syntax-e id) is ,(values-of (module-path-index-split mpi))))
        ;;(writeln (module-path-index-resolve mpi))
        (define rr-mp (module-path-index-resolve mpi))
        ;;(writeln `(rr-mp ,rr-mp))
        (define mod (hash-ref mods rr-mp #f))
        (when mod
          (define sym (second b))
          (define syms (Mod-syms mod))
          (define def (hash-ref syms sym #f))
          (unless def
            (error 'defs-resolve-names
                   "no definition in ~a for module-level name ~a: ~s"
                   rr-mp sym id))
          (set! def-id (Def-id def))
          (assert (free-identifier=? id def-id))))
       (else
        (error 'defs-resolve-names
               "unexpected identifier-binding: ~s" b)))
      ;;(writeln (list 'resolved-var ast 'reference id 'binding b 'module (syntax-source-module id) 'bound-to def-id))
      (if def-id
        (set-def-id ast def-id)
        ast))
  
    (define rw
      (topdown
       (lambda (ast)
         (if (name-ref? ast)
             (rw-ref ast)
             ast))))

    (rw def))
  
  (for/dict
   (make-immutable-free-id-table #:phase 0)
   (([id def] (in-dict defs)))
   (values id (rw-def def))))
