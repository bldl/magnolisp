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
         "parse.rkt" "strategy.rkt" "util.rkt" "util/struct.rkt"
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

(define-syntax-rule (may-fail b ...)
  (with-handlers
      ((exn:fail? (lambda (e) #f)))
    b ...))

;;;
;;; de-Racketization
;;;

;; A combinator that applies the rewrite 'rw' to each definition in
;; the passed set of definitions.
(define (make-for-all-defs rw)
  (lambda (defs)
    (for/dict
     (make-immutable-free-id-table #:phase 0)
     (([id def] (in-dict defs)))
     (values id (rw def)))))

(define (de-racketize defs ast)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((Apply _ e _)
          (assert (Var? e))
          (define id (Var-id e))
          (define def (dict-ref defs id #f))
          ;; Base namespace names may be unresolved.
          (when def
            (unless (matches? def (DefVar _ _ _ (? Lambda?)))
              (raise-language-error/ast
               "application target does not name a function"
               ast e)))
          ast)

         ((DefVar a1 n t (Lambda a2 p b))
          ;; Retain annos from the binding, which has any information
          ;; associated with the binding. It should also have the
          ;; declaration syntax corresponding to the function
          ;; definition.
          (define-values (pt-lst rt)
            (match t
              ((FunT _ ps r)
               (unless (= (length ps) (length p))
                 (raise-language-error/ast
                  "arity mismatch between function and its type"
                  ast t))
               (values ps r))
              (_
               (raise-language-error/ast
                "expected function type"
                ast t))))
          (set! p (map
                   (lambda (p t)
                     (match p
                       ((Param a n _)
                        (Param a n t))))
                   p pt-lst))
          (when (hash-ref a1 'foreign #f)
            (set! b (annoless NoBody)))
          (Defun a1 n t p b))
         
         (_ ast)))))
  (rw ast))

(define (defs-de-racketize defs)
  ((make-for-all-defs (fix de-racketize defs)) defs))

;;; 
;;; simplification
;;; 

(define (compose1-> . fs)
  (apply compose1 (reverse fs)))

(define-syntax-rule
  (match-or v clause ...)
  (match v clause ... (_ v)))

(define ast-empty-Let->BlockStat
  (topdown
   (lambda (ast)
     (match ast
       ((Let a (list) ss)
        (BlockStat a ss))
       (_ ast)))))

(define ast-nested-BlockStat->BlockStat
  (topdown
   (lambda (ast)
     (match-or ast
       ((BlockStat a ss)
        (if (not (ormap BlockStat? ss))
            ast
            (BlockStat a (apply append (for/list ((s ss))
                                         (if (BlockStat? s)
                                             (BlockStat-ss s)
                                             (list s)))))))))))

(define-syntax-rule
  (topdown-match-or #:ast ast clause ...)
  (topdown
   (lambda (ast)
     (match-or ast
       clause ...))))

(define (ast-get-ss ast)
  (match ast
    ((BlockStat _ ss) ss)
    ((BlockExpr _ ss) ss)
    ((Let _ _ ss) ss)
    (_ #f)))

(define (ast-set-ss ast n-ss)
  (match ast
    ((BlockExpr a ss)
     (BlockExpr a n-ss))
    ((BlockStat a ss)
     (BlockStat a n-ss))
    ((Let a bs ss)
     (Let a bs n-ss))))

(define (list-rm-Pass ss)
  (apply append (for/list ((s ss))
                  (if (Pass? s)
                      null
                      (list s)))))

(define ast-rm-Pass
  (topdown
   (lambda (ast)
     (define ss (ast-get-ss ast))
     (cond
      ((and ss (ormap Pass? ss))
       (ast-set-ss ast (list-rm-Pass ss)))
      (else
       ast)))))

(define ast-simplify
  (compose1->
   ast-empty-Let->BlockStat
   ast-rm-Pass
   ast-nested-BlockStat->BlockStat))

(define defs-simplify
  (make-for-all-defs ast-simplify))

;;; 
;;; local escapes
;;; 

(define (ast-LetLocalEc->BlockExpr ast)
  (define enclosing-id #f)
  
  (define rw
    (topdown-match-or
     #:ast ast
     ((LetLocalEc a k ss)
      (set! enclosing-id k)
      (BlockExpr a ss))
     ((AppLocalEc a k e)
      (unless enclosing-id
        (raise-language-error/ast
         "local escape without enclosing context"
         ast))
      (unless (free-identifier=? enclosing-id k)
        (raise-language-error/ast
         "local escape beyond its context"
         ast k
         #:fields (list (list "context" enclosing-id))))
      (Return a e))))

  (rw ast))

;;; 
;;; type checking
;;; 

;; Takes an immutable-free-id-table containing just the program, and
;; checks its types. Expects a fully typed program. 'defs' itself is
;; used as the type environment. Returns nothing.
(define (defs-type-check defs)
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values defs))))
  
  (define (lookup x)
    (assert (Var? x))
    (define def-id (get-def-id-or-fail x))
    (define def (dict-ref defs def-id))
    (match def
      ((DefVar _ _ t _)
       t)
      ((Param _ _ t)
       t)
      ((Defun _ _ t ps _)
       t)
      ((ForeignTypeDecl _ id _)
       (syntaxed id DefNameT id))
      ))
  
  (define (type=? x y)
    (cond
     ((AnyT? x) #t)
     ((AnyT? y) #t)
     ((and (NameT? x) (NameT? y))
      (free-identifier=?
       (get-def-id-or-fail x)
       (get-def-id-or-fail y)))
     ((and (FunT? x) (FunT? y))
      (define x-rt (FunT-rt x))
      (define y-rt (FunT-rt y))
      (define x-ats (FunT-ats x))
      (define y-ats (FunT-ats y))
      (and (= (length x-ats) (length y-ats))
           (type=? x-rt y-rt)
           (andmap type=? x-ats y-ats)))
     (else #f)))

  (define (tc ast)
    ;;(writeln `(tc ,ast))
    (match ast
      ((or (? ForeignTypeDecl?) (? Param?))
       (void))
      ((Defun _ id t ps b)
       ;; Type kind and arity correctness wrt parameters was already
       ;; checked in creating Defun.
       (define r-t (FunT-rt t))
       (define b-t (if (NoBody? b) r-t (tc b)))
       (unless (type=? r-t b-t)
         (raise-language-error/ast
          "function return type does not match body expression"
          #:fields (list (list "declared return type"
                               (ast-displayable/datum r-t))
                         (list "actual return type"
                               (ast-displayable/datum b-t)))
          ast b))
       (void))
      ((? Var?)
       (lookup ast))
      ((Apply _ f as)
       (define f-t (lookup f))
       (unless (FunT? f-t)
         (raise-language-error/ast
          "application of a non-function"
          ast f))
       (unless (= (length as) (length (FunT-ats f-t)))
         (raise-language-error/ast
          "function arity does not match number of arguments"
          #:fields (list (list "function type" (ast-displayable f-t)))
          ast))
       (for ([e as] [p-t (FunT-ats f-t)])
         (define e-t (tc e))
         (unless (type=? p-t e-t)
           (raise-language-error/ast
            "parameter type does not match that of expression"
            #:fields (list
                      (list "parameter type" (ast-displayable p-t))
                      (list "function type" (ast-displayable f-t)))
            ast e)))
       (FunT-rt f-t))
      ((? Literal?)
       the-AnyT) ;; for now, at least
      (else
       (raise-argument-error
        'tc "supported Ast?" ast))))
  
  (for (((id def) (in-dict defs)))
    (tc def)))

;;; 
;;; build options
;;; 

(define (defs-collect-build-annos defs)
  (define lst null)

  (define (add! id-stx build-stx)
    (set! lst (cons (list id-stx build-stx) lst)))
  
  (for (((id def) (in-dict defs)))
    (assert (Def? def))
    (define b (ast-anno-maybe def 'build))
    (when b (add! id b)))
  
  lst)

;;; 
;;; module
;;; 

;; [pt syntax?] is the parse tree, as loaded from the submodule.
;; [annos id-table?] are the annotations, as loaded from the
;; submodule. A non-Magnolisp module simply gets null values for the
;; above, since it contains no Magnolisp syntax. [defs id-table?]
;; contains Def objects for parsed modules. [provs id-table?] maps
;; each internally bound ID to a list of exported IDs. [reqs (listof
;; syntax?)] is a list of #%require specs. [syms (hash/c symbol?
;; Def?)] maps top-level names to definitions.
(struct Mod (pt annos defs provs reqs syms) #:transparent)

;;; 
;;; import resolution
;;; 

;; (-> module-path? (or/c Mod? #t)) Loads the specified module. It is
;; an error if the module path does not specify an existing module.
;; Only sets 'pt' and 'annos' fields.
(define (load-mod-from-submod mp)
  (define annos
    (may-fail
     (dynamic-require `(submod ,mp magnolisp-info) 'm-annos (thunk #f))))
  (when annos
    (define original
      (dynamic-require `(submod ,mp magnolisp-info) 'm-id-count))
    (define current (dict-count annos))
    (unless (= original current)
      (error 'load-mod-from-submod
             "count mismatch (~a): ~a != ~a" mp original current)))
  (define pt
    (if annos
        (dynamic-require `(submod ,mp magnolisp-info) 'm-ast)
        #'(#%module-begin)))
  (Mod pt
       (or annos (make-immutable-free-id-table #:phase 0))
       (make-immutable-free-id-table #:phase 0)
       (make-immutable-free-id-table #:phase 0)
       null
       #hasheq()))

(define (collect-entry-points annos)
  (define eps (make-free-id-table #:phase 0))
  (dict-for-each
   annos
   (lambda (id h)
     (define ep (parse-export id h))
     (when ep
       (dict-set! eps id ep))))
  eps)

(define (id-table-add-lst! t lst)
  (for ((id lst))
    (dict-set! t id #t)))

;; Compilation state. [mods hash?] maps resolved module paths to Mod
;; objects. [eps id-table?] has entry points as keys, and #t values.
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
        (define def-id (get-def-id var-id))
        (assert (or (not def-id) (def-identifier=? var-id def-id)))
        (writeln (list (syntax-e var-id) ast (identifier-binding var-id) def-id)))))
   def))

;; Currently only supports references to variable names, not to type
;; names.
(define (Def-all-referred-def-ids def)
  (define defs (make-free-id-table #:phase 0))
  ((topdown-visit
    (lambda (ast)
      (when (name-ref? ast)
        (define def-id (get-def-id ast))
        (when def-id
          (dict-set! defs def-id #t)))))
   def)
  (dict-keys defs))

;; For debugging.
(define (mods-display-Var-bindings mods)
  (for (((r-mp mod) mods))
    (writeln `(Vars ,r-mp))
    (define defs (Mod-defs mod))
    (dict-for-each
     defs
     (lambda (id def)
       (def-display-Var-bindings def)))))

;; For debugging.
(define (all-defs-display-Var-bindings all-defs)
  (dict-for-each
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

;; Takes a free-id-table and module information, and returns an
;; immutable-free-id-table with resolve Var and NameT nodes. I.e.,
;; sets 'def-id information into the nodes. This only affects name
;; references that resolve to definitions contained in the specified
;; modules.
(define (defs-resolve-names defs mods)
  ;;(pretty-print mods)
  
  (define (rw-def def)
    (define (get-mod)
      (define r-mp (ast-anno-must def 'r-mp))
      (values r-mp (hash-ref mods r-mp)))

    (define (get-mod-for-id stx)
      (define r-mp (syntax-source-resolve-module stx))
      ;;(writeln r-mp)
      (if r-mp
          (values r-mp (hash-ref mods r-mp #f))
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
      (define def-id #f)
      (cond
       ((not b)
        (define-values [mp mod] (get-mod-for-id id))
        (unless mod
          (error 'defs-resolve-names
                 "cannot determine module for unbound reference: ~s" id))
        (define syms (Mod-syms mod))
        (define def (hash-ref syms (syntax-e id) #f))
        (unless def
          (error 'defs-resolve-names
                 "no definition in ~a for unbound reference: ~s" mp id))
        (set! def-id (Def-id def)))
       ((eq? b 'lexical)
        (define def (dict-ref defs id #f))
        (unless def
          (error 'defs-resolve-names
                 "undefined lexical name reference: ~s" id))
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
            (error 'defs-resolve-names
                   "no definition in ~a for module-level name ~a: ~s"
                   r-mp sym id))
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

;; Drops all bindings from 'all-defs' that are not reachable via at
;; least one of the entry points in 'eps'. This relies on name
;; references (within the codebase) having been resolved. Returns the
;; trimmed down definitions as a free-id-table.
(define (defs-drop-unreachable all-defs eps)
  (define processed-defs (make-free-id-table #:phase 0))
  (let loop ((ids-to-process (dict-keys eps)))
    (unless (null? ids-to-process)
      (define id (car ids-to-process))
      (set! ids-to-process (cdr ids-to-process))
      (unless (dict-has-key? processed-defs id)
        (define def (dict-ref all-defs id))
        (dict-set! processed-defs id def)
        (define refs-in-def (Def-all-referred-def-ids def))
        (set! ids-to-process
              (append ids-to-process refs-in-def)))
      (loop ids-to-process)))
  ;;(pretty-print `(original-defs ,(dict-count all-defs) ,(dict-keys all-defs) retained-defs ,(dict-count processed-defs) ,(dict-keys processed-defs)))
  processed-defs)

;; defs-in-mod is an immutable id-table, whereas eps-in-mod is an
;; id-table.
(define (defs-annotate-export-names defs-in-mod eps-in-mod)
  (for ([(id v) (in-dict eps-in-mod)])
    (assert v)
    (define def (dict-ref defs-in-mod id))
    (when (get-foreign-name def)
      (raise-language-error/ast
       "definition marked both as 'export' and 'foreign'"
       def))
    (define n-def (Ast-anno-set def 'export-name v))
    (set! defs-in-mod
          (dict-set defs-in-mod id n-def)))
  defs-in-mod)

;; Returns (and/c hash? hash-eq? immutable?).
(define (build-sym-def-for-mod mod)
  (define defs (Mod-defs mod))
  (define sym-def #hasheq())
  (dict-for-each
   defs
   (lambda (id def)
     (when (ast-anno-maybe def 'top)
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

;;; 
;;; compilation
;;;

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof.
(define* (compile-modules . ep-mp-lst)
  (define mods (make-hash)) ;; r-mp -> Mod
  (define eps-in-prog (make-free-id-table #:phase 0))
  (define dep-q null) ;; deps queued for loading

  (define (load mp ep?)
    (define r-mp (resolve-module-path mp (mp-root-path)))
    (define mod (hash-ref mods r-mp #f))
    (unless mod ;; not yet loaded
      (set! mod (load-mod-from-submod mp))

      (when (Mod? mod) ;; is a Magnolisp module
        (define annos (Mod-annos mod))
        ;;(pretty-print-id-table annos)

        ;; For entry point modules, use annotations to build a set of
        ;; entry points. Add these to program entry points.
        (define eps-in-mod #f)
        (when ep?
          (set! eps-in-mod (collect-entry-points annos))
          (id-table-add-lst! eps-in-prog (dict-keys eps-in-mod)))

        ;; If a module has entry points, or if it is a dependency,
        ;; then collect further information from it.
        (when (or (not ep?) (and eps-in-mod (not (dict-empty? eps-in-mod))))
          (define pt (Mod-pt mod)) ;; parse tree
          ;;(pretty-print (syntax->datum pt))
          (define-values (defs provs reqs)
            (parse-defs-from-module pt annos r-mp))
          (when eps-in-mod
            (set! defs (defs-annotate-export-names defs eps-in-mod)))
          ;;(pretty-print (dict->list defs)) (exit)
          (set! mod
                (struct-copy Mod mod
                             (defs defs) (provs provs) (reqs reqs)))
          (define raw-mp-lst
            (req-specs->module-paths reqs))
          ;;(pretty-print (dict-map defs cons))
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
  (set! all-defs (defs-resolve-names all-defs mods))
  ;;(pretty-print (dict-map all-defs (lambda (x y) y)))
  (set! all-defs (defs-drop-unreachable all-defs eps-in-prog))
  (set! all-defs ((make-for-all-defs ast-LetLocalEc->BlockExpr) all-defs))
  (set! all-defs (defs-simplify all-defs))
  ;;(pretty-print (dict-values all-defs)) (exit)
  (set! all-defs (defs-de-racketize all-defs))
  (defs-type-check all-defs)
  
  ;;(all-defs-display-Var-bindings all-defs)
  ;;(mods-display-Var-bindings mods)
  ;;(pretty-print (list 'entry-points (dict-map eps-in-prog (compose car cons))))
  (pretty-print (dict-map all-defs (lambda (x y) (ast->sexp y))))
  ;;(for (([k v] mods)) (pretty-print (list 'loaded k v)))

  (St mods all-defs eps-in-prog))

;; Compiles the modules defined in the specified files. Returns a
;; compilation state with a full IR for the entire program. The
;; returned program is still in a backend-agnostic form.
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

(require "backend-build-parser.rkt")
(require "backend-build-writer.rkt")
(require "backend-cxx-main.rkt")

(define (string-file-id? s)
  (regexp-match? #rx"^[a-zA-Z0-9_][a-zA-Z0-9_-]*$" s))

(define-with-contract*
  (->* (St? (hash/c symbol? (set/c symbol? #:cmp 'eq)))
       (#:outdir path-string?
        #:basename string?
        #:stdout boolean?
        #:banner boolean?)
       void?)
  (generate-files st backends
                  #:outdir [outdir (current-directory)]
                  #:basename [basename "output"]
                  #:stdout [stdout? #t]
                  #:banner [banner? #t])

  (unless (string-file-id? basename)
    (raise-argument-error
     'generate-files
     "file basename of non-zero length, without exotic characters"
     basename))

  (let ((kinds (hash-ref backends 'cxx #f)))
    (when (and kinds (not (set-empty? kinds)))
      (define defs (St-defs st))
      (define path-stem (build-path outdir basename))
      (generate-cxx-file kinds defs path-stem stdout? banner?)))

  (let ((kinds (hash-ref backends 'build #f)))
    (when (and kinds (not (set-empty? kinds)))
      (define defs (St-defs st))
      (define opts-stx (defs-collect-build-annos defs))
      (define opts-lst (parse-analyze-build-annos opts-stx))
      (define path-stem (build-path outdir (string-append basename "_build")))
      ;;(pretty-print opts-lst)
      (set-for-each
       kinds
       (lambda (kind)
         (generate-build-file kind opts-lst path-stem stdout? banner?)))))
  
  (void))

;;; 
;;; testing
;;; 

(module* main #f
  (define st (compile-modules "test-b-prog.rkt"))
  (generate-files st (hasheq ;;'build (seteq 'gnu-make 'qmake 'c 'ruby)
                             'cxx (seteq 'cc 'hh))))
