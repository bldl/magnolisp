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

|#

(require "annos-parse.rkt" "app-util.rkt"
         "ast-magnolisp.rkt" "ast-util.rkt"
         "compiler-rewrites.rkt" "parse.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         syntax/moddep)

;;;
;;; de-Racketization
;;;

(define-with-contract
  (-> list? void?)
  (defs-check-Apply-target def-lst)
  (define bind->def (build-defs-table def-lst))
  (for-each
   (topdown-visit
    (lambda (ast)
      (match ast
        ((Apply _ e _)
         (assert (Var? e))
         (define id (Var-id e))
         (define def (ast-identifier-lookup bind->def id))
         ;; Base namespace names may be unresolved. (For now.)
         (when def
           (unless (matches? def (or (DefVar _ _ _ (? Lambda?))
                                     (Defun _ _ _ _ _)))
             (raise-language-error/ast
              "application target does not name a function"
              ast e))))
        (_ (void)))))
   def-lst))

(define-with-contract
  (-> Def? Def?)
  (de-racketize ast)
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((DefVar a1 n t (Lambda a2 p b))
          ;; Retain annos from the binding, which has any information
          ;; associated with the binding. It should also have the
          ;; declaration syntax corresponding to the function
          ;; definition.
          (when (hash-ref a1 'foreign #f)
            (set! b (annoless NoBody)))
          ;;(writeln (list n (hash-ref a1 'top)))
          (Defun a1 n t p b))
         
         (_ ast)))))
  
  (rw ast))

;;; 
;;; IfExpr and IfStat
;;; 

(define (defs-optimize-if TRUE-id FALSE-id defs)
  (define (make-f-pred id)
    (lambda (ast)
      (and (Apply? ast)
           (let ((f (Apply-f ast)))
             (and (Var? f)
                  (ast-identifier=? (Var-id f) id))))))

  (define TRUE? (make-f-pred TRUE-id))
  (define FALSE? (make-f-pred FALSE-id))
  
  (define-match-expander SomeIf
    (syntax-rules ()
      [(_ a c t e)
       (or (IfExpr a c t e)
           (IfStat a c t e))]))

  (define rw
    (bottomup
     (lambda (ast)
       (match ast
         ((SomeIf _ c t e)
          (cond
           ((TRUE? c) t)
           ((FALSE? c) e)
           (else ast)))
         (_ ast)))))

  (map rw defs))

;;; 
;;; LetExpr
;;; 

(define ast-rm-LetExpr
  (topdown
   (repeat
    (lambda (ast)
      (match ast
        ;; Simple case. Just retain expression 'v'.
        [(LetExpr _ (DefVar a1 bn t v) (Var _ rn))
         #:when (ast-identifier=? bn rn)
         (define a2 (Ast-annos v))
         (define a (hash-merge a1 a2))
         (when (Literal? v)
           (set! a (hash-set a 'type-ast t)))
         (define n-ast (set-Ast-annos v a))
         ;;(writeln (list n-ast (Ast-annos n-ast)))
         n-ast]
        ;; Complex case. Turn LetExpr into a BlockExpr + LetStat.
        [(? LetExpr?)
         (match-define (LetExpr a dv e) ast)
         (define n-ast
           (BlockExpr
            a
            (list
             (LetStat
              (hasheq 'let-kind (ast-anno-must ast 'let-kind))
              dv
              (list (annoless Return e))))))
         n-ast]
        [_ #f])))))

;;; 
;;; local escapes
;;; 

(define (ast-LetLocalEc->BlockExpr ast)
  (define enclosing-id (make-parameter #f))
  
  (define (rw ast)
    (match ast
     ((LetLocalEc a (Label _ k) ss)
      (parameterize ((enclosing-id k))
        (BlockExpr a (map rw ss))))
     ((AppLocalEc a (Label _ k) e)
      (define e-id (enclosing-id))
      (unless e-id
        (raise-language-error/ast
         "local escape without enclosing context"
         ast))
      (unless (ast-identifier=? e-id k)
        (raise-language-error/ast
         "local escape beyond its context"
         ast k
         #:fields (list (list "context" (ast-displayable e-id)))))
      (Return a (rw e)))
     (else
      (all-rw-term rw ast))))

  (rw ast))

;;; 
;;; program contents resolution
;;; 

(require "name-resolve.rkt")

;; Compilation state. [mods hash?] maps resolved module paths to Mod
;; objects. [defs hash?] maps bind symbols to AST nodes. [eps
;; id-table?] has entry points as keys, and #t values.
(struct St (mods defs eps) #:transparent)

(define* compilation-state? St?)

(define (merge-defs mods)
  (define all-defs (make-hasheq)) ;; bind -> Def
  (define next-r #hasheq())
  (define x-binds (make-hash)) ;; (cons/c rr-mp sym) -> bind
  (for ([(rr-mp mod) mods])
    (define def-lst (Mod-def-lst mod))
    (define bind->binding (Mod-bind->binding mod))
    (define m->p-bind (make-hasheq)) ;; local bind -> global bind
    (define (rw-id id)
      (define m-bind (Id-bind id))
      (define info (hash-ref bind->binding m-bind))
      (cond-or-fail
       [(list? info)
        (define r-mp (first info))
        (define sym (second info))
        (define rr-mp (r-mp->rr-mp r-mp))
        (define p-bind (hash-ref m->p-bind m-bind #f))
        (unless p-bind
          (define mp-and-sym (cons rr-mp sym))
          (set! p-bind (hash-ref x-binds mp-and-sym #f))
          (unless p-bind
            (set!-values (next-r p-bind) (next-gensym next-r (Id-name id)))
            (hash-set! x-binds mp-and-sym p-bind))
          (hash-set! m->p-bind m-bind p-bind))
        (set-Id-bind id p-bind)]
       [(or (eq? 'lexical info) (not info))
        (define p-bind (hash-ref m->p-bind m-bind #f))
        (unless p-bind
          (set!-values (next-r p-bind) (next-gensym next-r (Id-name id)))
          (hash-set! m->p-bind m-bind p-bind))
        (set-Id-bind id p-bind)]))
    (for ([def def-lst])
      (define n-def (ast-rw-Ids rw-id def))
      (hash-set! all-defs (Id-bind (Def-id n-def)) n-def)))
  all-defs)
    
(define (def-all-used-id-binds def)
  (define defs (make-hasheq))
  ((topdown-visit
    (lambda (ast)
      (define id (name-ref-id/maybe ast))
      (when id
        (hash-set! defs (Id-bind id) #t))))
   def)
  (dict-keys defs))

;; Drops all top-level definitions in 'tl-def-lst' that are not used
;; via at least one of the entry points in 'eps' (which has 'Id'
;; 'bind' values as keys). This relies on name references (within the
;; codebase) having been resolved. Returns the trimmed down
;; definitions.
(define-with-contract
  (-> (listof Def?)
      (set/c symbol? #:cmp 'eq)
      (listof Def?))
  (defs-drop-unreachable tl-def-lst eps)
  (define globals (build-global-defs-table tl-def-lst))
  (define processed-ids (mutable-seteq))
  (define processed-defs null)
  (let loop ((ids-to-process (set->list eps)))
    (unless (null? ids-to-process)
      (define next-ids null)
      (for ((id-bind ids-to-process))
        (unless (set-member? processed-ids id-bind)
          (set-add! processed-ids id-bind)
          (define def (dict-ref globals id-bind #f))
          (when def
            (set! processed-defs (cons def processed-defs))
            (define refs-in-def (def-all-used-id-binds def))
            (set! next-ids (append next-ids refs-in-def)))))
      (loop next-ids)))
  ;;(pretty-print `(ORIGINAL-DEFS ,(dict-count globals) ,(dict-keys globals) RETAINED-DEFS ,(length processed-defs) ,(map (compose Id-name Def-id) processed-defs)))
  processed-defs)

;;; 
;;; prelude
;;; 

;; Loads a map of IDs by name of provided symbol, for the specified
;; module. This will not work for symbols that have been renamed on
;; export. Only includes IDs that (1) are defined within the module,
;; (2) are variables rather than syntax, (3) are exported at phase
;; level 0, and (4) are Magnolisp. The result will be #f for any Mod
;; that has not been loaded.
(define (build-provide-map mods mp)
  (let* ((r-mp (resolve-module-path mp #f))
         (rr-mp (r-mp->rr-mp r-mp))
         (mod (hash-ref mods rr-mp #f)))
    (and mod
        (let ()
          (define syms (mutable-seteq))
          (define-values (vars stxs) (module->exports rr-mp))
          ;;(writeln `(exports for ,mp are ,vars))
          ;;(writeln `(stxs ,stxs))
          (for ([var vars]
                #:when (equal? (car var) 0))
            (for ([sym-and-lst (cdr var)])
              (define exp-sym (first sym-and-lst))
              (define origins (second sym-and-lst))
              ;; If something locally defined has been renamed on export,
              ;; we cannot see the original name from 'origins'.
              ;;(writeln `(origins of ,exp-sym in ,mp are ,origins))
              (when (null? origins)
                (set-add! syms exp-sym))))
          (for/hasheq ([def (Mod-def-lst mod)]
                       #:when (set-member? syms (Id-name (Def-id def))))
            (define id (Def-id def))
            (values (Id-name id) id))))))
                
;;; 
;;; compilation
;;;

(require "type-infer.rkt")

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof. All of the
;; 'ep-mp-lst' module paths should be either absolute ones, or '(file
;; ...)' paths relative to the working directory, unless
;; 'rel-to-path-v' is specified for relative path resolution.
(define* (compile-modules
          #:relative-to [rel-to-path-v #f]
          . ep-mp-lst)
  ;; resolved-module-path? is eq? comparable
  (define mods (make-hasheq)) ;; rr-mp -> Mod
  
  (define eps-in-prog (mutable-seteq)) ;; of bind
  
  (define dep-q null) ;; deps queued for loading

  (define (load ep? mp rel-to-path-v)
    ;;(writeln `(load mp ,mp))
    (define r-mp (resolve-module-path/primitive mp rel-to-path-v))
    ;;(writeln `(load r-mp ,r-mp))
    (define rr-mp (r-mp->rr-mp r-mp)) ;; resolved-module-path?
    ;;(writeln `(load rr-mp ,rr-mp))
    ;;(writeln `(load entry: ,ep? mp: ,mp rel: ,rel-to-path-v r-mp: ,r-mp rr-mp: ,rr-mp))
    (define mod (hash-ref mods rr-mp #f))
    (unless mod ;; not yet loaded
      ;;(writeln (list 'loading-submod-of r-mp mp))
      (set! mod (load-mod-from-submod r-mp mp))
      ;;(writeln `(LOADED ,rr-mp ,r-mp ,mp ,mod))

      (define def-lst (Mod-def-lst mod))
      
      ;; For entry point modules, use annotations to build a set of
      ;; entry points. Add these to program entry points.
      (define eps-in-mod #f) ;; (or/c #f (hash/c bind Def?)) xxx not actually being used
      (when ep?
        ;; We only consider top-level things here.
        (set! eps-in-mod
              (for/hasheq ([def def-lst]
                           #:when (ast-anno-maybe def 'export))
                (define bind (Id-bind (Def-id def)))
                (set-add! eps-in-prog bind)
                (values bind def))))

      ;; Build a list of dependencies for this module from the
      ;; bind->binding table. Stored as (list dep-r-mp rel-r-mp) per
      ;; entry.
      (for ([(bind info) (Mod-bind->binding mod)]
            #:when (list? info))
        (define dep-r-mp (first info))
        (define dep-mp (r-mp->mp dep-r-mp))
        (set! dep-q (cons (list dep-mp r-mp) dep-q)))

      (hash-set! mods rr-mp mod)))

  ;; Load all the "entry" modules.
  (for ((mp ep-mp-lst))
    (load #t mp rel-to-path-v))

  ;; Keep loading dependencies until all loaded.
  (let loop ()
    (unless (null? dep-q)
      (define mp-lst dep-q)
      (set! dep-q null)
      (for ((mp-and-rel mp-lst))
        ;;(writeln mp-and-rel)
        (apply load #f mp-and-rel))
      (loop)))

  ;;(writeln `(eps-in-prog ,eps-in-prog))
  ;;(writeln `(loaded mods ,(hash-keys mods)))
  
  ;; Make note of interesting prelude definitions (if it is even a
  ;; dependency).
  (define-values (predicate-id TRUE-id FALSE-id)
    (let* ((mp 'magnolisp/prelude)
           (syms (build-provide-map mods mp)))
      (define (get-id sym)
        (if (not syms)
            (fresh-ast-identifier sym)
            (let ()
              (define id (hash-ref syms sym #f))
              (unless id
                (error 'compile-modules
                       "prelude does not define '~a': ~s"
                       sym mp))
              id)))
      (values (get-id 'predicate)
              (get-id 'TRUE)
              (get-id 'FALSE))))

  ;;(writeln (list predicate-id TRUE-id FALSE-id))
  
  (define all-defs (merge-defs mods))
  ;;(pretty-print (dict->list all-defs)) (exit)
  ;;(pretty-print (map ast->sexp (dict-values all-defs))) (exit)
  (define def-lst (hash-values all-defs))
  
  (set! def-lst (defs-optimize-if TRUE-id FALSE-id def-lst))
  (set! def-lst (defs-drop-unreachable def-lst eps-in-prog))
  (set! def-lst (map ast-rm-LetExpr def-lst))
  (set! def-lst (map ast-LetLocalEc->BlockExpr def-lst))
  (set! def-lst (map ast-simplify def-lst))
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values all-defs))))
  ;;(pretty-print (dict->list all-defs)) (exit)
  ;;(pretty-print (dict->list all-defs)) (exit)
  ;;(pretty-print (dict->list id->bind)) (exit)
  ;;(pretty-print (dict-values all-defs)) (exit)
  (set! def-lst (map de-racketize def-lst))
  (defs-check-Apply-target def-lst)
  ;;(pretty-print def-lst)
  (set! all-defs (build-defs-table def-lst))
  (set! all-defs (defs-type-infer predicate-id all-defs))
  ;;(pretty-print (dict-values all-defs)) (exit)
  
  ;;(pretty-print (map ast->sexp (dict-values all-defs)))
  
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

(require "backend-build-main.rkt")
(require "backend-cxx-main.rkt")

(define (string-file-id? s)
  (regexp-match? #rx"^[a-zA-Z0-9_][a-zA-Z0-9_-]*$" s))

(define-with-contract*
  (->* (St? (listof (cons/c symbol? any/c)))
       (#:outdir path-string?
        #:basename string?
        #:out output-port?
        #:banner boolean?)
       void?)
  (generate-files st backends
                  #:outdir [outdir (current-directory)]
                  #:basename [basename "output"]
                  #:out [out (current-output-port)]
                  #:banner [banner? #t])

  (unless (string-file-id? basename)
    (raise-argument-error
     'generate-files
     "file basename of non-zero length, without exotic characters"
     basename))

  (when-let entry (assq 'cxx backends)
    (match entry
      ((list _ (list (? symbol? kinds) ...))
       (unless (null? kinds)
         (set! kinds (remove-duplicates kinds eq?))
         (define defs (St-defs st))
         (define path-stem (build-path outdir basename))
         (generate-cxx-file kinds defs path-stem out banner?)))))

  (when-let entry (assq 'build backends)
    (match entry
      ((list _ (list (? symbol? kinds) ...))
       (unless (null? kinds)
         (set! kinds (remove-duplicates kinds eq?))
         (define defs (St-defs st))
         (define opts-stx (defs-collect-build-annos defs))
         (define opts-lst (parse-analyze-build-annos opts-stx))
         (define path-stem (build-path outdir (string-append basename "_build")))
         ;;(pretty-print opts-lst)
         (for ((kind kinds))
           (generate-build-file kind opts-lst path-stem out banner?))))))
  
  (void))

;;; 
;;; testing
;;; 

(module* test #f
  (define st (compile-files "tests/test-names-1.rkt"))
  (generate-files st '(
                       ;;(build (gnu-make qmake c ruby))
                       (cxx (cc hh))
                       )))
