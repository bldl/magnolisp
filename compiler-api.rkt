#lang racket

#|

Implements a compiler for Magnolisp. Loads the code to be compiled
from Racket module metadata, included as submodules by the Racket
'magnolisp' language implementation.

The compiler ignores module top-level expressions.

The compiler requires a fully typed program (although not all types
have to be written out explicitly -- think 'auto' in C++).

Compiles only the 'export' operations of the specified modules, and
their dependencies. This essentially means full program/library
optimization.

|#

(require "app-util.rkt" "ast-magnolisp.rkt" "ast-util.rkt"
         "compiler-rewrites.rkt" "parse.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         syntax/moddep)

;;; 
;;; AnnoExpr removal
;;; 

(define (Anno-kind+value ast)
  (cond
   ((TypeAnno? ast) (values 'type (TypeAnno-t ast)))
   ((GenericAnno? ast) (values (GenericAnno-kind ast) (GenericAnno-datum ast)))
   (else (raise-argument-error 
          'Anno-kind+value
          "supported Anno object" ast))))

(define (Anno-ast-lst->h as)
  (for/hasheq ([anno-ast as])
    (Anno-kind+value anno-ast)))

(define (Def-process-annos ast)
  (define ann-h (Ast-annos ast))
  (define id (Def-id ast))
  
  (define (put! n v)
    (set! ann-h (hash-set ann-h n v)))

  (put! 'top #t)
  
  ;;(pretty-print ann-h)
  (define foreign (hash-ref ann-h 'foreign #f))
  (define export (hash-ref ann-h 'export #f))

  (match ast
    [(DefVar a id _ (ForeignTypeExpr _))
     (unless foreign
       (raise-language-error/ast
        "missing 'foreign' C++ type annotation"
        ast))
     ;;(writeln `(foreign ,foreign))
     (let ((foreign
            (cond
             ((equal? #t foreign)
              (annoless ForeignNameT (datum->syntax #f (Id-name id))))
             ((identifier? foreign)
              (syntaxed foreign ForeignNameT foreign))
             (else
              (error 'Def-process-annos
                     "unexpected 'foreign anno ~s" foreign)))))
       (set! ast (ForeignTypeDecl a id foreign)))]
    [_ (void)])
  
  (when (and foreign export)
    (raise-language-error/ast
     (format "definition ~a marked both as 'export' and 'foreign'"
             (ast-displayable/datum id))
     ast))
  
  (set-Ast-annos ast ann-h))

(define-with-contract
  (-> Def? Def?)
  (Def-rm-AnnoExpr def)
  
  (define rw-merge
    (topdown
     (repeat
      (lambda (ast)
        (match ast
          [(AnnoExpr a as-1 (AnnoExpr _ as-2 e))
           (AnnoExpr a (hash-merge-2 as-2 as-1) e)]
          [_ #f])))))
  
  (define rw-incorporate
    (topdown
     (repeat
      (lambda (ast)
        ;;(when (DefVar? ast) (writeln ast))
        (match ast
          [(DefVar a id t (AnnoExpr _ as e))
           (define ann-h (Anno-ast-lst->h as))
           (set! a (hash-merge-2 a ann-h))
           (when-let n-t (hash-ref a 'type #f)
             (set! a (hash-remove a 'type))
             (set! t n-t))
           (DefVar a id t e)]
          [(AnnoExpr _ as e)
           ;;(writeln `(bare AnnoExpr seen ,ast))
           (define ann-h (Anno-ast-lst->h as))
           (modify-ast-annos e (lambda (h) (hash-merge-2 h ann-h)))]
          [_ #f])))))
  
  (rw-incorporate (rw-merge def)))

(define (mods-rm-AnnoExpr mods)
  (for/hasheq ([(rr-mp mod) mods])
    (define def-lst (Mod-def-lst mod))
    (set! def-lst
          (for/list ([def def-lst])
            (Def-process-annos (Def-rm-AnnoExpr def))))
    (values rr-mp (struct-copy Mod mod [def-lst def-lst]))))

;;;
;;; de-Racketization
;;;

(define-with-contract
  (-> list? void?)
  (defs-check-ApplyExpr-target def-lst)
  (define bind->def (build-defs-table def-lst))
  (for-each
   (topdown-visit
    (lambda (ast)
      (match ast
        ((ApplyExpr _ e _)
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
            (set! b the-NoBody))
          ;;(writeln (list n (hash-ref a1 'top)))
          (Defun a1 n t p b))
         
         (_ ast)))))
  
  (rw ast))

;;; 
;;; IfExpr and IfStat
;;; 

(define (defs-optimize-if defs)
  (define ((make-lit-pred lit) ast)
    (matches? ast (Literal _ (? (lambda (x) (equal? x lit))))))

  (define TRUE? (make-lit-pred #t))
  (define FALSE? (make-lit-pred #f))
  
  (define rw
    (bottomup
     (lambda (ast)
       (match ast
         [(IfExpr _ c t e)
          (cond
           [(TRUE? c) t]
           [(FALSE? c) e]
           [else ast])]
         [_ ast]))))

  (map rw defs))

;;; 
;;; program contents resolution
;;; 

(require "module-load.rkt")

;; Compilation state. [defs hash?] maps bind symbols to AST nodes.
;; [eps (set/c symbol? #:cmp 'eq)] contains bind symbols for program
;; entry points.
(struct St (defs eps) #:transparent)

(define* compilation-state? St?)

(define-with-contract
  (-> hash? (values hash? (set/c symbol? #:cmp 'eq) hash?))
  (merge-defs mods)
  
  (define eps-in-prog (mutable-seteq)) ;; of bind
  (define all-defs (make-hasheq)) ;; bind -> Def
  (define next-r #hasheq())
  (define x-binds (make-hash)) ;; (cons/c rr-mp sym) -> bind
  (for ([(rr-mp mod) mods])
    (define def-lst (Mod-def-lst mod))
    (define bind->binding (Mod-bind->binding mod))
    ;;(pretty-print `(,rr-mp ,(Mod-r-mp mod) bind->binding ,bind->binding))
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
            (set!-values (next-r p-bind) (next-gensym1 next-r sym))
            (hash-set! x-binds mp-and-sym p-bind))
          (hash-set! m->p-bind m-bind p-bind))
        (set-Id-bind id p-bind)]
       [(or (eq? 'lexical info) (not info))
        (define p-bind (hash-ref m->p-bind m-bind #f))
        (unless p-bind
          (set!-values (next-r p-bind) (next-gensym1 next-r (Id-name id)))
          (hash-set! m->p-bind m-bind p-bind))
        (set-Id-bind id p-bind)]))
    (define n-def-lst
      (for/list ([def def-lst])
        (define n-def (ast-rw-Ids rw-id def))
        (hash-set! all-defs (Id-bind (Def-id n-def)) n-def)
        n-def))
    ;;(pretty-print `(,rr-mp x-binds ,x-binds m->p-bind ,m->p-bind))
    (when (Mod-ep? mod)
      (for ([def n-def-lst]
            #:when (ast-anno-maybe def 'export))
        (define bind (Id-bind (Def-id def)))
        (set-add! eps-in-prog bind)))
    (void)) ;; end (for ([(rr-mp mod) mods])
  ;;(pretty-print all-defs)
  (values all-defs eps-in-prog x-binds))

;; Returns a list of all Id-bind's appearing within a Def.
(define-with-contract
  (-> Def? (listof symbol?))
  (def-all-used-id-binds def)
  
  (define binds (mutable-seteq)) ;; (set/c bind)

  (define rw
    (topdown-visit
     (lambda (ast)
       (define id (name-ref-id/maybe ast))
       (when id
         ;;(writeln `(found used Id ,id))
         (set-add! binds (Id-bind id)))
       (when (Expr? ast)
         (when-let t (Expr-type ast)
           (rw t))))))
  
  (rw def)
  (set->list binds))

;; Drops all top-level definitions in 'tl-def-lst' that are not used
;; via at least one of the entry points in 'eps' (which has 'Id'
;; 'bind' values as keys). Returns the trimmed down collection of
;; definitions.
(define-with-contract
  (-> (listof Def?)
      (set/c symbol? #:cmp 'eq)
      (listof Def?))
  (defs-drop-unreachable tl-def-lst eps)

  (define globals (build-global-defs-table tl-def-lst)) ;; bind -> Def?
  ;;(pretty-print `(ORIGINAL-DEFS ,globals eps ,eps))
  (define processed-ids (mutable-seteq)) ;; (set/c bind)
  (define processed-defs null) ;; (listof Def?)
  (let loop ((ids-to-process (set->list eps)))
    (unless (null? ids-to-process)
      (define next-ids null) ;; (listof bind)
      (for ((id-bind ids-to-process))
        (unless (set-member? processed-ids id-bind)
          (set-add! processed-ids id-bind)
          (define def (dict-ref globals id-bind #f))
          (when def
            (set! processed-defs (cons def processed-defs))
            (define refs-in-def (def-all-used-id-binds def))
            (set! next-ids (append next-ids refs-in-def)))))
      (loop next-ids)))
  ;;(pretty-print `(ORIGINAL-DEFS ,(dict-count globals) ,(dict-keys globals) RETAINED-DEFS ,(length processed-defs) ,(map (compose Id-bind Def-id) processed-defs)))
  processed-defs)

;;; 
;;; prelude
;;; 

(define prelude-mp 'magnolisp/prelude)

;; Builds a map of bind values in loaded prelude to the fixed bind
;; values of definitions that are special to the compiler. The map
;; makes it possible to switch the definitions over to said known
;; values.
(define (build-prelude-bind->bind rr-mp-sym->bind the-sym->bind)
  (define prelude-rr-mp
    (let* ((r-mp (resolve-module-path prelude-mp #f))
           (rr-mp (r-mp->rr-mp r-mp)))
      rr-mp))
  
  (for/hasheq (((sym the-bind) the-sym->bind))
    (define prelude-bind 
      (hash-ref rr-mp-sym->bind 
                (cons prelude-rr-mp sym)
                (thunk
                 (error 'compile-modules
                        "'~a' does not define '~a': ~s"
                        prelude-mp sym prelude-rr-mp))))
    (assert (not (eq? prelude-bind the-bind)))
    (values prelude-bind the-bind)))

;; Sets identifier references that matter to the compiler, but which
;; Racket will not have resolved, so that they can be accounted for
;; during whole-program optimization. The type names of literals are
;; the only such dependencies.
(define (ast-add-prelude-lit-types ast)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((Literal a (? boolean? d))
          #:when (not (Expr-type ast))
          (set-Expr-type ast the-bool-type))
         (_ ast)))))
  (rw ast))

;; Applies the identifier mappings specified by `bind->builtin`, which
;; maps bind values to bind values. Modifies mutable set `eps-in-prog`
;; in place. Returns a modified copy of `def-lst`.
(define (switch-ids-for-builtins! def-lst eps-in-prog bind->builtin)
  (for (((k v) bind->builtin))
    (when (set-member? eps-in-prog k)
      (set-remove! eps-in-prog k)
      (set-add! eps-in-prog v)))

  (define (rw-id id)
    (define bind (Id-bind id))
    (define v (hash-ref bind->builtin bind #f))
    (if (not v)
        id
        (set-Id-bind id v)))
  
  (define rw (curry ast-rw-Ids rw-id))
  
  (for/list ((def def-lst))
    (rw def)))

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
      (set! mod (Mod-load r-mp mp ep?))
      ;;(writeln `(LOADED ,rr-mp ,r-mp ,mp ,mod))

      (define def-lst (Mod-def-lst mod))
      
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

  ;; Load prelude if not already loaded. It may be a dependency during
  ;; compilation even if no Racket identifier references it.
  (load #f prelude-mp #f)
  
  ;;(writeln `(eps-in-prog ,eps-in-prog))
  ;;(writeln `(loaded mods ,(hash-keys mods)))
  ;;(pretty-print `(loaded modules ,mods)) (exit)
  ;;(displayln 'ast-after-marshaling) (for ([(x mod) mods]) (for ([def (Mod-def-lst mod)]) (ast-dump-loc-info def)))
  
  (set! mods (mods-rm-AnnoExpr mods))
  ;;(pretty-print mods) (exit)
  (define-values (all-defs eps-in-prog rr-mp-sym->bind)
    (merge-defs mods))
  
  (define prelude-bind->bind
    (build-prelude-bind->bind 
     rr-mp-sym->bind
     (for/hasheq ((id builtin-type-id-lst))
       (values (Id-name id) (Id-bind id)))))
  
  (define def-lst (hash-values all-defs))
  
  (set! def-lst
        (switch-ids-for-builtins! def-lst eps-in-prog
                                  prelude-bind->bind))

  (set! def-lst (defs-optimize-if def-lst))
  (set! def-lst (map ast-add-prelude-lit-types def-lst))
  ;;(pretty-print `(prelude-map ,prelude-bind->bind defs ,def-lst eps ,eps-in-prog)) (exit)
  (set! def-lst (defs-drop-unreachable def-lst eps-in-prog))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map ast-simplify def-lst))
  (set! def-lst (map de-racketize def-lst))
  (defs-check-ApplyExpr-target def-lst)
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map ast-normalize-LetLocalEc def-lst))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map ast-update-ExprLike-result-annos def-lst))
  ;;(pretty-print def-lst) (exit)
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values all-defs))))
  ;;(pretty-print def-lst)
  (set! all-defs (build-defs-table def-lst))
  ;;(pretty-print all-defs) (exit)
  (set! all-defs (defs-type-infer all-defs))
  ;;(pretty-print all-defs)
  (set! all-defs (defs-map/bind ast-rm-dead-constants all-defs))
  ;;(pretty-print all-defs) (exit)
  ;;(pretty-print (dict-values all-defs)) (exit)
  
  ;;(pretty-print (map ast->sexp (dict-values all-defs)))
  
  (St all-defs eps-in-prog))

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
        #:out (or/c #f output-port?)
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
  (define st (compile-files "tests/test-simple-1.rkt"))
  (generate-files st '(
                       ;;(build (gnu-make qmake c ruby))
                       (cxx (cc hh))
                       )))
