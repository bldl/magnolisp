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

(require "annos-parse.rkt" "ast-magnolisp.rkt" "ast-util.rkt"
         "compiler-util.rkt"
         "parse.rkt" "strategy.rkt" "util.rkt" "util/struct.rkt"
         syntax/id-table syntax/moddep)

;;; 
;;; identifier -> Id conversion
;;;

(define-with-contract
  (-> list? immutable-free-id-table?)
  (make-id->bind tl-def-lst)

  (define id->bind (make-immutable-free-id-table #:phase 0))
  
  (define f
    (topdown-visit
     (lambda (ast)
       (define id (binding-or-use-id ast))
       (when id
         (let ((def-id (or (syntax-property id 'def-id) id)))
           (unless (dict-has-key? id->bind def-id)
             (define bind (gensym (syntax-e def-id)))
             (set! id->bind (dict-set id->bind def-id bind))))))))
  
  (for-each f tl-def-lst)

  id->bind)

(define-with-contract
  (-> immutable-free-id-table? identifier? Id?)
  (conv-id->ast id->bind id)
  (define def-id (or (syntax-property id 'def-id) id))
  (define bind (dict-ref id->bind def-id #f))
  (unless bind
    (error 'conv-id->ast
           "unbound identifier ~a: ~s\n def-id = ~s\nentries = ~a"
           (syntax-e id) id
           (syntax-property id 'def-id)
           (dict-count id->bind)))
  (identifier->ast id #:bind bind))

;; Like above, but adds an entry into the table if the identifier is
;; not present.
(define-with-contract
  (-> immutable-free-id-table? identifier?
      (values immutable-free-id-table? Id?))
  (conv-id->ast/update id->bind id)
  (define def-id (or (syntax-property id 'def-id) id))
  (define bind (dict-ref id->bind def-id #f))
  (unless bind
    (set! bind (gensym (syntax-e id)))
    (set! id->bind (dict-set id->bind id bind)))
  (values id->bind (identifier->ast id #:bind bind)))

;; Updates the value of id->bind.
(define-syntax-rule
  (conv-id->ast/update! id->bind id-stx)
  (let-values ([(t id) (conv-id->ast/update id->bind id-stx)])
    (set! id->bind t)
    id))

(define (ast-id->ast id->bind ast)
  (define (mk-id id)
    (conv-id->ast id->bind id))

  (define (rw-annos annos)
    (define type-ast (hash-ref annos 'type-ast #f))
    (and type-ast
         (hash-set annos 'type-ast (rw type-ast))))

  (define (ast-rw-annos ast)
    (define annos (rw-annos (Ast-annos ast)))
    (if annos (ast-set-annos ast annos) ast))
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         ((? Def?)
          (define id (Def-id ast))
          (define id-ast (mk-id id))
          (dynamic-struct-copy Def ast (id id-ast)))
         ((Var a id)
          (define id-ast (mk-id id))
          (Var (or (rw-annos a) a) id-ast))
         ((NameT a id)
          (define id-ast (mk-id id))
          (NameT a id-ast))
         ((Label a id)
          (define id-ast (mk-id id))
          (Label a id-ast))
         (else
          (ast-rw-annos ast))))))

  (rw ast))

;;; 
;;; definition table utilities
;;;

;; A combinator that applies the rewrite 'rw' to each definition in
;; the passed set of definitions.
(define (make-for-all-defs/stx rw)
  (lambda (defs)
    (for/dict
     (make-immutable-free-id-table #:phase 0)
     (([id def] (in-dict defs)))
     (values id (rw def)))))

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

(define (defs-optimize-if TRUE-stx FALSE-stx defs)
  (define (make-f-pred id)
    (lambda (ast)
      (and (Apply? ast)
           (let ((f (Apply-f ast)))
             (and (Var? f)
                  (free-identifier=? (Var-id f) id))))))

  (define TRUE? (make-f-pred TRUE-stx))
  (define FALSE? (make-f-pred FALSE-stx))
  
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

  ((make-for-all-defs/stx rw) defs))

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
         (define n-ast (ast-set-annos v a))
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

(define (merge-defs mods)
  (define all-defs (make-free-id-table #:phase 0))
  (for (([rr-mp mod] mods))
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

(define (def-all-used-id-binds def)
  (define defs (make-hasheq))
  ((topdown-visit
    (lambda (ast)
      (define id (name-ref-id/maybe ast))
      (when id
        (hash-set! defs (Id-bind id) #t))))
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
  ;;(pretty-print `(original-defs ,(dict-count globals) ,(dict-keys globals) retained-defs ,(dict-count processed-defs) ,(dict-keys processed-defs)))
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
    (define n-def (ast-anno-set def 'export-name v))
    (set! defs-in-mod
          (dict-set defs-in-mod id n-def)))
  defs-in-mod)

;;; 
;;; compilation
;;;

(require "type-infer.rkt")

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof. All of the
;; 'ep-mp-lst' module paths should be either absolute ones, or '(file
;; ...)' paths relative to the working directory (no module relative
;; paths as entry points).
(define* (compile-modules
          #:relative-to [rel-to-path-v #f]
          . ep-mp-lst)
  ;;(writeln ep-mp-lst)

  (define mods (make-hasheq)) ;; rr-mp -> Mod
  (define eps-in-prog (make-free-id-table #:phase 0))
  (define dep-q null) ;; deps queued for loading

  (define (add-deps! deps)
    (set! dep-q (append dep-q deps)))

  (define (load ep? mp rel-to-path-v)
    ;;(writeln `(load ,ep? ,mp ,rel-to-path-v))
    (define r-mp (resolve-module-path mp rel-to-path-v))
    (define rr-mp (r-mp->rr-mp r-mp))
    (define mod (hash-ref mods rr-mp #f))
    (unless mod ;; not yet loaded
      ;;(writeln (list 'loading-submod-of r-mp mp))
      (set! mod (load-mod-from-submod r-mp mp))
      ;;(writeln `(LOADED ,rr-mp ,r-mp ,mp ,mod)) (exit)

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
          ;;(pretty-print (syntax->datum pt)) (exit)
          ;;(pretty-print (syntax->datum/binding pt #:pred (lambda (x) (memq x '(equal? r.equal?))))) (exit)
          ;;(print-with-select-syntax-properties '(in-racket local-ec) pt) (exit)
          ;;(pretty-print (syntax->datum/free-id pt)) (exit)
          (define-values (defs provs reqs)
            (parse-defs-from-module pt annos rr-mp))
          ;;(pretty-print reqs) (exit)
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
          (add-deps! (map
                      (lambda (raw-mp)
                        (list (syntax->datum raw-mp) r-mp))
                      raw-mp-lst))))

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

  (set! mods (mods-fill-in-syms mods))

  ;; Make note of interesting prelude definitions (if it is even a
  ;; dependency).
  (define-values (predicate-stx TRUE-stx FALSE-stx)
    (let* ((r-mp (resolve-module-path 'magnolisp/prelude #f))
           (rr-mp (r-mp->rr-mp r-mp))
           (mod (hash-ref mods rr-mp #f)))
      (define (get-id sym)
        (if (not mod)
            (datum->syntax #'here sym)
            (let ((syms (Mod-syms mod)))
              (define def (hash-ref syms sym #f))
              (unless def
                (error 'compile-modules
                       "prelude does not define '~a': ~s"
                       sym r-mp))
              (Def-id def))))
      (values (get-id 'predicate)
              (get-id 'TRUE)
              (get-id 'FALSE))))
  
  (define all-defs (merge-defs mods))
  (set! all-defs (defs-resolve-names all-defs mods))
  ;;(pretty-print (dict->list all-defs)) (exit)
  (set! all-defs (defs-optimize-if TRUE-stx FALSE-stx all-defs))
  ;;(pretty-print (dict-map all-defs (lambda (x y) y))) (exit)
  ;;(pretty-print (map ast->sexp (dict-values all-defs))) (exit)
  (define def-lst (dict-values all-defs))
  (set! def-lst (for/list ([def def-lst] #:unless (DefStx? def)) def))
  (define id->bind (make-id->bind def-lst))
  (set! def-lst (map (fix ast-id->ast id->bind) def-lst))
  (define eps-in-prog/Id
    (for/seteq ([(id x) (in-dict eps-in-prog)])
      (dict-ref id->bind id)))
  (set! def-lst (defs-drop-unreachable def-lst eps-in-prog/Id))
  (set! def-lst (map ast-rm-LetExpr def-lst))
  (set! def-lst (map ast-LetLocalEc->BlockExpr def-lst))
  (set! def-lst (map ast-simplify def-lst))
  ;;(parameterize ((show-bindings? #t)) (pretty-print (map ast->sexp (dict-values all-defs))))
  ;;(pretty-print (dict->list all-defs)) (exit)
  ;;(pretty-print (dict->list all-defs)) (exit)
  ;;(pretty-print (dict->list id->bind)) (exit)
  (define predicate-id (conv-id->ast/update! id->bind predicate-stx))
  ;;(pretty-print (dict-values all-defs)) (exit)
  (set! def-lst (map de-racketize def-lst))
  (defs-check-Apply-target def-lst)
  (set! all-defs (build-defs-table def-lst))
  (set! all-defs (defs-type-infer predicate-id all-defs))
  ;;(pretty-print (dict-values all-defs)) (exit)
  
  ;;(all-defs-display-Var-bindings all-defs) (exit)
  ;;(mods-display-Var-bindings mods)
  ;;(pretty-print (list 'entry-points (dict-map eps-in-prog (compose car cons))))
  ;;(for (([k v] mods)) (pretty-print (list 'loaded k v)))

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

(module* main #f
  (define st (compile-files "tests/test-block-expr-2.rkt"))
  (generate-files st '(
                       ;;(build (gnu-make qmake c ruby))
                       (cxx (cc hh))
                       )))
