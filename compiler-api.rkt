#lang racket/base

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

(require racket/contract/base
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/pretty
         racket/set
         "app-util.rkt"
         "ir-id-coll.rkt"
         "ir-ast.rkt"
         "backend-build-main.rkt"
         "backend-cxx-main.rkt"
         "backend-mgl-print.rkt"
         "backend-util.rkt"
         "ir-transform.rkt"
         "module-load.rkt"
         "strategy.rkt"
         "strategy-stratego.rkt"
         "strategy-term.rkt"
         "type-infer.rkt"
         "util.rkt"
         "util/field.rkt")

;;;
;;; AnnoExpr removal
;;;

(define (Anno->values ast)
  (cond
   ((TypeAnno? ast) (values 'type (TypeAnno-t ast)))
   ((GenericAnno? ast) (values (GenericAnno-kind ast) (GenericAnno-datum ast)))
   (else (raise-argument-error
          'Anno->values
          "supported Anno object" ast))))

(define (Anno->hash ast)
  (define-values (k v) (Anno->values ast))
  (hasheq k v))

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

;; Removes `AnnoExpr` nodes by incorporating their information into
;; surrounding nodes' annotation fields.
(define-with-contract
  (-> Def? Def?)
  (Def-rm-AnnoExpr def)

  (define rw-merge
    (topdown
     (repeat
      (lambda (ast)
        (match ast
          [(AnnoExpr a as-1 (AnnoExpr _ as-2 e))
           ;;(pretty-print `(MERGING ,as-1 with ,as-2))
           (AnnoExpr a (append as-1 as-2) e)]
          [_ #f])))))

  (define (merge-into-hash h ast-lst)
    ;; Use separate hashes to preserve conflicting keys, and to thus
    ;; allow `merge-annos` to decide how to deal with conflicts.
    (apply merge-annos h (map Anno->hash ast-lst)))

  (define rw-incorporate
    (topdown
     (repeat
      (lambda (ast)
        ;;(when (DefVar? ast) (writeln ast))
        (match ast
          [(DefVar a id t (AnnoExpr _ as e))
           ;;(pretty-print `(WITH ANNOS ,ast))
           (define n-a (merge-into-hash a as))
           (when-let n-t (hash-ref n-a 'type #f)
             (set! n-a (hash-remove n-a 'type))
             (set! t n-t))
           (DefVar n-a id t e)]
          [(AnnoExpr _ as e)
           ;;(writeln `(bare AnnoExpr seen ,ast))
           (modify-ast-annos e (lambda (h) (merge-into-hash h as)))]
          [_ #f])))))

  (rw-incorporate (rw-merge def)))

(define (mods-rm-AnnoExpr mods)
  (for/hasheq ([(rr-mp mod) mods])
    (define def-lst
      (for/list ([def (Mod-def-lst mod)])
        (Def-process-annos (Def-rm-AnnoExpr def))))
    (values rr-mp (struct-copy Mod mod [def-lst def-lst]))))

;;;
;;; Begin0 translation
;;;

(define ast-rm-Begin0
  (bottomup
   (lambda (ast)
     (match ast
       [(Begin0 a (list e bs ..1))
        (define id (fresh-Id 'bg))
        (define dv (annoless DefVar id the-AnyT e))
        (LetExpr a dv (append bs (list (annoless Var id))))]
       [_ ast]))))

;;;
;;; de-Racketization
;;;

(define-with-contract
  (-> list? void?)
  (defs-check-ApplyExpr-target def-lst)
  (define bind->def (build-full-defs-table def-lst))
  (for-each
   (topdown-visitor
    (lambda (ast)
      (match ast
        [(fields ApplyExpr [f e])
         (assert (Var? e))
         (define id (Var-id e))
         (define def (ast-identifier-lookup bind->def id))
         ;; Base namespace names may be unresolved. (For now.)
         (when def
           (unless (Defun? def)
             (raise-language-error/ast
              "application target does not name a function"
              ast e)))]
        [_ (void)])))
   def-lst))

(define (topdown-has-matching? p? ast)
  (let/ec k
    ((topdown-visitor
      (lambda (x)
        (when (p? x)
          (k #t))))
     ast)
    #f))

;; The `ast` argument is only used for error reporting. The `f?` flag
;; indicates whether the function is foreign. Any original annotations
;; for the binding should be in `annos`. The `Param`eter list `ps` and
;; function `body` must be specified also for foreign functions, even
;; if they must be inferred.
(define (mk-Defun ast f? annos n t ps body)
  (when (topdown-has-matching? ForAllT? t)
    (unless f?
      (raise-language-error/ast
       "non-`foreign` function has generic type"
       ast t))
    (set! annos (hash-set annos 'generic-type t))
    (define-values (u-lst u-t)
      (type-expr-rm-ForAllT/def t))
    (set! annos (hash-set annos 'univ-type-params u-lst))
    (when (type-expr-return-type-overloaded? t)
      (set! annos (hash-set annos 'return-type-overloaded? #t)))
    (set! t u-t))

  (define n-ast 
    (Defun annos n t ps body))
  ;;(writeln n-ast)
  n-ast)

(define-datatype (UseKind)
  ((UseType) (UseVar) (UseFunc arity))
  #:transparent)

;; Accumulates information about name uses in `ast`, updating `kinds`
;; with that information (of type `UseKind`).
(define-with-contract
  (-> hash? Ast? void?)
  (update-kinds-with! kinds ast)
  
  (define (visit-any-type ast)
    (define t (Expr-type ast))
    (when t
      (rw t))
    (void))
  
  (define rw
    (alltd
     (lambda (ast)
       (match ast
         [(Var _ id)
          (dict-set! kinds id (UseVar))
          (visit-any-type ast)]
         [(NameT _ id)
          (dict-set! kinds id (UseType))]
         [(ApplyExpr _ (Var _ id) as)
          (dict-set! kinds id (UseFunc (length as)))
          (visit-any-type ast)
          (for-each rw as)]
         [_ #f]))))

  (rw ast)
  (void))

;; Turns variable definitions into function definitions as
;; appropriate.
(define-with-contract
  (-> dict? Def? Def?)
  (def-make-Defuns kinds ast)

  (define (foreign? annos)
    (and (hash-ref annos 'foreign #f) #t))

  (define (fun-use id)
    (dict-ref kinds id #f))
  
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(DefVar a n t e)
          (define f? (foreign? a))
          (define use (fun-use n))
          ;;(writeln (list f? use ast))
          (match e
            [(Lambda _ ps orig-body)
             (define body (if f? the-NoBody orig-body))
             (mk-Defun ast f? a n t ps body)]
            [_
             (cond
               [(and f? (UseFunc? use))
                (define arity (UseFunc-arity use))
                (define ps
                  (for/list ([at (in-range arity)])
                    (define id (fresh-Id 'arg))
                    (annoless Param id the-AnyT)))
                (define body the-NoBody)
                (mk-Defun ast f? a n t ps body)]
               [else
                ast])])]
         [_ ast]))))

  (rw ast))

(define (defs-make-Defuns def-lst)
  (define kinds (make-hashId))
  (for ((def def-lst))
    (update-kinds-with! kinds def))
  ;;(pretty-print kinds) (exit)

  (define lst
    (for/list ((def def-lst))
      (def-make-Defuns kinds def)))
  ;;(pretty-print lst) (exit)
  lst)

(define-with-contract
  (-> (listof Def?) (listof Def?))
  (defs-lift-typedefs def-lst)

  (define t-defs (mutable-seteq))

  (define rw-Defun
    (bottomup
     (lambda (ast)
       (match ast
         [(LetExpr a (fields DefVar dv [body (? ForeignTypeExpr?)]) body)
          (set-add! t-defs (Def-process-annos dv))
          (SeqExpr a body)]
         [_ ast]))))
  
  (define n-def-lst
    (for/list ((ast def-lst))
      (if (Defun? ast)
          (rw-Defun ast)
          ast)))
  
  (append (set->list t-defs) n-def-lst))

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
;;; lifting of local functions
;;; 

;; Removes the free variables that local functions close over. Assumes
;; a fully typed AST, without local types, but possibly with local
;; functions.
(define-with-contract
  (-> Def? Def?)
  (fun-rm-free-vars def)

  (define this (make-parameter #f))
  (struct FunInfo (id args vars calls uses) #:transparent)
  (define fun-lst null)
  (define seen-use-lst null)
  (define types-h (mutable-hashId)) ;; for Param, DefVar, and DeclVar

  (define (find-uses)
    (define (collect ast)
      (match ast
        [(Defun _ id _ ps b)
         (for ((p ps))
           (dict-set! types-h (Param-id p) (Param-t p)))
         (define info (FunInfo id
                               (for/mutable-setId ((p ps))
                                 (Param-id p))
                               (mutable-setId)
                               (mutable-setId)
                               (mutable-setId)))
         (set! fun-lst (cons info fun-lst))
         (parameterize ((this info))
           (collect b))]
        [(DefVar _ id t b)
         (dict-set! types-h id t)
         (set-add! (FunInfo-vars (this)) id)
         (collect b)]
        [(DeclVar _ id t)
         (dict-set! types-h id t)
         (set-add! (FunInfo-vars (this)) id)]
        [(ApplyExpr _ (Var _ id) as)
         (set-add! (FunInfo-calls (this)) id)
         ;; important not to add `id` into `uses`
         (map collect as)]
        [(Var _ id)
         (set-add! (FunInfo-uses (this)) id)
         (set! seen-use-lst (cons id seen-use-lst))]
        [_
         (term-visit-all collect ast)]))

    (collect def)

    (for ((info fun-lst))
      ;; make it just direct free var `uses`
      (set-subtract! (FunInfo-uses info)
                     (FunInfo-args info)
                     (FunInfo-vars info))))

  (define (any-free-vars?)
    (ormap (lambda (info)
             (not (set-empty? (FunInfo-uses info))))
           fun-lst))

   ;; Returns a dictionary mapping Id -> (listof Id). A corresponding
   ;; fresh argument must be added for each of the listed free
   ;; variables, for each of the functions.
  (define (compute-extra-args)
    (let ((fun-ids (for/mutable-setId ((info fun-lst))
                     (FunInfo-id info))))
      (for ((info fun-lst))        
        ;; Restrict examination to local calls.
        (set-intersect! (FunInfo-calls info) fun-ids)

        ;; Remove "self" calls, since no additional information to be
        ;; found from there.
        (set-remove! (FunInfo-calls info) (FunInfo-id info))

        ;; Arguments are also local variables.
        (set-union! (FunInfo-vars info) (FunInfo-args info))))
        
    (define id-ranks ;; used for deterministic ordering of new args
      (for/mutable-hashId ([id seen-use-lst]
                           [j (in-naturals)])
        (values id j)))

    (define (sort-Ids r) ;; (-> set list)
      (map cdr
           (sort
            (for/list ((id (in-set r)))
              (cons (dict-ref id-ranks id) id))
            < #:key car)))

    (define fun-h ;; function Id -> FunInfo
      (for/mutable-hashId ((info fun-lst))
        (values (FunInfo-id info) info)))
      
    (define work-lst
      (for/list ((info fun-lst))
        (define self-uses (FunInfo-uses info))
        (define call-uses-lst
          (for/list ((id (in-set (FunInfo-calls info))))
            (FunInfo-uses (dict-ref fun-h id))))
        (list (FunInfo-id info) self-uses
              call-uses-lst (FunInfo-vars info))))
    (let loop ()
      (let ((progress? #f))
        (for ((item work-lst))
          (match-define (list id self-uses call-uses-lst self-vars) item)
          (define count (set-count self-uses))
          (apply set-union! self-uses call-uses-lst)
          ;; What is free in a called function may not be free here.
          (set-subtract! self-uses self-vars)
          (when (> (set-count self-uses) count)
            (set! progress? #t)))
        (when progress?
          (loop))))

    (define extra-args
      (for/mutable-hashId ([item work-lst])
        (match-define (list id self-uses call-uses-lst self-vars) item)
        (values id (sort-Ids self-uses))))

    extra-args) ;; end compute-extra-args

  (define (rm-free-vars free-vars)
    (define (rw ast)
      (match ast
        [(Defun a id t ps b)
         (define fv-lst (dict-ref free-vars id))
         (define fv->xp (mutable-hashId))
         (define x-ps
           (for/list ((fv fv-lst))
             (define fv-t (dict-ref types-h fv))
             (define x-id (another-Id fv))
             (dict-set! fv->xp fv x-id)
             (annoless Param x-id fv-t)))
         (parameterize ((this (cons id fv->xp)))
           (define ats (FunT-ats t))
           (define n-t (set-FunT-ats t (append ats (map Param-t x-ps))))
           (define n-ps (append ps x-ps))
           (Defun a id n-t n-ps (rw b)))]
        [(Var a id)
         (define n-id (dict-ref (cdr (this)) id #f))
         (if n-id (Var a n-id) ast)]
        [(ApplyExpr _ (Var _ f-id) es) (=> fail)
         ;; `x-ids` will be #f for another top-level function
         (define x-ids (dict-ref free-vars f-id #f))
         (when (or (not x-ids) (null? x-ids))
           (fail))
         (define x-es
           (for/list ((fv-id x-ids))
             (define a-t (dict-ref types-h fv-id))
             (define fv->xp (cdr (this)))
             ;; `fv-id` may not be free in current context,
             ;; hence the default
             (define a-id (dict-ref fv->xp fv-id fv-id))
             (Var (hasheq 'type a-t) a-id)))
         (set-ApplyExpr-args ast (append (map rw es) x-es))]
        [(AssignStxp _ (Var _ id) _)
         #:when (dict-has-key? (cdr (this)) id)
         (raise-language-error/ast
          "free variable not allowed as an L-value"
          ast (AssignStxp-lv ast))]
        [_
         (term-rewrite-all rw ast)]))

    (rw def))

  (let ()
    (find-uses)
    (cond
      [(not (any-free-vars?))
       ;; Optimize for the case where there are none.
       def]
      [else
       ;;(pretty-print fun-lst)
       (define free-vars (compute-extra-args))
       ;;(pretty-print free-vars)
       (rm-free-vars free-vars)])))
  
(define-with-contract
  (-> (listof Def?) hash?)
  (defs-lift-local-Defuns def-lst)
  
  (define n-defs (make-hasheq))
  (define owner-id (make-parameter #f))

  (define rw-body
    (topdown
     (lambda (ast)
       (cond
        [(and (LetExpr? ast) (Defun? (LetExpr-def ast)))
         (match-define (LetExpr a b ss) ast)
         (do-Defun b)
         (SeqExpr a ss)]
        [else
         ast]))))
  
  (define (do-Defun ast)
    (match-define (Defun a id t ps b) ast)
    (define oid (owner-id))
    (unless (Id-bind=? id oid)
      (set! a (hash-set a 'owner-id oid)))
    (set! b (ast-splice-SeqExpr (rw-body b)))
    (hash-set! n-defs (Id-bind id) (Defun a id t ps b)))
  
  (for ([def def-lst])
    (cond
     [(Defun? def)
      (when (ast-anno-maybe def 'top)
        (parameterize ((owner-id (Def-id def)))
          (do-Defun (fun-rm-free-vars def))))]
     [else
      (hash-set! n-defs (Id-bind (Def-id def)) def)]))
  
  n-defs)

;;;
;;; program contents resolution
;;;

;; Compilation state. [defs (listof Def?)] lists top-level
;; definitions. [eps (set/c symbol? #:cmp 'eq)] contains bind symbols
;; for program entry points.
(struct St (defs eps) #:transparent)

(define* compilation-state? St?)

;; Merges the definitions of the specified modules `mods`, which is a
;; (hash/c rr-mp Mod). Returns the definitions for the whole program
;; as (values def-lst eps-in-prog prelude-sym->bind).
(define-with-contract
  (-> hash? (values (listof Def?) (set/c symbol? #:cmp 'eq) hash?))
  (merge-defs mods)

  (define eps-in-prog (mutable-seteq)) ;; of bind
  (define all-defs (make-hasheq)) ;; bind -> Def
  (define next-r #hasheq())

  ;; Maps each binding's original binding site to that binding's
  ;; whole-program `bind` value (as assigned here).
  (define x-binds (make-hash)) ;; (cons/c rr-mp sym) -> bind

  ;; Maps each appearing core binding's whole-program `bind` value (as
  ;; assigned here) to its IR bind value. The map makes it possible to
  ;; switch the definitions over to said known IR bind values. Not all
  ;; the compiler-known names are required to be declared in prelude
  ;; modules, except if they are actually used, and code is to be
  ;; generated for such uses.
  (define prelude-bind->bind (make-hasheq)) ;; bind -> sym

  (for ([(rr-mp/mgl mod) mods])
    (define def-lst (Mod-def-lst mod))
    ;;(pretty-print `(before-merge-of ,rr-mp/mgl : ,(map Def-id def-lst)))
    (define bind->binding (Mod-bind->binding mod))
    ;;(pretty-print `(,rr-mp/mgl ,(Mod-r-mp mod) bind->binding ,bind->binding))
    (define m->p-bind (make-hasheq)) ;; local bind -> global bind

    (define (rw-id id)
      (define m-bind (Id-bind id))
      (define info (hash-ref bind->binding m-bind))
      (cond-or-fail
       [(list? info)
        (define r-mp (first info))
        (define sym (second info))
        (define rr-mp/bind (r-mp->rr-mp r-mp))
        (define p-bind (hash-ref m->p-bind m-bind #f))
        (unless p-bind
          (define mp-and-sym (cons rr-mp/bind sym))
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
        (define id (Def-id n-def))
        (define bind (Id-bind id))
        (when (hash-has-key? all-defs bind)
          (error 'merge-defs
                 "redefinition for `~a`: ~s"
                 (Id-name id) n-def))
        (hash-set! all-defs bind n-def)
        n-def))
    ;;(pretty-print `(,rr-mp/mgl x-binds ,x-binds m->p-bind ,m->p-bind))

    (for ([(sym m-bind) (Mod-core->bind mod)])
      (define p-bind (hash-ref m->p-bind m-bind))
      (hash-set! prelude-bind->bind p-bind sym))

    (when (Mod-ep? mod)
      (for ([def n-def-lst]
            #:when (ast-anno-maybe def 'export))
        (define bind (Id-bind (Def-id def)))
        (set-add! eps-in-prog bind)))

    (void)) ;; end (for ([(rr-mp/mgl mod) mods])

  ;;(pretty-print `(after-merge ,(map Def-id (hash-values all-defs))))
  (values (hash-values all-defs) eps-in-prog prelude-bind->bind))

;; Returns a list of all Id-bind's appearing within a Def.
(define-with-contract
  (-> Def? (listof symbol?))
  (def-all-used-id-binds def)

  (define binds (mutable-seteq)) ;; (set/c bind)

  (define rw
    (topdown-visitor
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

  (define globals (build-tl-defs-table tl-def-lst)) ;; bind -> Def?
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
          (set-Expr-type ast the-Bool-type))
         (_ ast)))))
  (rw ast))

;; Applies the identifier mappings specified by `prelude-bind->bind`,
;; which maps bind values to bind values. Modifies mutable set
;; `eps-in-prog` in place. Returns a modified copy of `def-lst`.
(define (switch-ids-for-builtins! def-lst eps-in-prog prelude-bind->bind)
  (for ([(k v) prelude-bind->bind])
    (when (set-member? eps-in-prog k)
      (set-remove! eps-in-prog k)
      (set-add! eps-in-prog v)))

  (define (rw-id id)
    (define bind (Id-bind id))
    (define v (hash-ref prelude-bind->bind bind #f))
    (if (not v)
        id
        (set-Id-bind id v)))

  (define rw (fix ast-rw-Ids rw-id))

  (map rw def-lst))

;;;
;;; compilation
;;;

(define (load-program ep-mp-lst rel-to-path-v)
  ;; resolved-module-path? is eq? comparable
  (define mods (make-hasheq)) ;; rr-mp -> Mod

  (struct Dep (ep? prelude? mp rel) #:transparent)

  ;; Dependencies queued for loading
  (define dep-q null) ;; (listof Dep?)

  (define (load ep? prelude? mp rel-to-path-v)
    ;;(writeln `(load mp ,mp))
    (define r-mp (resolve-module-path/primitive mp rel-to-path-v))
    ;;(writeln `(load r-mp ,r-mp))
    (define rr-mp (r-mp->rr-mp r-mp)) ;; resolved-module-path?
    ;;(writeln `(load rr-mp ,rr-mp))
    ;;(writeln `(load entry: ,ep? mp: ,mp rel: ,rel-to-path-v r-mp: ,r-mp rr-mp: ,rr-mp))
    (define mod (hash-ref mods rr-mp #f))

    (unless mod ;; not yet loaded
      ;;(writeln (list 'loading-submod-of r-mp mp))
      (set! mod (Mod-load r-mp mp))
      ;;(writeln `(LOADED ,rr-mp ,r-mp ,mp ,mod))

      (define def-lst (Mod-def-lst mod))

      ;; Queue all runtime libraries for loading. They may be
      ;; dependencies during compilation even if they are not for
      ;; Racket VM execution.
      (set! dep-q (append
                   (for/list ((mp (Mod-prelude-lst mod)))
                     (define dep (Dep #f #t mp r-mp))
                     ;;(writeln `(queuing prelude ,dep))
                     dep)
                   dep-q))

      ;; Build a list of dependencies for this module from the
      ;; bind->binding table. Stored as (list dep-r-mp rel-r-mp) per
      ;; entry.
      (for ([(bind info) (Mod-bind->binding mod)]
            #:when (list? info))
        (define dep-r-mp (first info))
        (define dep-mp (r-mp->mp dep-r-mp))
        (define dep (Dep #f #f dep-mp r-mp))
        ;;(writeln `(queuing dependency ,dep))
        (set! dep-q (cons dep dep-q)))

      (hash-set! mods rr-mp mod))

    (when ep?
      (define h (Mod-attrs mod))
      (hash-set! h 'ep? #t))
    (when prelude?
      (define h (Mod-attrs mod))
      (hash-set! h 'prelude? #t))

    mod)

  ;; Load all the "entry" modules.
  (for ([mp ep-mp-lst])
    (load #t #f mp rel-to-path-v))

  ;; Keep loading dependencies until all loaded.
  (let loop ()
    (unless (null? dep-q)
      (define lst dep-q)
      (set! dep-q null)
      (for ([dep lst])
        (match-define (Dep ep? prelude? mp rel) dep)
        (load ep? prelude? mp rel))
      (loop)))

  mods)

;; Compiles a program consisting of all the entry points in the
;; specified modules, and all dependencies thereof. All of the
;; 'ep-mp-lst' module paths should be either absolute ones, or '(file
;; ...)' paths relative to the working directory, unless
;; 'rel-to-path-v' is specified for relative path resolution.
(define* (compile-modules
          #:relative-to [rel-to-path-v #f]
          . ep-mp-lst)
  (define mods ;; rr-mp -> Mod
    (load-program ep-mp-lst rel-to-path-v))
  ;;(writeln `(loaded mods ,(hash-keys mods)))
  ;;(pretty-print `(loaded modules ,mods)) (exit)
  ;;(displayln 'ast-after-marshaling) (for ([(x mod) mods]) (for ([def (Mod-def-lst mod)]) (ast-dump-loc-info def)))

  (set! mods (mods-rm-AnnoExpr mods))
  ;;(pretty-print mods) (exit)
  (define-values (def-lst eps-in-prog prelude-bind->bind)
    (merge-defs mods))

  (set! def-lst
        (switch-ids-for-builtins! def-lst eps-in-prog
                                  prelude-bind->bind))

  (set! def-lst (map ast-rm-ExistsT def-lst))
  (set! def-lst (map ast-rm-Begin0 def-lst))
  ;;(pretty-print def-lst)
  (set! def-lst (defs-optimize-if def-lst))
  ;;(pretty-print def-lst)
  (set! def-lst (map ast-add-prelude-lit-types def-lst))
  ;;(pretty-print `(prelude-map ,prelude-bind->bind defs ,def-lst eps ,eps-in-prog)) (exit)
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map ast-trim-useless-constants def-lst))
  (set! def-lst (map ast-rm-useless-conds def-lst))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (defs-make-Defuns def-lst))
  (set! def-lst (map def-drop-unused-local-Defuns def-lst))
  (defs-check-ApplyExpr-target def-lst)
  (set! def-lst (defs-lift-typedefs def-lst))
  (set! def-lst (defs-drop-unreachable def-lst eps-in-prog))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map ast-update-ExprLike-result-annos def-lst))
  ;;(pretty-print def-lst)

  (let ((def-h (build-full-defs-table def-lst)))
    (set! def-h (defs-type-infer def-h))
    (set! def-lst (map ast-rm-result-discarded-constants (hash-values def-h)))
    (set! def-lst (defs-set-formats-to-Literals def-h def-lst))
    (set! def-h (defs-lift-local-Defuns def-lst)) ;; (hash/c bind Def)
    (set! def-lst (hash-values def-h))
    (set! def-lst (filter
                   (lambda (def) (any-pred-holds Defun? ForeignTypeDecl? def))
                   def-lst)))

  (St def-lst eps-in-prog))

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
;;; private APIs
;;;

(define-with-contract*
  (-> St? (or/c #f syntax?))
  (get-expected-anno-value st)

  (define def-lst (St-defs st))

  (let/ec k
    (for ([def def-lst])
      (define v (ast-anno-maybe def 'expected))
      (when v
        (k v)))
    #f))

;;;
;;; code generation
;;;

(define (string-file-id? s)
  (regexp-match? #rx"^[a-zA-Z0-9_][a-zA-Z0-9_-]*$" s))

(define-with-contract*
  (->* (St? (listof (cons/c symbol? any/c)))
       (#:outdir path-string?
        #:basename string?
        #:out (or/c #f output-port?)
        #:dont-touch boolean?
        #:banner boolean?)
       void?)
  (generate-files st backends
                  #:outdir [outdir (current-directory)]
                  #:basename [basename "output"]
                  #:out [out (current-output-port)]
                  #:dont-touch [dont-touch? #f]
                  #:banner [banner? #t])

  (unless (string-file-id? basename)
    (raise-argument-error
     'generate-files
     "file basename of non-zero length, without exotic characters"
     basename))

  (parameterize ([dont-touch-generated-file? dont-touch?])
    (when-let spec (assq 'mgl backends)
      (define def-lst (St-defs st))
      (define mgl-file (build-path outdir
                                   (string-append basename ".ir.rkt")))
      (generate-mgl-file spec def-lst out mgl-file banner?))

    (when-let spec (assq 'cxx backends)
      (define def-lst (St-defs st))
      (define path-stem (build-path outdir basename))
      (generate-cxx-file spec def-lst path-stem out banner?))

    (when-let spec (assq 'build backends)
      (define def-lst (St-defs st))
      (define opts-stx (defs-collect-build-annos def-lst))
      (define opts-lst (parse-analyze-build-annos opts-stx))
      (define path-stem (build-path outdir (string-append basename "_build")))
      (generate-build-file spec opts-lst path-stem out banner?)))

  (void))

;;;
;;; testing
;;;

(module* test #f
  (define st (compile-files "tests/test-locals-3.rkt"))
  (generate-files st '(
                       ;;(build (targets gnu-make qmake c ruby))
                       (cxx (parts cc hh))
                       )))
