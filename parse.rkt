#lang racket/base

#|

Note that language not in the runtime library may appear. Such
language may come from the lexical cope of 'expand' itself, or from
the lexical scope of a macro expander. But what we are parsing here is
fully expanded, and it is an error if there are any references to
functions in the Racket runtime, unless we happen to explicitly
support such things.

Note that #%app and lambda in 'racket' are macros that expand to
primitives, probably ones in '#%kernel, and this is something to keep
in mind when comparing identifiers. The 'syntax/kerncase' module and
particularly kernel-syntax-case should be of interest here.

We would of course be able to manually import '#%kernel versions of
constructs, e.g.

  (only-in '#%kernel [#%app k-app] [lambda k-lambda])

We collect some information about all bindings. Really only those ones
that are top-level might have to be translated into C++. Although some
locals might have to be lifted to the top-level in C++.

We drop all #%require and #%provide directives, anything within
begin-for-syntax, any submodules, and also all top-level expressions.

identifier-binding may be useful for getting information about IDs. It
will tell us if an ID is local, module-level, or something else. For
module bindings it also tells us the original name, plus the defining
module.

Note that letrec-syntaxes+values does not appear in fully expanded
programs, but it can appear in the result of a local-expand, which
means that it can appear here. We can just translate to a kernel
letrec-syntaxes, although that may not quite correspond to what Racket
would have done. Still retains correct scoping and evaluation order.

|#

(require "annos-parse.rkt"
         "annos-util.rkt"
         "ast-magnolisp.rkt"
         "compiler-util.rkt"
         (only-in "runtime.rkt" %core)
         (rename-in "strategy.rkt" [id id-rw])
         "util.rkt"
         "util/case.rkt"
         racket/contract
         racket/dict
         racket/function
         racket/list
         racket/pretty
         syntax/id-table
         syntax/kerncase
         syntax/parse
         syntax/stx)

;;; 
;;; debugging utilities
;;; 

(define (print-stx-with-bindings stx)
  (define lst (syntax->list stx))
  (cond
   (lst (for-each print-stx-with-bindings lst))
   ((identifier? stx) (writeln (list stx (identifier-binding stx))))
   (else (writeln stx))))

(define (stx-print-if-type-annoed stx)
  (define annos (syntax-get-annos stx))
  (when annos
    (define type (hash-ref annos 'type #f))
    (when type
      (writeln (list 'TYPED stx type)))))

(define (stx-print-all-type-annoed stx)
  (stx-print-if-type-annoed stx)
  (define lst (syntax->list stx))
  (when lst
   (for-each stx-print-all-type-annoed lst)))

(define (find-id-stx find-stx stx (msg #f))
  (define (f stx)
    (define lst (syntax->list stx))
    (cond
     (lst
      (for-each f lst))
     ((identifier? stx)
      (when (free-identifier=? find-stx stx)
        (writeln (list (or msg (format "~a" (syntax->datum stx)))
                       stx (identifier-binding stx)))))))
  (f stx))

(define (stx-binding-info stx)
  (define p (syntax-e stx))
  (and (pair? p)
       (let ((id (car p)))
         (and (identifier? id)
              (identifier-binding id)))))

;;; 
;;; parsing
;;; 

;; Reference: Typed Racket implementation of same.
(define (resolve-provides prov-lst)
  (define provide-tbl
    (for/fold
        ([h (make-immutable-free-id-table #:phase 0)])
        ([p (in-list prov-lst)])
      (syntax-parse p
        [in-out:id
         (dict-update h #'in-out (fix cons #'in-out) null)]
        [((~datum rename) in out)
         (dict-update h #'in (fix cons #'out) null)]
        [_
         (error 'resolve-provides
                "unsupported #%provide form: ~s" p)])))
  provide-tbl)

(define-syntax-class phase-level
  (pattern lv:exact-integer)
  (pattern (~and lv (~datum #f))))

(define (for-runtime-require? stx)
  ;; 'just-meta' forms cannot nest, and the other raw-require-spec
  ;; non-terminals can only contain phaseless-spec non-terminals.
  ;; Also, presumably all of the 'for-*' forms have been translated to
  ;; 'just-meta' forms in full expansion. See documentation for the
  ;; #%require form.
  (syntax-parse stx
    (((~or (~datum for-syntax) (~datum for-template)
           (~datum for-label)) . _)
     #f)
    (((~or (~datum for-meta) (~datum just-meta))
      lv:phase-level . _)
     (equal? (syntax-e #'lv.lv) 0))
    (_
     #t)))

;; Assumes that 'req-lst' has raw-require-spec syntax objects.
;; Produces a list of syntax for raw-module-path forms.
(define-with-contract*
  (-> (listof syntax?) (listof syntax?))
  (req-specs->module-paths req-lst)
  (define require-lst null)
  (define (raw-module-path! mp)
    (set! require-lst (cons mp require-lst)))
  ;; We would be able to quite easily pass around the phase level
  ;; (recursively), and support all the raw-require-spec forms.
  (define (parse-raw-require-spec stx)
    (syntax-parse stx
      [((~or (~datum for-syntax) (~datum for-template)
             (~datum for-label) (~datum for-meta)) . _)
       (unsupported stx)]
      [((~datum just-meta) lv:phase-level . specs)
       (for ((spec (syntax->list #'specs)))
         (parse-raw-require-spec spec))]
      [phaseless-spec
       (parse-phaseless-spec #'phaseless-spec)]))
  (define (parse-phaseless-spec stx)
    (syntax-parse stx
      [((~datum only) raw-mp . _)
       (raw-module-path! #'raw-mp)]
      [((~datum prefix) pfx raw-mp)
       (raw-module-path! #'raw-mp)]
      [((~datum all-except) raw-mp . _)
       (raw-module-path! #'raw-mp)]
      [((~datum prefix-all-except) pfx raw-mp . _)
       (raw-module-path! #'raw-mp)]
      [((~datum rename) raw-mp . _)
       (raw-module-path! #'raw-mp)]
      [raw-mp
       (raw-module-path! #'raw-mp)]))
  (for ((stx req-lst))
    (parse-raw-require-spec stx))
  (reverse require-lst))

;; Parses passed require specs, returning a hash. The hash maps each
;; module path into a list of elements, each of which is one of: 'all,
;; (listof ID), or (cons/c local-ID exported-ID).
(define-with-contract*
  (-> (listof syntax?) hash?)
  (req-specs->reqs-per-mp req-lst)
  (define raw-mp-h (make-hash))
  (define (add! mp x)
    (hash-update! raw-mp-h mp (fix cons x) null))
  (define (parse-phaseless-spec stx)
    (syntax-parse stx
      [((~datum only) raw-mp . ids)
       (define id-lst (syntax->list #'ids))
       (unless (null? id-lst)
         (add! (syntax->datum #'raw-mp) id-lst))]
      [((~datum rename) raw-mp local-id exported-id)
       (add! (syntax->datum #'raw-mp)
             (cons #'local-id #'exported-id))]
      [((~datum prefix) pfx raw-mp)
       (unsupported stx)]
      [((~datum all-except) raw-mp . _)
       (unsupported stx)]
      [((~datum prefix-all-except) pfx raw-mp . _)
       (unsupported stx)]
      [raw-mp
       (add! (syntax->datum #'raw-mp) 'all)]))
  (define (parse-raw-require-spec stx)
    (syntax-parse stx
      [((~or (~datum for-syntax) (~datum for-template)
             (~datum for-label) (~datum for-meta)) . _)
       (unsupported stx)]
      [((~datum just-meta) lv:phase-level . specs)
       (assert (equal? (syntax-e #'lv) 0))
       (for ((spec (syntax->list #'specs)))
         (parse-raw-require-spec spec))]
      [phaseless-spec
       (parse-phaseless-spec #'phaseless-spec)]))
  (for ((stx req-lst))
    (parse-raw-require-spec stx))
  raw-mp-h)

(define (core-id? x)
  (matches-global-id? #'%core x))

(define-syntax-class core-id
  (pattern x #:when (core-id? #'x)))

(define (quote? x)
  (matches-global-id? #'quote x))

;; Returns defs, provides, and requires in module.
(define-with-contract*
  (-> syntax? immutable-id-table? resolve-module-path-result?
      (values immutable-id-table? immutable-id-table? (listof syntax?)))
  (parse-defs-from-module modbeg-stx annos r-mp)

  (define defs-in-mod (make-immutable-free-id-table #:phase 0))
  (define prov-lst null)
  (define req-lst null)

  (define (get-def-in-mod id)
    (dict-ref defs-in-mod id #f))

  (define (set-def-in-mod! id def)
    (set! defs-in-mod (dict-set defs-in-mod id def)))
  
  (define (provide! stx-lst)
    ;;(pretty-print (list r-mp 'provide! stx-lst))
    (set! prov-lst (append prov-lst stx-lst)))

  ;; Records #%require specs, which may look like:
  ;;   (just-meta 0 (rename "test-6-lib.rkt" h six))
  ;;   (just-meta 0 (rename "test-6-lib.rkt" seven seven))
  ;;   (only "test-6-lib.rkt")
  (define (require! stx-lst)
    ;;(for-each (compose writeln syntax->datum) stx-lst)
    (set! req-lst
          (append req-lst
                  (filter for-runtime-require? stx-lst))))
  
  (define (not-magnolisp stx)
    (error 'parse-defs-from-module "not Magnolisp: ~a" stx))

  (define (redefinition id old-def new-stx)
    (error 'parse-defs-from-module
           "redefinition of ~a: ~a and ~a"
           id (ast-anno-must old-def 'stx) new-stx))

  (define (check-redefinition id new-stx)
    (when-let old-def (get-def-in-mod id)
              (redefinition id old-def new-stx)))

  (define (mk-annos ctx stx id-stx)
    (define global? (eq? ctx 'module-level))
    (define ann-h (dict-ref annos id-stx #hasheq()))
    ;;(writeln (list 'annos ann-h (hash? ann-h) (immutable? ann-h)))
    (set! ann-h (hash-set* ann-h 'stx stx 'r-mp r-mp 'top global?))
    ;;(writeln `(annos for ,id-stx ,(hash-count ann-h)))
    ann-h)

  (define (lookup-type id-stx)
    (define (f)
      (define anno-h (dict-ref annos id-stx #f))
      (and anno-h
           (let ((type-stx (hash-ref anno-h 'type #f)))
             (and type-stx
                  (parse-type type-stx)))))
    (or (f) the-AnyT))

  (define (maybe-parse-foreign-anno id-stx ann-h)
    (define foreign-stx (hash-ref ann-h 'foreign #f))
    (when foreign-stx
      (define foreign-name (parse-cxx-name-anno foreign-stx))
      (when foreign-name
        (set! ann-h (hash-set ann-h 'foreign foreign-name))))
    ann-h)
  
  (define (make-DefVar ctx stx id-stx e-stx)
    (check-redefinition id-stx stx)
    (define ast (parse 'expr e-stx))
    (define ann-h (mk-annos ctx stx id-stx))
    (set! ann-h (maybe-parse-foreign-anno id-stx ann-h))
    (define t (lookup-type id-stx))
    (define def (DefVar ann-h id-stx t ast))
    (set-def-in-mod! id-stx def)
    def)

  (define (make-DefStx ctx stx id-stx)
    (check-redefinition id-stx stx)
    (define ann-h (mk-annos ctx stx id-stx))
    (define def (DefStx ann-h id-stx))
    (set-def-in-mod! id-stx def)
    def)

  (define (make-ForeignTypeDecl ctx stx id-stx)
    (check-redefinition id-stx stx)
    (define id-annos (dict-ref annos id-stx #f))
    (define foreign-stx (and id-annos (hash-ref id-annos 'foreign #f)))
    (unless foreign-stx
      (raise-language-error
       #f
       "missing 'foreign' C++ type annotation"
       stx))
    (define cxx-t (parse-cxx-type id-stx foreign-stx))
    (define ann-h (mk-annos ctx stx id-stx))
    (define def (ForeignTypeDecl ann-h id-stx cxx-t))
    (set-def-in-mod! id-stx def)
    def)
  
  (define (make-Param ctx stx id-stx)
    (check-redefinition id-stx stx)
    (define ann-h (mk-annos ctx stx id-stx))
    (define def (Param ann-h id-stx the-AnyT))
    (set-def-in-mod! id-stx def)
    def)
  
  (define (make-Let ctx stx kind binds-stx exprs-stx)
    (define i-e-lst (syntax->list binds-stx))
    (define b-ast-lst (map
                       (lambda (i-e)
                         (syntax-case i-e ()
                           ;; TODO multiple (or zero) binding case
                           (((id) e)
                            (identifier? #'id)
                            (make-DefVar ctx stx #'id #'e))
                           (_ (unsupported i-e))))
                       i-e-lst))
    (define e-stx-lst (syntax->list exprs-stx))
    (define e-ast-lst (map (fix parse 'stat) e-stx-lst))
    (Let (hasheq 'stx stx 'let-kind kind) b-ast-lst e-ast-lst))

  (define (parse-expr-annos stx)
    (define as (syntax-get-annos stx))
    (define t (hash-ref as 'type #f))
    (define p-as #hasheq())
    (when t
      (define t-ast (parse-type t))
      (writeln `(parsed type ,t-ast))
      (set! p-as (hash-set p-as 'type t-ast)))
    p-as)
  
  (define (make-Literal stx datum-stx)
    (define ann-h (parse-expr-annos datum-stx))
    (set! ann-h (hash-set ann-h 'stx stx))
    (Literal ann-h datum-stx))
  
  (define (parse-define-value ctx stx id-stx e-stx)
    ;;(writeln (list e-stx (syntax->datum e-stx)))
    ;;(writeln (identifier-binding #'%core 0))
    (kernel-syntax-case e-stx #f
      ((#%plain-app c (q k))
       (and (core-id? #'c)
            (quote? #'q)
            (eq? 'foreign-type (syntax-e #'k)))
       (make-ForeignTypeDecl ctx stx id-stx))
      (_
       (make-DefVar ctx stx id-stx e-stx))))
  
  ;; 'ctx' is a symbolic name of the context that the 'stx' being
  ;; parsed is in. Inserts bindings into 'defs-in-mod' as a side effect.
  ;; Returns an Ast object for non top-level things.
  (define (parse ctx stx)
    ;;(stx-print-if-type-annoed stx)
    ;;(writeln (list ctx stx))
    
    (kernel-syntax-case* stx #f (%core)

      ((#%module-begin . bs)
       (eq? ctx 'module-begin)
       (for-each (fix parse 'module-level)
                 (syntax->list #'bs)))

      ;; top-level-form non-terminal
      
      ((#%expression _)
       (eq? ctx 'module-level)
       (void))

      ((module . _)
       (eq? ctx 'module-level)
       (void))

      ((begin . bs)
       (eq? ctx 'stat)
       (syntaxed BlockStat stx
                 (map (fix parse ctx)
                      (syntax->list #'bs))))
      
      ((begin-for-syntax . _)
       (eq? ctx 'module-level)
       (void))

      ;; module-level-form non-terminal

      ((#%provide . specs)
       (eq? ctx 'module-level)
       (provide! (syntax->list #'specs)))

      ;; submodule-form non-terminal

      ((module* . _)
       (eq? ctx 'module-level)
       (void))
      
      ;; general-top-level-form non-terminal

      ;; TODO multiple (or zero) binding case (in the general case)
      ((define-values (id) e)
       (begin
         (assert (identifier? #'id))
         (parse-define-value ctx stx #'id #'e)))

      ;; We only support splitting of module top-level define-values,
      ;; for now. We do not support any top-level computation, so we
      ;; expect a direct (values v ...) expression.
      ((define-values (id ...) e)
       (eq? ctx 'module-level)
       (let ()
         (define ids (syntax->list #'(id ...)))
         (unless (null? ids) ;; (void) result otherwise
           (assert (> (length ids) 1))
           (define e-stx #'e)
           (kernel-syntax-case e-stx #f
             ((#%plain-app values v ...)
              (let ()
                (define vs (syntax->list #'(v ...)))
                (unless (= (length vs) (length ids))
                  (raise-language-error
                   #f
                   (format "expected ~a values" (length ids))
                   stx e-stx
                   #:continued not-magnolisp-message))
                (define def-lst
                  (map
                   (lambda (id-stx v-stx)
                     (syntax-track-origin
                      (quasisyntax/loc stx
                        (define-values (#,id-stx) #,v-stx))
                      stx (car (syntax-e stx))))
                   ids vs))
                (parse
                 ctx
                 (quasisyntax/loc stx
                   (begin #,@def-lst)))))
             (_
              (raise-language-error
               #f "expected (values v ...) expression"
               stx e-stx
               #:continued not-magnolisp-message))))))
      
      ((define-syntaxes (id ...) _)
       (begin
         (assert (eq? ctx 'module-level))
         (define id-lst (syntax->list #'(id ...)))
         (assert (andmap identifier? id-lst))
         (for ((id id-lst))
           (make-DefStx ctx stx id))))

      ((#%require . specs)
       (eq? ctx 'module-level)
       (require! (syntax->list #'specs)))

      ;; expr non-terminal

      ((#%plain-lambda formals . exprs)
       (when (eq? ctx 'expr)
         (define par-id-lst (syntax->list #'formals))
         (define e-stx-lst (syntax->list #'exprs))
         (when (> (length e-stx-lst) 1)
           (raise-syntax-error
            #f
            "function body must be a single expression"
            stx))
         (define par-ast-lst
           (map (lambda (id)
                  ;; Annotations would probably have to be propagated
                  ;; from any binding whose value this lambda is, but
                  ;; that must wait until later.
                  (make-Param ctx stx id))
                par-id-lst))
         (define e-stx (first e-stx-lst))
         (define e-ast (parse 'expr e-stx))
         (new-Lambda stx par-ast-lst e-ast)))
      
      ((if c t e)
       (when (eq? ctx 'expr)
         (new-IfExpr stx
                     (parse ctx #'c)
                     (parse ctx #'t)
                     (parse ctx #'e))))

      ((#%plain-app f e ...)
       (and (eq? ctx 'stat)
            (identifier? #'f)
            (or (free-identifier=? #'f #'void)
                (free-identifier=? #'f #'values)))
       (syntaxed stx Pass))

      ((#%plain-app kall
        (#%plain-lambda (k) b ...))
       (and (eq? ctx 'expr)
            (syntax-property stx 'local-ec)
            (identifier? #'kall)
            (free-identifier=? #'kall #'call/ec))
       (syntaxed stx
        LetLocalEc #'k
        (map
         (fix parse 'stat)
         (syntax->list #'(b ...)))))
      
      ((#%plain-app k e)
       (and (eq? ctx 'stat)
            (syntax-property stx 'local-ec)
            (identifier? #'k))
       (syntaxed stx
        AppLocalEc #'k (parse 'expr #'e)))
      
      ((#%plain-app p-expr . a-expr)
       (when (eq? ctx 'expr)
         (unless (identifier? #'p-expr)
           ;; No first-class functions in Magnolisp.
           (raise-syntax-error #f "expected identifier"
                               stx #'p-expr))
         (new-Apply
          stx
          (parse ctx #'p-expr)
          (map
           (fix parse ctx)
           (syntax->list #'a-expr)))))

      ((let-kw binds . exprs)
       (and (identifier? #'let-kw)
            (or (free-identifier=? #'let-kw #'let-values)
                (free-identifier=? #'let-kw #'letrec-values)))
       (unless (eq? ctx 'module-level)
         (when (eq? ctx 'expr)
           (raise-syntax-error #f "let form in expr context" stx))
         (make-Let ctx stx
                   (syntax-e #'let-kw) #'binds #'exprs)))

      ((set! id expr)
       (identifier? #'id)
       (when (eq? ctx 'expr)
         (new-Assign #'id (parse ctx #'expr))))

      ;; 'quote', as it comes in, appears to be unbound for us.
      ((q lit)
       (and (identifier? #'q) (module-or-top-identifier=? #'q #'quote))
       (when (eq? ctx 'expr)
         (make-Literal stx #'lit)))
      
      ((#%top . id) ;; module-level variable
       (identifier? #'id)
       (new-Var stx #'id))
      
      (id
       (identifier? #'id)
       (new-Var stx #'id))

      ((quote-syntax datum)
       (not-magnolisp stx))

      ((case-lambda (f b ...) ...)
       (not-magnolisp stx))

      ((begin0 v-e e ...)
       (not-magnolisp stx))
      
      ((with-continuation-mark expr1 expr2 expr3)
       (not-magnolisp stx))
      
      ((#%variable-reference (#%top . id))
       (not-magnolisp stx))
      
      ((#%variable-reference id)
       (not-magnolisp stx))
      
      ((#%variable-reference)
       (not-magnolisp stx))
      
      ;; These do appear as well.
      ((#%expression e)
       (eq? ctx 'expr)
       (parse ctx #'e))

      ;; local-expand result language
      
      ;; The letrec-syntaxes+values ID we get here is either top-level
      ;; or unbound, according to identifier-binding.
      ((let-kw _ v-binds body ...)
       (and (identifier? #'let-kw)
            (module-or-top-identifier=? #'let-kw #'letrec-syntaxes+values))
       (parse ctx
              (syntax-track-origin
               (syntax/loc stx (letrec-values v-binds body ...))
               stx #'let-kw)))

      ;; unrecognized language
      
      (_ (raise-language-error
          #f (format "unsupported syntax in ~a context" ctx) stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  (parse 'module-begin modbeg-stx)
  (define prov-h (resolve-provides prov-lst))
  ;;(pretty-print (dict-map prov-h list))
  (values defs-in-mod prov-h req-lst))
