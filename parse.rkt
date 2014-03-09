#lang racket/base

#|

|#

(require "annos-parse.rkt"
         "annos-util.rkt"
         "ast-magnolisp.rkt"
         "compiler-util.rkt"
         (only-in "runtime.rkt" #%magnolisp)
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
  (matches-global-id? #'#%magnolisp x))

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
  
  (define (not-magnolisp stx [sub-stx #f])
    (raise-language-error
     #f "syntax not supported in Magnolisp" stx sub-stx
     #:fields `(("binding"
                 ,(or (stx-binding-info stx) 'unbound)))))

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

  ;; Parses annotations specific to variable declarations. Also for
  ;; function declarations, in practice. Presently only 'foreign'
  ;; declarations (which make no sense for variables, and will be
  ;; ignored).
  (define (parse-add-DefVar-annos id-stx ann-h)
    (define foreign-stx (hash-ref ann-h 'foreign #f))
    (when foreign-stx
      (define foreign-name (parse-cxx-name-anno foreign-stx))
      (when foreign-name
        (set! ann-h (hash-set ann-h 'foreign foreign-name))))
    ann-h)
  
  (define (make-DefVar ctx stx id-stx e-stx)
    (check-redefinition id-stx stx)
    (define ast (parse-expr e-stx))
    (define ann-h (mk-annos ctx stx id-stx))
    (set! ann-h (parse-add-DefVar-annos id-stx ann-h))
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
  
  (define (make-Literal stx datum-stx)
    (syntaxed stx Literal datum-stx))
  
  (define (make-Let ctx stx kind binds-stx exprs-stx)
    (define i-e-lst (syntax->list binds-stx))
    (define b-ast-lst
      (apply append
             (for/list ((i-e i-e-lst))
               ;;(pretty-print `(parsing ,(syntax->datum i-e)))
               (kernel-syntax-case* i-e #f (values)
                 [((id ...) (#%plain-app values v ...))
                  (let ()
                    (define id-lst (syntax->list #'(id ...)))
                    (assert (andmap identifier? id-lst))
                    (define v-lst (syntax->list #'(v ...)))
                    (unless (= (length v-lst) (length id-lst))
                      (raise-language-error
                       #f
                       (format "expected ~a values" (length id-lst))
                       stx))
                    (map
                     (lambda (id-stx v-stx)
                       (make-DefVar ctx stx id-stx v-stx))
                     id-lst v-lst))]
                 [((id) v)
                  (identifier? #'id)
                  (list (make-DefVar ctx stx #'id #'v))]
                 [_
                  (not-magnolisp stx i-e)]))))
    (define e-stx-lst (syntax->list exprs-stx))
    (define e-ast-lst (map parse-stat e-stx-lst))
    (Let (hasheq 'stx stx 'let-kind kind) b-ast-lst e-ast-lst))

  (define (parse-define-value ctx stx id-stx e-stx)
    ;;(writeln (list 'parse-define-value e-stx (syntax->datum e-stx)))
    ;;(writeln (identifier-binding #'#%magnolisp 0))
    (kernel-syntax-case e-stx #f
      ((#%plain-app c (q k))
       (and (core-id? #'c)
            (quote? #'q)
            (eq? 'foreign-type (syntax-e #'k)))
       (make-ForeignTypeDecl ctx stx id-stx))
      (_
       (make-DefVar ctx stx id-stx e-stx))))

  (define (parse-module-begin stx)
    (kernel-syntax-case stx #f
      ((#%module-begin . bs)
       (for-each
        parse-module-level
        (syntax->list #'bs)))

      (_ (raise-language-error
          #f "illegal syntax for #%module-begin" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  (define (parse-module-level stx)
    (kernel-syntax-case* stx #f (values)
      (_
       (syntax-property stx 'in-racket)
       (void))

      ((begin-for-syntax . _)
       (void))

      ;; We support (values v ...) in an expression position only
      ;; directly in binding form value expressions.
      ((define-values (id ...) (#%plain-app values v ...))
       (let ()
         (define id-lst (syntax->list #'(id ...)))
         (assert (andmap identifier? id-lst))
         (define v-lst (syntax->list #'(v ...)))
         (unless (= (length v-lst) (length id-lst))
           (raise-language-error
            #f
            (format "expected ~a values" (length id-lst))
            stx))
         (define def-lst
           (map
            (lambda (id-stx v-stx)
              (syntax-track-origin
               (quasisyntax/loc stx
                 (define-values (#,id-stx) #,v-stx))
               stx (car (syntax-e stx))))
            id-lst v-lst))
         (for-each parse-module-level def-lst)))

      ;; Must come after the ((define-values (id ...) (#%plain-app
      ;; values v ...)) pattern.
      ((define-values (id) e)
       (identifier? #'id)
       (let ()
         ;;(writeln (syntax->datum stx))
         (parse-define-value 'module-level stx #'id #'e)))

      ((define-syntaxes (id ...) _)
       (let ()
         (define id-lst (syntax->list #'(id ...)))
         (assert (andmap identifier? id-lst))
         (for ((id id-lst))
           (make-DefStx 'module-level stx id))))

      ((#%provide . specs)
       (provide! (syntax->list #'specs)))
      
      ;; Local requires are not supported.
      ((#%require . specs)
       (require! (syntax->list #'specs)))
      
      ((module . _)
       (void))

      ((module* . _)
       (void))

      ((#%expression e)
       (void))
      
      ;; Any other module-level Racket expression is ignored. (These
      ;; are as for the Racket 'expr' non-terminal, in the same order,
      ;; too.) Note that we should only have (begin expr ...+) forms,
      ;; and not anything containing non-expressions, since those
      ;; should have gotten spliced into the module body.
      (id
       (identifier? #'id)
       (void))
      ((#%plain-lambda . _)
       (void))
      ((case-lambda . _)
       (void))
      ((if c t e)
       (void))
      ((begin . _)
       (void))
      ((begin0 . _)
       (void))
      ((let-kw binds . exprs)
       (and (identifier? #'let-kw)
            (or (free-identifier=? #'let-kw #'let-values)
                (free-identifier=? #'let-kw #'letrec-values)))
       (void))
      ((set! id expr)
       (identifier? #'id)
       (void))
      ((q lit)
       (and (identifier? #'q) (module-or-top-identifier=? #'q #'quote))
       (void))
      ((quote-syntax _)
       (void))
      ((with-continuation-mark expr1 expr2 expr3)
       (void))
      ((#%plain-app . _)
       (void))
      ((#%top . id) ;; should not appear in a module body
       (identifier? #'id)
       (void))
      ((#%variable-reference id)
       (identifier? #'id)
       (void))
      ((#%variable-reference (#%top . id))
       (identifier? #'id)
       (void))
      ((#%variable-reference)
       (void))

      ;; Not Racket core language, but may appear here.
      ((let-kw _ v-binds body ...)
       (and (identifier? #'let-kw)
            (module-or-top-identifier=? #'let-kw #'letrec-syntaxes+values))
       (void))
      
      (_ (raise-language-error
          #f "illegal syntax at module level" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))
  
  (define (parse-stat stx)
    (kernel-syntax-case* stx #f (values)
      (_
       (syntax-property stx 'in-racket)
       (syntaxed stx BlockStat null))
      
      ((begin . bs)
       (syntaxed stx BlockStat
                 (map parse-stat
                      (syntax->list #'bs))))
      
      ((#%expression e)
       (parse-stat #'e))
      
      ((if c t e)
       (syntaxed stx IfStat
                 (parse-expr #'c)
                 (parse-stat #'t)
                 (parse-stat #'e)))

      ((#%plain-app f . e)
       (and (identifier? #'f)
            (or (free-identifier=? #'f #'void)
                (free-identifier=? #'f #'values)))
       (begin
         (unless (null? (syntax-e #'e))
           (raise-syntax-error
            #f
            (format "arguments not allowed for ~a in a statement position"
                    (syntax-e #'f))
            stx #'e))
         (syntaxed stx BlockStat null)))

      ((#%plain-app k e)
       (and (syntax-property stx 'local-ec)
            (identifier? #'k))
       (syntaxed stx AppLocalEc #'k (parse-expr #'e)))
      
      ((let-kw binds . exprs)
       (and (identifier? #'let-kw)
            (or (free-identifier=? #'let-kw #'let-values)
                (free-identifier=? #'let-kw #'letrec-values)))
       (make-Let 'stat stx
                 (syntax-e #'let-kw) #'binds #'exprs))

      ((set! id expr)
       (identifier? #'id)
       (syntaxed stx Assign (parse-expr #'id) (parse-expr #'expr)))

      ;; The letrec-syntaxes+values ID we get here is either top-level
      ;; or unbound, according to identifier-binding. This form is
      ;; local-expand result language.
      ((let-kw _ v-binds body ...)
       (and (identifier? #'let-kw)
            (module-or-top-identifier=? #'let-kw #'letrec-syntaxes+values))
       (parse-stat
        (syntax-track-origin
         (syntax/loc stx (letrec-values v-binds body ...))
         stx #'let-kw)))

      (_ (raise-language-error
          #f "illegal syntax in statement context" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  ;; Inserts bindings into 'defs-in-mod' as a side effect. Returns an
  ;; Ast object for non top-level things.
  (define (parse-expr stx)
    ;;(stx-print-if-type-annoed stx)
    ;;(writeln (list ctx stx))
    
    (kernel-syntax-case* stx #f (values)

      (_
       (syntax-property stx 'in-racket)
       (syntaxed stx RacketExpr))
      
      ;; These do appear as well.
      ((#%expression e)
       (parse-expr #'e))

      ((#%plain-lambda formals . exprs)
       (let ()
         (define par-id-lst (syntax->list #'formals))
         (define e-stx-lst (syntax->list #'exprs))
         (when (> (length e-stx-lst) 1)
           (raise-syntax-error
            #f "function body must be a single expression" stx))
         (define par-ast-lst
           (map (lambda (id)
                  ;; Annotations would probably have to be propagated
                  ;; from any binding whose value this lambda is, but
                  ;; that must wait until later.
                  (make-Param 'expr stx id))
                par-id-lst))
         (define e-stx (first e-stx-lst))
         (define e-ast (parse-expr e-stx))
         (syntaxed stx Lambda par-ast-lst e-ast)))
      
      ((if c t e)
       (syntaxed stx IfExpr
                 (parse-expr #'c)
                 (parse-expr #'t)
                 (parse-expr #'e)))

      ((#%plain-app kall
        (#%plain-lambda (k) b ...))
       (and (syntax-property stx 'local-ec)
            (identifier? #'kall)
            (free-identifier=? #'kall #'call/ec))
       (syntaxed stx
        LetLocalEc #'k
        (map
         parse-stat
         (syntax->list #'(b ...)))))
      
      ((#%plain-app p-expr . a-expr)
       (let ()
         (unless (identifier? #'p-expr)
           ;; No first-class functions in Magnolisp.
           (raise-syntax-error #f "expected identifier"
                               stx #'p-expr))
         (syntaxed stx Apply
          (parse-expr #'p-expr)
          (map
           parse-expr
           (syntax->list #'a-expr)))))

      ((let-values (((n) v)) e)
       (identifier? #'n)
       (let ()
         (define d-ast (make-DefVar 'expr stx #'n #'v))
         (define e-ast (parse-expr #'e))
         (syntaxed stx LetExpr d-ast e-ast)))
       
      ;; 'quote', as it comes in, appears to be unbound for us.
      ((q lit)
       (and (identifier? #'q) (module-or-top-identifier=? #'q #'quote))
       (make-Literal stx #'lit))
      
      ((#%top . id) ;; module-level variable
       (identifier? #'id)
       (syntaxed stx Var #'id))
      
      ;; The letrec-syntaxes+values ID we get here is either top-level
      ;; or unbound, according to identifier-binding. This form is
      ;; local-expand result language.
      ((let-kw _ v-binds body ...)
       (and (identifier? #'let-kw)
            (module-or-top-identifier=? #'let-kw #'letrec-syntaxes+values))
       (parse-expr
        (syntax-track-origin
         (syntax/loc stx (letrec-values v-binds body ...))
         stx #'let-kw)))

      (id
       (identifier? #'id)
       (syntaxed stx Var #'id))
      
      (_ (raise-language-error
          #f "illegal syntax in expression context" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  ;;(print-with-select-syntax-properties '(in-racket local-ec) modbeg-stx)
  (parse-module-begin modbeg-stx)
  (define prov-h (resolve-provides prov-lst))
  ;;(pretty-print (dict-map prov-h list))
  (values defs-in-mod prov-h req-lst))
