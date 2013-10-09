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

(require "ast-magnolisp.rkt"
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
        ([h (make-immutable-free-id-table)])
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

(define-with-contract*
  (-> syntax? bound-id-table? resolve-module-path-result?
      (values bound-id-table? free-id-table? (listof syntax?)))
  (parse-defs-from-module modbeg-stx annos r-mp)

  (define defs (make-bound-id-table #:phase 0))
  (define prov-lst null)
  (define req-lst null)

  (define (provide! stx-lst)
    (set! prov-lst (append prov-lst stx-lst)))

  ;; Records #%require specs, which may look like:
  ;;   (just-meta 0 (rename "test-6-lib.rkt" h six))
  ;;   (just-meta 0 (rename "test-6-lib.rkt" seven seven))
  ;;   (only "test-6-lib.rkt")
  (define (require! stx-lst)
    ;;(for-each (compose writeln syntax->datum) stx-lst)
    (set! req-lst (append req-lst stx-lst)))
  
  (define (not-magnolisp stx)
    (error 'parse-defs-from-module "not Magnolisp: ~a" stx))

  (define (redefinition id old-def new-stx)
    (error 'parse-defs-from-module
           "redefinition of ~a: ~a and ~a"
           id (Ast-anno-ref old-def 'stx) new-stx))

  (define (check-redefinition id new-stx)
    (when-let old-def (bound-id-table-ref defs id #f)
              (redefinition id old-def new-stx)))

  (define (make-DefVar ctx stx id-stx e-stx)
    (check-redefinition id-stx stx)
    (define global? (eq? ctx 'top-level))
    (define ast (parse 'expr e-stx))
    (define ann-h (bound-id-table-ref annos id-stx #hasheq()))
    (set! ann-h (hash-set ann-h 'stx stx))
    (when global?
      (set! ann-h (hash-set ann-h 'r-mp r-mp)))
    (define def (DefVar ann-h id-stx ast))
    (bound-id-table-set! defs id-stx def)
    def)

  (define (make-Let ctx stx
                    ctor binds-stx exprs-stx)
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
    (define e-ast-lst (map (fix parse ctx) e-stx-lst))
    (ctor stx b-ast-lst e-ast-lst))
  
  ;; 'ctx' is a symbolic name of the context that the 'stx' being
  ;; parsed is in. Inserts bindings into 'defs' as a side effect.
  ;; Returns an Ast object for non top-level things.
  (define (parse ctx stx)
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
       (Begin (map (fix parse ctx)
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

      ;; TODO multiple (or zero) binding case
      ((define-values (id) e)
       (identifier? #'id)
       (make-DefVar ctx stx #'id #'e))
           
      ((define-syntaxes . _)
       (eq? ctx 'module-level)
       (void))

      ((#%require . specs)
       (eq? ctx 'module-level)
       (require! (syntax->list #'specs)))

      ;; %core language
      
      ;; ((k-app rt.%core (quote n))
      ;;  (eq? 'pass (syntax-e #'n))
      ;;  (new-Pass stx))

      ;; ((k-app rt.%core (quote n) id-stx)
      ;;  (eq? 'call (syntax-e #'n))
      ;;  (new-Call stx (Var-from-stx #'id-stx)))

      ;; expr non-terminal

      ((#%plain-lambda formals . exprs)
       (when (eq? ctx 'expr)
         (define par-id-lst (syntax->list #'formals))
         (define e-stx-lst (syntax->list #'exprs))
         (define par-ast-lst
           (map (lambda (id)
                  (check-redefinition id stx)
                  (define ast (new-Param id id))
                  (bound-id-table-set! defs id ast)
                  ast)
                par-id-lst))
         (define e-ast-lst
           (map (lambda (e-stx)
                  (parse 'expr e-stx)) e-stx-lst))
         (new-Lambda stx par-ast-lst e-ast-lst)))
      
      ((if c t e)
       (when (eq? ctx 'expr)
         (new-IfExpr stx
                     (parse ctx #'c)
                     (parse ctx #'t)
                     (parse ctx #'e))))
      
      ((#%plain-app p-expr . a-expr)
       (when (eq? ctx 'expr)
         (new-Apply
          stx
          (parse ctx #'p-expr)
          (map
           (fix parse ctx)
           (syntax->list #'a-expr)))))

      ((let-values binds . exprs)
       (when (eq? ctx 'expr)
         (make-Let ctx stx
                   new-Let #'binds #'exprs)))
      
      ((letrec-values binds . exprs)
       (when (eq? ctx 'expr)
         (make-Let ctx stx
                   new-Letrec #'binds #'exprs)))

      ((set! id expr)
       (identifier? #'id)
       (when (eq? ctx 'expr)
         (new-Assign #'id (parse ctx #'expr))))

      ;; 'quote', as it comes in, appears to be unbound for us.
      ((q lit)
       (and (identifier? #'q) (module-or-top-identifier=? #'q #'quote))
       (when (eq? ctx 'expr)
         (new-Literal stx #'lit)))
      
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
      
      ;; local-expand result language
      
      ;; The letrec-syntaxes+values ID we get here is either top-level
      ;; or unbound, according to identifier-binding.
      ((lsv _ v-binds body ...)
       (and (identifier? #'lsv)
            (module-or-top-identifier=? #'lsv #'letrec-syntaxes+values))
       (when (eq? ctx 'expr)
         (parse ctx
                ;; letrec-values might not be the kernel one, but we
                ;; risk it here.
                (syntax/loc stx (letrec-values v-binds body ...)))
         ))

      ;; unrecognized language
      
      (_ (error 'parse-defs-from-module
                "unsupported syntax in ~a context: ~s ~s: ~s"
                ctx stx (or (stx-binding-info stx) '(unbound))
                (syntax->datum stx)))))

  (parse 'module-begin modbeg-stx)
  (define prov-h (resolve-provides prov-lst))
  ;;(pretty-print (dict-map prov-h list))
  (values defs prov-h req-lst))
