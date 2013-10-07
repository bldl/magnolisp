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

|#

(require "ast-magnolisp.rkt"
         "compiler-util.rkt"
         (only-in "runtime.rkt" %core)
         "strategy.rkt"
         "util.rkt"
         "util/case.rkt"
         racket/contract
         racket/function
         racket/list
         syntax/id-table
         syntax/kerncase
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

;;; 
;;; parsing
;;; 

(define-with-contract*
  (-> syntax? bound-id-table? resolve-module-path-result?
      bound-id-table?)
  (parse-defs-from-module modbeg-stx annos r-mp)

  (define defs (make-bound-id-table #:phase 0))

  (define (not-magnolisp stx)
    (error 'parse-defs-from-module "not Magnolisp: ~a" stx))

  (define (redefinition id old-def new-stx)
    (error 'parse-defs-from-module
           "redefinition of ~a: ~a and ~a"
           id (Ast-anno-ref old-def 'stx) new-stx))

  (define (check-redefinition id new-stx)
    (when-let old-def (bound-id-table-ref defs id #f)
              (redefinition id old-def new-stx)))
  
  ;; 'ctx' is a symbolic name of the context that the 'stx' being
  ;; parsed is in. 'outer-ctx' is the outer context as a list of IDs
  ;; of surrounding definitions, with innermost ID first. Inserts
  ;; bindings into 'defs' as a side effect. Returns an Ast object for
  ;; non top-level things.
  (define (parse ctx outer-ctx stx)
    ;;(writeln (list ctx stx))
    
    (kernel-syntax-case* stx #f (%core)

      ((#%module-begin . bs)
       (eq? ctx 'module-begin)
       (for-each (fix parse 'module-level outer-ctx)
                 (syntax->list #'bs)))

      ;; top-level-form non-terminal
      
      ((#%expression _)
       (eq? ctx 'module-level)
       (void))

      ((module . _)
       (eq? ctx 'module-level)
       (void))

      ((begin . bs)
       (Begin (map (fix parse ctx outer-ctx)
                   (syntax->list #'bs))))
      
      ((begin-for-syntax . _)
       (eq? ctx 'module-level)
       (void))

      ;; module-level-form non-terminal

      ((#%provide . _)
       (eq? ctx 'module-level)
       (void))

      ;; submodule-form non-terminal

      ((module* . _)
       (eq? ctx 'module-level)
       (void))
      
      ;; general-top-level-form non-terminal

      ;; TODO multiple (or zero) binding case
      ((define-values (id) e)
       (identifier? #'id)
       (let ()
         (check-redefinition #'id stx)
         (define ast (parse 'expr (cons #'id outer-ctx) #'e))
         (define ann-h (bound-id-table-ref annos #'id #hasheq()))
         (set! ann-h (hash-set ann-h 'stx stx))
         (define def (Def ann-h #'id ast r-mp outer-ctx))
         (bound-id-table-set! defs #'id def)
         ast))
           
      ((define-syntaxes . _)
       (eq? ctx 'module-level)
       (void))

      ((#%require . _)
       (eq? ctx 'module-level)
       (void))

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
                  (define ast (new-Param id id the-NoBody r-mp outer-ctx))
                  (bound-id-table-set! defs id ast)
                  ast)
                par-id-lst))
         (define e-ast-lst
           (map (lambda (e-stx)
                  (parse 'expr outer-ctx e-stx)) e-stx-lst))
         (new-Lambda stx par-ast-lst (Begin #'exprs e-ast-lst))))
      
      ((if c t e)
       (when (eq? ctx 'expr)
         (new-IfExpr stx
                     (parse ctx outer-ctx #'c)
                     (parse ctx outer-ctx #'t)
                     (parse ctx outer-ctx #'e))))
      
      ;; xxx (let-values ([(id ...) expr] ...) expr ...+)
      
      ;; xxx (letrec-values ([(id ...) expr] ...) expr ...+)

      ;; xxx letrec-syntaxes+values
      
      ((set! id expr)
       (identifier? #'id)
       (when (eq? ctx 'expr)
         (new-Assign #'id (parse ctx outer-ctx #'expr))))

      ;; xxx (quote datum)

      ;; ((quote lit)
      ;;  (cond
      ;;   ((eq? ctx 'expr)
      ;;    (new-Literal stx #'lit))
      ;;   ((and (eq? ctx 'stat) (in-primitive?))
      ;;    (let ((d (syntax-e #'lit)))
      ;;      (if (string? d)
      ;;          (new-Verbatim stx d)
      ;;          (error-non-core stx ctx))))
      ;;   (else
      ;;    #f)))
      
      ;; xxx (#%plain-app expr ...+)

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
      
      (_ (error 'parse-defs-from-module
                "unsupported syntax in ~a context: ~s: ~s"
                ctx stx (syntax->datum stx)))))

  (parse 'module-begin null modbeg-stx)
  defs)
