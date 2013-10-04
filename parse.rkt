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
  (->* (syntax? bound-id-table?) void?)
  (parse-defs-from-syntax! mod-stx defs)

  ;; 'ctx' is a symbolic name of the context that the 'stx' being
  ;; parsed is in. 'outer-ctx' is the outer context as a list of IDs
  ;; of surrounding definitions, with innermost ID first.
  (define (prs ctx outer-ctx stx)
    ;;(writeln (list ctx stx))
    
    (kernel-syntax-case* stx #f (%core)
      ;; ((module n pn (#%plain-module-begin body ...))
      ;;  (new-Module stx (prs-lst 'mod (syntax->list #'(body ...)))))

      ;; ((k-app rt.%core (quote n))
      ;;  (eq? 'pass (syntax-e #'n))
      ;;  (new-Pass stx))

      ;; ((k-app rt.%core (quote n) id-stx)
      ;;  (eq? 'call (syntax-e #'n))
      ;;  (new-Call stx (Var-from-stx #'id-stx)))

      ;; ((k-app . _)
      ;;  (eq? ctx 'mod)
      ;;  #f)
      
      ;; ((define-syntaxes . _)
      ;;  (eq? ctx 'mod)
      ;;  #f)

      ;; ((#%require . _)
      ;;  (eq? ctx 'mod)
      ;;  #f)
      
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
      
      ;; ((define-values (n) def)
      ;;  (let* ((def-stx #'def)
      ;;         (export
      ;;          (lets then-if-let x-stx (syntax-property #'n 'export)
      ;;                then-let x (syntax-e x-stx)
      ;;                (if (boolean? x) x
      ;;                    (error "expected boolean" `(export ,x-stx)))))
      ;;         (annos (hash-set (stx-annos stx) 'export export)))
      ;;    (syntax-case def-stx (k-lambda quote rt.%core)
      ;;      ((k-app rt.%core (quote t) (k-lambda () body ...))
      ;;       (let ((kind (syntax-e #'t)))
      ;;         (case-eq kind
      ;;          (procedure
      ;;           (Define annos (Var-from-stx #'n)
      ;;             'procedure
      ;;             (prs-lst 'stat (syntax->list #'(body ...)))))
               
      ;;          (primitive
      ;;           (parameterize ((in-primitive? #t))
      ;;             (Define annos (Var-from-stx #'n)
      ;;               'primitive
      ;;               (prs-lst 'stat (syntax->list #'(body ...))))))
               
      ;;          (else
      ;;           (error-non-core stx)))))
           
      (_ (unsupported ctx stx))))

  (prs 'module null mod-stx))
