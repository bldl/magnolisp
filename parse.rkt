#lang racket/base

#|

Note that language not in the runtime library may appear. Such
language may come from the lexical cope of 'expand' itself, or from
the lexical scope of a macro expander, and indeed macros may form
chains.

Note that #%app and lambda in 'racket' are macros that probably expand
to code containing '#%kernel versions thereof. All macros must
'expand' to something. Something to keep in mind when comparing
identifiers. Note also the 'syntax/kerncase' module, and particularly
'kernel-form-identifier-list'.

|#

(require "ast.rkt" "strategy.rkt" "util.rkt")
(require (prefix-in rt. "runtime-compiler.rkt"))
(require (only-in '#%kernel (#%app k-app) (lambda k-lambda)))
(require racket/function racket/list)
(require syntax/stx) ;; deconstructing syntax objects
(require syntax/id-table)

(define (filter-ast ast)
  (cond
   ((Module? ast)
    (struct-copy Module ast
                 (body (filter Define? (Module-body ast)))))
   (else ast)))

(define (unique-rename ast)
  (define t (make-free-id-table))
  (define g
    (lambda (ast)
      (cond
       ((Define? ast)
        ;; We preserve the names of all module level declarations
        ;; that appear in the program. Amongst themselves they are
        ;; unique. Everything else will be given a fresh symbol, and
        ;; hence we will still be free of conflicts.
        (let* ((var (Define-var ast))
               (n (Var-name var))
               (annos (Ast-annos var))
               (id-stx (hash-ref annos 'stx)))
          (free-id-table-set! t id-stx n)))
       (else (for-each-subterm g ast)))))
   (define f
    (topdown
     (lambda (ast)
       (cond
        ((Var? ast)
         (let* ((annos (Ast-annos ast))
                (id-stx (hash-ref annos 'stx))
                (n (free-id-table-ref
                    t id-stx
                    (thunk                  
                     (let ((n (gensym (symbol->string (Var-name ast)))))
                       (free-id-table-set! t id-stx n)
                       n)))))
           (Var-rename ast n)))
        (else ast)))))
   (g ast)
   (f ast))

;; Drops macro definitions and top-level expressions and statements,
;; gives a unique name to all identifiers (for easier transforming),
;; and turns the input syntax object into an AST.
(define* (parse mod-stx)
  (define (prs stx)
    (syntax-case stx (k-app k-lambda
                      #%plain-module-begin
                      define-syntaxes
                      define-values module quote
                      rt.%core)

      ((module n pn (#%plain-module-begin body ...))
       (new-Module stx (prs-lst (syntax->list #'(body ...)))))

      ((k-app rt.%core (quote n) _)
       (eq? 'pass (syntax-e #'n))
       (new-Pass stx))

      ((k-app rt.%core (quote n) id-stx)
       (eq? 'call (syntax-e #'n))
       (new-Call stx (Var-from-stx #'id-stx)))

      ((define-syntaxes . _)
       #f)

      ((quote lit)
       (new-Literal stx #'lit))
      
      ((define-values (n) def)
       (let ((def-stx #'def))
         (syntax-case def-stx (k-lambda quote rt.%core)
           ((k-app rt.%core (quote t) (k-lambda () body ...))
            (eq? 'procedure (syntax-e #'t))
            (new-Define stx (Var-from-stx #'n)
                        'procedure
                        (prs-lst (syntax->list #'(body ...)))))
           (else
            (error "not core language 'define'" stx)))))

      (else (error "not core language" stx))))

  (define (prs-lst lst)
    (filter Ast? (map prs lst)))

  (unique-rename (filter-ast (prs mod-stx))))

(define* (print-stx-with-bindings stx)
  (define lst (syntax->list stx))
  (cond
   (lst (for-each print-stx-with-bindings lst))
   ((identifier? stx) (writeln (list stx (identifier-binding stx))))
   (else (writeln stx))))

(define* (find-id-stx find-stx stx (msg #f))
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
