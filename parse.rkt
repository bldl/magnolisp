#lang racket/base

#|

Note that language not in the runtime library may appear. Such
language may come from the lexical cope of 'expand' itself, or from
the lexical scope of a macro expander, and indeed macros may form
chains.

Note that #%app and lambda in 'racket' are macros that probably expand
to code containing '#%kernel versions thereof. All macros must
'expand' to something. Something to keep in mind when comparing
identifiers.

|#

(require "ast.rkt")
(require "util.rkt")
(require (only-in '#%kernel (#%app k-app) (lambda k-lambda)))
(require (prefix-in rt. "runtime-compiler.rkt"))
(require racket/list)

(define (filter-ast ast)
  (cond
   ((Module? ast)
    (struct-copy Module ast
                 (body (filter Define? (Module-body ast)))))
   (else ast)))

;; Drops macro definitions and top-level expressions and statements,
;; does alpha conversion, and turns the input syntax object into an
;; AST.
(define* (parse mod-stx)
  (define (prs stx)
    (syntax-case stx (k-app k-lambda
                      #%plain-module-begin
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

  (filter-ast (prs mod-stx)))

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
