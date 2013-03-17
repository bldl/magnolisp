#lang racket/base

#|

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

;; Drops macro definitions, does alpha conversion, and turns the input
;; syntax object into an AST.
(define* (parse mod-stx)
  (define (p stx)
    (syntax-case stx (#%plain-module-begin
                      define-values
                      module quote rt.%core)

      ((module n pn (#%plain-module-begin body ...))
       (new-Module stx (p-lst (syntax->list #'(body ...)))))

      ((_ rt.%core (quote n) _)
       (eq? 'pass (syntax-e #'n))
       (new-Pass stx))

      ((_ rt.%core (quote n) id-stx)
       (eq? 'call (syntax-e #'n))
       (new-Call stx (Var-from-stx #'id-stx)))

      ((define-values (n) def)
       (let ((def-stx #'def))
         (syntax-case def-stx (quote rt.%core)
           ((_ rt.%core (quote t) (_ () body ...))
            (eq? 'procedure (syntax-e #'t))
            (new-Define stx (Var-from-stx #'n)
                        'procedure
                        (p-lst (syntax->list #'(body ...)))))
           (else
            (error "not core language 'define'" stx)))))

      (else (error "not core language" stx))))

  (define (p-lst lst)
    (filter Ast? (map p lst)))

  (p mod-stx))

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
