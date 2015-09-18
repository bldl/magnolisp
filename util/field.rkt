#lang racket/base

#|

A match pattern for matching against individual fields of `struct`
instances, concrete AST nodes (see `define-ast`), and views (see
`define-view`), and potentially other things as well.

E.g.:
  (fields Ast)
  (fields Expr e [type t])
  (fields AssignExpr [rv e] [lv (? Var?)])

|#

(require racket/match
         (for-syntax racket/base racket/syntax syntax/parse))

(provide fields)

(define-match-expander fields
  (lambda (stx)
    (define-splicing-syntax-class maybe-id ;; matches #'(id) or #'()
      (pattern (~or (~seq (~var _ id)) (~seq))))
    (syntax-parse stx
      [(_ t:id obj:maybe-id [fn:id pat:expr] ...)
       (define tsym (syntax-e #'t))
       (with-syntax ([t? (format-id #'t "~a?" tsym)]
                     [(fget ...)
                      (map
                       (lambda (x) (format-id #'t "~a-~a" tsym (syntax-e x)))
                       (syntax->list #'(fn ...)))])
         #'(? t? (app fget pat) ... . obj))])))
