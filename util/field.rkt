#lang racket/base

#|

A `fields` match pattern for matching against individual fields of
`struct` instances, concrete AST nodes (see `define-ast`), and
views (see `define-view`), and potentially other things as well.

E.g.:
  (fields Ast)
  (fields Expr e [type t])
  (fields AssignExpr [rv e] [lv (? Var?)])

See also: `struct*`

|#

(require "module.rkt"
         racket/match
         (for-syntax racket/base racket/syntax syntax/parse))

(define-match-expander* fields
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

(define-match-expander _-or
  (lambda (stx)
    (syntax-case stx ()
      [(_ c-pat t-pat)
       (if (eq? '_ (syntax-e #'c-pat))
           #'_
           #'t-pat)])))
  
;; Returns syntax for a match pattern transformer, for things of the
;; specified predicate `pred-id`, and its specified field accessors
;; `get-id-lst`. Pattern matching is positional, so the order of the
;; fields in `get-id-lst` matters, as does their number. A datum
;; literal pattern `_` is treated specially, to avoid unnecessary
;; field accesses.
(define-for-syntax* (make-fields-match-proc-expr pred-id get-id-lst)
  (with-syntax*
    ([pred? pred-id]
     [(get ...) get-id-lst]
     [(pat ...) (generate-temporaries get-id-lst)]
     [(fld-pat ...) #'((app get pat) ...)])
    #'(lambda (stx)
        (syntax-case stx ()
          [(_ pat ...)
           #'(? pred? (_-or pat fld-pat) ...)]))))

(define-syntax* (define-fields-match-expander stx)
  (syntax-parse stx
    [(_ n:id pred:id (get:id ...))
     (define/with-syntax lam
       (make-fields-match-proc-expr #'pred (syntax->list #'(get ...)))) 
     #'(define-match-expander n
         lam)]))
