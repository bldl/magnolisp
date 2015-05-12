#lang racket/base

#|

This is a basic Stratego-inspired term rewriting library for
Racket. http://strategoxt.org/

The primitive traversal operators (one, some, all) and strategy
combinators (e.g., topdown, bottomup) together implement the notion of
generic traversal strategies.

Everything here apart from failure values and `one`, `some`, and `all`
operators are generic, and some of those are also implemented in terms
of interfaces.

We have some additions here, such as `must` to function as a sort of
an assertion. If a `must` succeed strategy fails it is an error, and
not merely a reason to backtrack.

We have `visit` variants of applicable strategies. A visit does not
rewrite, and is hence more efficient, as terms do not need to be
reconstructed. No return values are checked during a visit, as a visit
is only done for its side effects. This also means that a lot of the
rewriting combinators simply do not make sense. Consider `try` or
`alt`, for example. Calling `rec` is semantically valid as `rec` is
not specific to rewriting.

Using topdown-visit is not suitable when wanting to prune subtrees,
but we have no topdown-visit-prune. (Note that pruning makes no sense
for bottom-up traversals.) We can simply instead choose the subtrees
we do want to visit, for now, by using lower-level operations.
Breaking is easy for visits (but not for rewrites), as one can just
record an escape continuation for the visit. If required, it can be
recorded in a dynamically scoped variable.

Mutable, closed over variables may be used to hold dynamic rules in
the sense of Stratego. Something like the dynamic rule scope construct
in turn can be achieved through the use of parameters. See Bravenboer
et al: Program Transformation with Scoped Dynamic Rewrite Rules (2005).

For related discussion on Scheme-based implementation of generic
traversal strategies, see Chapter 5 of Pankaj Surana's dissertation
Meta-Compilation of Language Abstractions (2006).

|#

(require "util.rkt" racket/generic)

;;; 
;;; Subterm access operations.
;;; 

(define-generics* strategic
  (term-visit-all s strategic)
  (term-rewrite-all s strategic)
  (term-fields strategic)
  (set-term-fields strategic lst))

;; Like `map`, except that: does not accept multiple list arguments;
;; does not change elements for which `s` returns #f; and if `s`
;; returns #f for all elements, then returns #f. Returns unmodified
;; `lst` if `s` does not change any elements (i.e., `eq?`uivalence
;; holds).
(define (list-rewrite-some s lst)
  (define changed? #f)
  (define some? #f)
  (define res (map (lambda (x)
                     (define y (s x))
                     (if y
                         (begin
                           (unless (eq? x y)
                             (set! changed? #t))
                           (set! some? #t)
                           y)
                         x))
                   lst))
  (and some? (if changed? res lst)))

;; Like `map`, but stops transforming elements in `lst` as soon as `s`
;; has produced a true value for an element. Does not change elements
;; for which `s` returns #f. If `s` returns #f for all elements, the
;; overall result will also be #f. Returns unmodified `lst` if `s`
;; does not change any elements.
(define (list-rewrite-one s in-lst)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        #f
        (let* ((x (car lst))
               (xs (cdr lst))
               (res (s x)))
          (if res
              (if (eq? x res)
                  in-lst
                  (append (reverse res-lst) (cons res xs)))
              (next (cons x res-lst) xs))))))

(define (term-rewrite-some s strategic)
  (define o-lst (term-fields strategic))
  (define changed? #f)
  (define some? #f)
  (define n-lst 
    (for/list ([fv o-lst])
      (let ([nv (if (list? fv)
                    (list-rewrite-some s fv)
                    (s fv))])
        (if nv
            (begin
              (unless (eq? fv nv)
                (set! changed? #t))
              (set! some? #t)
              nv)
            fv))))
  (and some?
       (if changed?
           (set-term-fields strategic n-lst)
           strategic)))

(define (term-rewrite-one s strategic)
  (define o-lst (term-fields strategic))
  (define changed? #f)
  (define one? #f)
  (define n-lst 
    (for/list ([fv o-lst])
      (if one? 
          fv
          (let ()
            (define nv (if (list? fv)
                           (list-rewrite-one s fv)
                           (s fv)))
            (if nv
                (begin
                  (unless (eq? fv nv)
                    (set! changed? #t))
                  (set! one? #t)
                  nv)
                fv)))))
  (and one?
       (if changed?
           (set-term-fields strategic n-lst)
           strategic)))

(module* private #f
  (provide list-rewrite-some list-rewrite-one
           term-rewrite-some term-rewrite-one))

;;; 
;;; Primitive traversal operators.
;;; 

(define* (fail-rw x) #f)
(define* (id-rw x) x)

(define* current-visit-all (make-parameter term-visit-all))
(define* current-rewrite-all (make-parameter term-rewrite-all))
(define* current-rewrite-some (make-parameter term-rewrite-some))
(define* current-rewrite-one (make-parameter term-rewrite-one))

;;; 
;;; Strategy combinators.
;;; 

(define-syntax-rule*
  (define-strategy-combinator* n f)
  (define* (n s)
    (lambda (ast)
      (f s ast))))
