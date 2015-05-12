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
;;; List element access operations.
;;; 

;; Like `for-each`, except that does not accept multiple list
;; arguments.
(define (list-visit-all s lst)
  (for-each s lst))

;; Like `map`, except that: does not accept multiple list arguments;
;; and if `s` returns #f, then stops mapping and returns #f. Returns
;; unmodified `in-lst` if `s` returns each element unmodified.
(define (list-rewrite-all s in-lst)
  (define changed? #f)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        (if changed?
            (reverse res-lst)
            in-lst)
        (let* ((x (car lst))
               (res (s x)))
          (and res
               (let ()
                 (unless (eq? x res)
                   (set! changed? #t))
                 (next (cons res res-lst) (cdr lst))))))))

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

;;; 
;;; Subterm access operations.
;;; 

(define-generics* strategic
  (term-visit-all s strategic)
  (term-rewrite-all s strategic)
  (term-fields strategic)
  (set-term-fields strategic lst))

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

;;; 
;;; Primitive strategies.
;;; 

(define* (fail-rw x) #f)
(define* (id-rw x) x)

(module* private #f
  (provide list-visit-all list-rewrite-all
           list-rewrite-some list-rewrite-one
           term-rewrite-some term-rewrite-one))

(define ((make-strategic-default-visit-all get) s obj)
  (list-visit-all s (get obj)))

(define ((make-strategic-default-rewrite list-rw get set) s obj)
  (define o-lst (get obj))
  (define n-lst (list-rw s o-lst))
  (and n-lst (if (eq? o-lst n-lst) obj (set obj n-lst))))

(define* (make-strategic-data-accessors
          get set
          #:visit-all [vall #f]
          #:rewrite-all [rall #f]
          #:rewrite-some [rsome #f]
          #:rewrite-one [rone #f])
  (hasheq 'fields get
          'set-fields set
          'visit-all (or vall (make-strategic-default-visit-all get))
          'rewrite-all (or rall (make-strategic-default-rewrite
                                 list-rewrite-all get set))
          'rewrite-some (or rsome (make-strategic-default-rewrite
                                   list-rewrite-some get set))
          'rewrite-one (or rone (make-strategic-default-rewrite
                                 list-rewrite-one get set))))

(define* strategic-term-accessors
  (make-strategic-data-accessors
   term-fields set-term-fields
   #:visit-all term-visit-all
   #:rewrite-all term-rewrite-all
   #:rewrite-some term-rewrite-some
   #:rewrite-one term-rewrite-one))

(define* current-strategic-data-accessors
  (make-parameter strategic-term-accessors))

(define-syntax-rule* (with-strategic-data-accessors acc e ...)
  (parameterize ([current-strategic-data-accessors acc])
    e ...))

(define* (get-current-strategic-data-accessor name)
  (define h (current-strategic-data-accessors))
  (unless h
    (error 'get-current-strategic-data-accessor
           "no accessors configured"))
  (hash-ref h name))

(define-syntax-rule*
  (define-strategy-combinator* n f-expr)
  (define* (n s)
    (let ([f f-expr])
      (lambda (ast)
        (f s ast)))))

(define-strategy-combinator* all-visitor
  (get-current-strategic-data-accessor 'visit-all))
(define-strategy-combinator* all-rewriter
  (get-current-strategic-data-accessor 'rewrite-all))
(define-strategy-combinator* some-rewriter
  (get-current-strategic-data-accessor 'rewrite-some))
(define-strategy-combinator* one-rewriter
  (get-current-strategic-data-accessor 'rewrite-one))

;;; 
;;; Strategy combinators.
;;; 

;; Note quite the Stratego `rec`, but close, and handles the common
;; case. `impl` is (-> ast (or/c ast #f)), and has both `s` and itself
;; (as `again`) in scope.
(define-syntax-rule* (rec again s impl)
  (lambda (s)
    (letrec ([again impl])
      again)))

;; Note that (and e ...) defines left-to-right evaluation order, and
;; also that (and) == #t.
(define-syntax-rule* (seq s ...)
  (lambda (ast)
    (and (begin (set! ast (s ast))
                ast) ...
         ast)))

;; Note that (or e ...) defines left-to-right evaluation order, and
;; also that (or) == #f.
(define-syntax* alt
  (syntax-rules ()
    ((_ s ...)
     (lambda (ast)
       (or (s ast) ...)))))

;; Combines visit actions in a way that `compose` would not.
(define-syntax-rule* (seq-visit s ...)
  (lambda (ast)
    (s ast) ...
    (void)))

(struct Break () #:transparent)
(struct BreakWith (v) #:transparent)

(define-syntax* break
  (syntax-rules ()
    ((_ v)
     (BreakWith v))
    ((_)
     (Break))))

;; A sequence that may be interrupted without failing by invoking `break`.
(define-syntax-rule* (seq-break s ...)
  (lambda (ast)
    (let/ec k 
      (and (begin (set! ast (s ast))
                  (when (BreakWith? ast)
                    (k (BreakWith-v ast)))
                  ast) ...
           ast))))

(define-syntax-rule* (seq-visit-break s ...)
  (lambda (ast)
    (and (not (Break? (s ast))) ...)
    (void)))

(define* (try s)
  (alt s id-rw))

(define* repeat
  (rec again s
       (try (seq s again))))

;; Tries a rewrite but restores original term on success.
(define* (where s)
  (lambda (ast)
    (and (s ast) ast)))

;; ((seq (where number?) (must (lambda (x) 2))) 1)   ;=> 2
;; ((seq (where number?) (must (lambda (x) #f))) 1)  ;=> error
(define-syntax* must
  (syntax-rules ()
    [(_ s)
     (must s "strategy did not apply" (quote s))]
    [(_ s msg v ...)
     (lambda (ast)
       (or (s ast)
           (error msg v ...)))]))

;;; 
;;; Tree traversal strategy combinators. 
;;; 

(define* topdown
  (rec again s
       (seq s (all-rewriter again))))

(define* topdown-visit
  (rec again s
       (seq-visit s (all-visitor again))))

(define* topdown-break
  (rec again s
       (seq-break s (all-rewriter again))))

(define* topdown-visit-break
  (rec again s
       (seq-visit-break s (all-visitor again))))

(define* bottomup
  (rec again s
       (seq (all-rewriter again) s)))

(define* bottomup-visit
  (rec again s
       (seq-visit (all-visitor again) s)))

(define* outermost
  (rec again s
       (topdown (try (seq s again)))))

(define* innermost
  (rec again s
       (bottomup (try (seq s again)))))
