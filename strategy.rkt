#lang racket/base

#|

This is a basic Stratego-inspired term rewriting library for
Racket. http://strategoxt.org/

The primitive traversal operators (one, some, all) and strategy
combinators (e.g., topdown, bottomup) together implement the notion of
generic traversal strategies.

Everything here apart from failure values and 'one', 'some', and 'all'
operators are generic, and some of those are also implemented in terms
of interfaces.

We have some additions here, such as 'must' to function as a sort of
an assertion. If a 'must' succeed strategy fails it is an error, and
not merely a reason to backtrack.

We have 'visit' variants of applicable strategies. A visit does not
rewrite, and is hence more efficient, as terms do not need to be
reconstructed. No return values are checked during a visit, as a visit
is only done for its side effects. This also means that a lot of the
rewriting combinators simply do not make sense. Consider 'try' or
'alt', for example. Calling 'rec' is semantically valid as 'rec' is
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
;;; Subterm access interface.
;;; 

(define-generics* strategic
  (all-visit-term s strategic)
  (all-rw-term s strategic)
  (get-term-fields strategic)
  (set-term-fields strategic lst))

;;; 
;;; List access operations.
;;;

;; Implementations of gen:strategic operations for lists. We do not
;; actually include the list type into gen:strategic, but these
;; operations may be useful in implementing gen:strategic operations
;; for user-defined types.

;; Like 'for-each', except that does not accept multiple list
;; arguments.
(define* (all-visit-list s lst)
  (for-each s lst))

;; Like 'map', except that: does not accept multiple list arguments;
;; and if 's' returns #f, then stops mapping and returns #f.
(define* (all-rw-list s lst)
  (let next ((res-lst '())
             (lst lst))
    (if (null? lst)
        (reverse res-lst)
        (let ((res (s (car lst))))
          (and res
               (next (cons res res-lst) (cdr lst)))))))

;; Like 'map', except that: does not accept multiple list arguments;
;; does not change elements for which 's' returns #f; and if 's'
;; returns #f for all elements, then returns #f.
(define* (some-rw-list s lst)
  (define some? #f)
  (define res (map (lambda (x)
                     (define y (s x))
                     (if y (begin (set! some? #t) y) x))
                   lst))
  (and some? res))

(define* (one-rw-list s lst)
  (let next ((res-lst '())
             (lst lst))
    (if (null? lst)
        #f
        (let* ((x (car lst))
               (xs (cdr lst))
               (res (s x)))
          (if res
              (append (reverse res-lst) (cons res xs))
              (next (cons x res-lst) xs))))))

;;; 
;;; Abstract term access operations.
;;;

(define* (for-each-term-field f strategic)
  (for-each f (get-term-fields strategic)))

(define* (map-term-field f strategic)
  (set-term-fields strategic (map f (get-term-fields strategic))))

(define* (some-rw-term s strategic)
  (define o-lst (get-term-fields strategic))
  (define some? #f)
  (define n-lst 
    (for/list ((fv o-lst))
      (define nv (if (list? fv)
                     (some-rw-list s fv)
                     (s fv)))
      (if nv
          (begin
            (set! some? #t)
            nv)
          fv)))
  (and some? (set-term-fields strategic n-lst)))

(define* (one-rw-term s strategic)
  (define o-lst (get-term-fields strategic))
  (define one? #f)
  (define n-lst 
    (for/list ((fv o-lst))
      (if one? 
          fv
          (let ()
            (define nv (if (list? fv)
                           (one-rw-list s fv)
                           (s fv)))
            (if nv
                (begin
                  (set! one? #t)
                  nv)
                fv)))))
  (and one? (set-term-fields strategic n-lst)))

;;; 
;;; Stateful term access operators.
;;; 

;; Rewrites subterms of `ast` with strategy `f`, threading through
;; state `st` in the process. The function `f` must take and return an
;; extra state value, whose initial value is `st`.
(define* (stateful-all-rw-term f st ast)
  (let ([ast
         (all-rw-term
          (lambda (ast)
            (let-values ([(sub-st ast) (f st ast)])
              (set! st sub-st)
              ast))
          ast)])
    (values st ast)))

;;; 
;;; Primitive traversal operators for lists.
;;; 

(define-syntax-rule
  (define-strategy-combinator* n f)
  (define* (n s)
    (lambda (ast)
      (f s ast))))

;; These subterm traversals may be invoked for immediate "local"
;; traversals within terms containing list data. We could later
;; provide operations for vectors, boxes, and immutable hash tables,
;; for instance.

(module+ test
  (require rackunit))

(define-strategy-combinator* one/list one-rw-list)

(module+ test
  (check-equal?
   (list
    ((one/list number?) '())
    ((one/list number?) '(x y z))
    ((one/list number?) '(x 2 y 4)))
   '(#f #f (x #t y 4))))

(define-strategy-combinator* some/list some-rw-list)

(module+ test
  (check-equal?
   (list
    ((some/list number?) '())
    ((some/list number?) '(x y z))
    ((some/list number?) '(x 2 y 4)))
   '(#f #f (x #t y #t))))

;; This is an 'all' for lists, where elements are "subterms".
(define-strategy-combinator* all/list all-rw-list)

(module+ test
  (check-equal?
   (list
    ((all/list number?) '())
    ((all/list number?) '(1 2 3))
    ((all/list number?) '(x 2 y 4)))
   '(() (#t #t #t) #f)))

(define-strategy-combinator* all-visit/list all-visit-list)

(module+ test
  (check-equal?
   '(#f #f #t)
   (let ()
     (define lst null)
     ((all-visit/list
       (lambda (x)
         (set! lst (cons x lst))))
      '(#t #f #f))
     lst)))

;;; 
;;; Primitive traversal operators.
;;; 

(define* (fail-rw ast) #f)
(define* (id-rw ast) ast)

(define-strategy-combinator* all-visit all-visit-term)
(define-strategy-combinator* all all-rw-term)
(define-strategy-combinator* some some-rw-term)
(define-strategy-combinator* one one-rw-term)

;;; 
;;; Strategy combinators.
;;; 

;; Note quite the Stratego 'rec', but close, and handles the common
;; case. 'impl' is (-> ast (or/c ast #f)), and has both 's' and itself
;; (as 'again') in scope.
(define-syntax-rule* (rec again s impl)
  (lambda (s)
    (letrec ((again impl))
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

;; Combines visit actions in a way that 'compose' would not.
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
    ((_ s)
     (must s "strategy did not apply" (quote s)))
    ((_ s msg v ...)
     (lambda (ast)
       (or (s ast)
           (error msg v ...))))))

;;; 
;;; Tree traversal strategy combinators. 
;;; 

(define* topdown
  (rec again s
       (seq s (all again))))

(define* topdown-visit
  (rec again s
       (seq-visit s (all-visit again))))

(define* topdown-break
  (rec again s
       (seq-break s (all again))))

(define* topdown-visit-break
  (rec again s
       (seq-visit-break s (all-visit again))))

(define* bottomup
  (rec again s
       (seq (all again) s)))

(define* bottomup-visit
  (rec again s
       (seq-visit (all-visit again) s)))

(define* outermost
  (rec again s
       (topdown (try (seq s again)))))

(define* innermost
  (rec again s
       (bottomup (try (seq s again)))))

(module+ test
  (struct List (lst)
          #:methods gen:strategic
          [(define (all-visit-term s strategic)
             (all-visit-list s (List-lst strategic)))
           (define (all-rw-term s strategic)
             (define r (all-rw-list s (List-lst strategic)))
             (and r (List r)))
           (define (get-term-fields strategic)
             (list (List-lst strategic)))
           (define (set-term-fields strategic lst)
             (apply List lst))])
  
  (define lst (List '(1 2 3)))
  (let ((x 0))
    (define (s ast) (set! x (add1 x)))
    (define (br ast) (break))
    ((all-visit s) lst)
    (check-eqv? x 3)
    ((seq-visit-break (all-visit s) (all-visit s)) lst)
    (check-eqv? x 9)
    ((seq-visit-break (all-visit s) br (all-visit s)) lst)
    (check-eqv? x 12)))
