#lang racket

#|

This is a basic Stratego-inspired term rewriting library for
Racket.

Everything here apart from failure values and 'one', 'some', and 'all'
strategies are generic, and some of those are also implemented in
terms of interfaces. Unfortunately such interfaces cannot be defined
for existing datatypes. Perhaps we should define this module as a
'unit' or somesuch parameterizable construct to allow these operations
to be freely specified.

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
recorded in a dynamically scoped global variable.

|#

(require "util.rkt")
(require racket/generic)

;;; 
;;; Subterm access interface.
;;; 

(define-generics strategic
  (for-each-subterm s strategic)
  (subterm-all s strategic))

;; There seems to be no "generics-out" provide spec.
(provide gen:strategic for-each-subterm subterm-all)

;;; 
;;; Strategies for lists.
;;; 

;; We cannot implement custom interfaces for lists. You may invoke
;; these subterm traversal strategies instead as required, for
;; immediate "local" traversals within terms containing list data. We
;; could later provide operations for vectors, boxes, and immutable
;; hash tables, for instance.

;; (list
;;  ((list-one number?) '())
;;  ((list-one number?) '(x y z))
;;  ((list-one number?) '(x 2 y 4))) ; => '(#f #f (x #t y 4))
(define* (list-one s)
  (lambda (lst)
    (let loop ((r '()) (lst lst))
      (if (null? lst)
          #f
          (let* ((h (car lst))
                 (t (cdr lst))
                 (c (s h)))
            (if c
                (append (reverse r) (cons c t))
                (loop (cons h r) t)))))))

;; (list
;;  ((list-some number?) '())
;;  ((list-some number?) '(x y z))
;;  ((list-some number?) '(x 2 y 4))) ; => '(#f #f (x #t y #t))
(define* (list-some s)
  (lambda (lst)
    (define found #f)
    (let ((r (for/list ((x lst))
                 (let ((y (s x)))
                   (if y
                       (begin (set! found #t) y)
                       x)))))
      (and found r))))

;; This is an 'all' for lists, where elements are "subterms". As 'map'
;; in Stratego.
;;
;; (list
;;  ((list-all number?) '())
;;  ((list-all number?) '(1 2 3))
;;  ((list-all number?) '(x 2 y 4))) ; => '(() (#t #t #t) #f)
(define* (list-all s)
  (lambda (lst)
    (map-while s lst)))

;;; 
;;; Rewrites.
;;; 

(define* (fail ast) #f)

(define* (id ast) ast)

;; Note quite the Stratego 'rec', but close, and handles the common
;; case.
(define-syntax-rule* (rec again s impl)
  (lambda (s)
    (letrec ((again impl))
      again)))

;; Note that (and e ...) defines left-to-right evaluation order, and
;; also that (and) == #t.
(define-syntax* seq
  (syntax-rules ()
    ((_ s ...)
     (lambda (ast)
       (and (begin
              (set! ast (s ast))
              ast) ...
              ast)))))

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

(define* (try s)
  (alt s id))

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
;;; One-level traversals.
;;; 

;; xxx one
;; xxx some

;; While most strategies here are generic, this one is only supported
;; for terms that implement the required operation.
(define* (all s)
  (lambda (ast)
    (subterm-all s ast)))

(define* (all-visit s)
  (lambda (ast)
    (for-each-subterm s ast)))

;;; 
;;; Tree traversals.
;;; 

(define* topdown
  (rec again s
       (seq s (all again))))

(define* topdown-visit
  (rec again s
       (seq-visit s (all-visit again))))

(define* bottomup
  (rec again s
       (seq (all again) s)))

(define* bottomup-visit
  (rec again s
       (seq-visit (all-visit again) s)))

(define* innermost
  (rec again s
       (bottomup (try (seq s again)))))
