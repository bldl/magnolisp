#lang racket/base

#|
|#

(require "strategy.rkt" "strategy-list.rkt" "util.rkt")

;;; 
;;; Abstract term access operations.
;;;

(define* (term-for-each f strategic)
  (for-each f (term-fields strategic)))

(define* (term-map f strategic)
  (set-term-fields strategic (map f (term-fields strategic))))

(require*-only-in [(submod "strategy.rkt" private)
                   term-rewrite-some term-rewrite-one])

;;; 
;;; Stateful term access operators.
;;; 

;; Rewrites subterms of `ast` with strategy `f`, threading through
;; state `st` in the process. The function `f` must take and return an
;; extra state value, whose initial value is `st`.
(define* (term-rewrite-all/stateful f st ast)
  (let ([ast
         (term-rewrite-all
          (lambda (ast)
            (let-values ([(sub-st ast) (f st ast)])
              (set! st sub-st)
              ast))
          ast)])
    (values st ast)))

(module* test #f
  (require rackunit)
  
  (struct List (lst)
          #:methods gen:strategic
          [(define (term-visit-all s strategic)
             (list-visit-all s (List-lst strategic)))
           (define (term-rewrite-all s strategic)
             (define r (list-rewrite-all s (List-lst strategic)))
             (and r (List r)))
           (define (term-fields strategic)
             (list (List-lst strategic)))
           (define (set-term-fields strategic lst)
             (apply List lst))])
  
  (define lst (List '(1 2 3)))
  (let ((x 0))
    (define (s ast) (set! x (add1 x)))
    (define (br ast) (break))
    ((all-visitor s) lst)
    (check-eqv? x 3)
    ((seq-visit-break (all-visitor s) (all-visitor s)) lst)
    (check-eqv? x 9)
    ((seq-visit-break (all-visitor s) br (all-visitor s)) lst)
    (check-eqv? x 12)))

;;; 
;;; Strategies.
;;; 

(define-strategy-combinator* term-all-visitor term-visit-all)
(define-strategy-combinator* term-all-rewriter term-rewrite-all)
(define-strategy-combinator* term-some-rewriter term-rewrite-some)
(define-strategy-combinator* term-one-rewriter term-rewrite-one)

;; DEPRECATED
(provide (rename-out [term-all-visitor all-visit]
                     [term-all-rewriter all]
                     [term-some-rewriter some]
                     [term-one-rewriter one]))
