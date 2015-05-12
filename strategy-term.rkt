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
    ((all-visit s) lst)
    (check-eqv? x 3)
    ((seq-visit-break (all-visit s) (all-visit s)) lst)
    (check-eqv? x 9)
    ((seq-visit-break (all-visit s) br (all-visit s)) lst)
    (check-eqv? x 12)))
