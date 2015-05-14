#lang racket/base

#|
|#

(require "strategy.rkt" "util.rkt")

;;; 
;;; Abstract term access operations.
;;;

(define* (term-for-each f strategic)
  (for-each f (term-fields strategic)))

(define* (term-map f strategic)
  (set-term-fields strategic (map f (term-fields strategic))))

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
;;; Primitive strategies.
;;; 

(define-specific-data-strategy* term-all-visitor term-visit-all)
(define-specific-data-strategy* term-all-rewriter term-rewrite-all)
(define-specific-data-strategy* term-some-rewriter term-rewrite-some)
(define-specific-data-strategy* term-one-rewriter term-rewrite-one)

;; DEPRECATED
(provide (rename-out [term-all-visitor all-visit]
                     [term-all-rewriter all]
                     [term-some-rewriter some]
                     [term-one-rewriter one]))

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
