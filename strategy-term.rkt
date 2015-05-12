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

(define* (term-rewrite-some s strategic)
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

(define* (term-rewrite-one s strategic)
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

;;; 
;;; Primitive traversal operators.
;;; 

(define-strategy-combinator* all-visit term-visit-all)
(define-strategy-combinator* all term-rewrite-all)
(define-strategy-combinator* some term-rewrite-some)
(define-strategy-combinator* one term-rewrite-one)

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
