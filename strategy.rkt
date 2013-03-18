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

|#

(require "util.rkt")

;;; 
;;; Subterm interface.
;;; 

;; E.g. for 'struct', can say #:property prop:subterm-all identity
(define-values* (prop:subterm-all subterm-all? subterm-all)
  (make-struct-type-property 'subterm-all))

;;; 
;;; Strategies for lists.
;;; 

;; We cannot implement custom interfaces for lists. You may invoke
;; these subterm traversal strategies instead as required. We could
;; later provide operations for vectors, boxes, and immutable hash
;; tables, for instance.

(define (list-rw rw ast-lst)
  (map-while (force rw) ast-lst not))

;; This is an 'all' for lists, where elements are "subterms". As 'map'
;; in Stratego.
(define* (list-all s)
  (lambda (ast-lst)
    (list-rw s ast-lst)))

;;; 
;;; Rewrites.
;;; 

(define* (fail ast) #f)

(define* (id ast) ast)

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
(define-syntax-rule* (must s)
  (lambda (ast)
    (or (s ast)
        (error "strategy did not apply" (quote s)))))

;;; 
;;; One-level traversals.
;;; 

;; xxx one
;; xxx some

;; While most strategies here are generic, this one is only supported
;; for terms that implement the required operation.
(define* (all s)
  (lambda (ast)
    (let ((all (subterm-all ast)))
      (all s ast))))

;;; 
;;; Tree traversals.
;;; 

(define* topdown
  (rec again s
       (seq s (all again))))

(define* bottomup
  (rec again s
       (seq (all again) s)))

(define* innermost
  (rec again s
       (bottomup (try (seq s again)))))
