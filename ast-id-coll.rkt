#lang racket/base

#|
|#

(require "ast-magnolisp.rkt" "util.rkt"
         racket/set unstable/custom-write
         (for-syntax racket/base syntax/parse))

;;; 
;;; mutable `Id` set
;;; 

(struct SetId (h)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (obj) 'setId)
   (lambda (obj) (hash-values (SetId-h obj))))
  #:methods gen:set
  [(define (set-member? st v)
     (hash-has-key? (SetId-h st) (Id-bind v)))
   (define (set-add! st v)
     (hash-set! (SetId-h st) (Id-bind v) v))
   (define (set-remove! st v)
     (hash-remove! (SetId-h st) (Id-bind v)))
   (define (in-set st)
     (in-hash-values (SetId-h st)))])

;; Predicate.
(define* setId? SetId?)

;; Comprehension.
(define-syntax* (for/setId stx)
  (syntax-parse stx
    ((_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let ([h (make-hasheq)])
           (for/fold/derived original () clauses
             (let ([id (let () . defs+exprs)])
               (hash-set! h (Id-bind id) id))
             (values))
           (SetId h))))))

;; Constructor.
(define* (mutable-setId . lst)
  (for/setId ([id lst]) id))
