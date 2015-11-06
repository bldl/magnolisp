#lang racket/base

#|
|#

(require "ast-ir.rkt" "util.rkt"
         unstable/custom-write
         (for-syntax racket/base syntax/parse))

;;; 
;;; mutable `Id` set
;;; 

(require racket/set)

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
   (define (set-empty? st)
     (hash-empty? (SetId-h st)))
   (define (in-set st)
     (in-hash-values (SetId-h st)))])

;; Predicate.
(define* setId? SetId?)

;; Comprehension.
(define-syntax* (for/mutable-setId stx)
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
  (for/mutable-setId ([id lst]) id))

;;; 
;;; mutable `Id` dictionary
;;; 

(require racket/dict)

(define not-found
  (let ()
    (struct NotFound ())
    (NotFound)))

;; Reporting as in Racket's "custom-hash.rkt".
(define (key-failure who dict key)
  (raise-arguments-error
   who
   "no value found for key"
   "key" key
   "all keys" (map car (hash-values dict))))

(struct HashId (h)
  #:property prop:custom-write
  (make-constructor-style-printer
   (lambda (obj) 'hashId)
   (lambda (obj) (for/list ([(k v) (HashId-h obj)]) v)))
  #:methods gen:dict
  [(define (dict-ref dict key [failure-result not-found])
     (define e (hash-ref (HashId-h dict) (Id-bind key) not-found))
     (if (eq? not-found e)
         (cond
           [(eq? failure-result not-found)
            (key-failure 'dict-ref (HashId-h dict) key)]
           [(procedure? failure-result)
            (failure-result)]
           [else
            failure-result])
         (cdr e)))
   (define (dict-set! dict key v)
     (hash-set! (HashId-h dict) (Id-bind key) (cons key v)))
   (define (dict-remove! dict key)
     (hash-remove! (HashId-h dict) (Id-bind key)))
   (define (dict-iterate-first dict)
     (hash-iterate-first (HashId-h dict)))
   (define (dict-iterate-next dict pos)
     (hash-iterate-next (HashId-h dict) pos))
   (define (dict-iterate-key dict pos)
     (car (hash-iterate-value (HashId-h dict) pos)))
   (define (dict-iterate-value dict pos)
     (cdr (hash-iterate-value (HashId-h dict) pos)))])

;; Predicate.
(define* hashId? HashId?)

;; Constructor.
(define* (make-hashId [assocs null])
  (HashId (make-hasheq
           (for/list ([e assocs])
             (values (Id-bind (car e)) e)))))

(define* (mutable-hashId . kv)
  (define h (make-hasheq))
  (let loop ([p kv])
    (unless (null? p)
      (define id (car p))
      (define v (cadr p))
      (when (null? v)
        (raise-arguments-error
         'mutable-hashId
         "expected an even number of arguments"
         "kv" kv))
      (hash-set! h (Id-bind id) (cons id v))
      (loop (cddr p))))
  (HashId h))

;; Comprehension.
(define-syntax* (for/mutable-hashId stx)
  (syntax-parse stx
    ((_ clauses . defs+exprs)
     (with-syntax ([original stx])
       #'(let ([h (make-hasheq)])
           (for/fold/derived original () clauses
             (let-values ([(id v) (let () . defs+exprs)])
               (hash-set! h (Id-bind id) (cons id v)))
             (values))
           (HashId h))))))
