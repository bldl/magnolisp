#lang magnolisp
(require "lib-cxx-runner.rkt")

(define-syntax-rule (define<> x f e)
  (define-syntax f (cons #'x #'e)))

(define-syntax (use stx)
  (syntax-case stx (with as)
    [(_ f with new-x as fx)
     (let ([v (syntax-local-value #'f)])
       (with-syntax ([old-x (car v)] [e (cdr v)])
         #'(define fx
             (let-syntax ([old-x 
               (make-rename-transformer #'new-x)])
               e))))]))

(define<> T id 
  (annotate ([type (-> T T)]) 
    (lambda (x) x)))

(use id with int as int-id)        
(use id with long as long-id)      

(function (run-with x)
  (long-id (->long (int-id x))))   

(function (run) #:: (export [expected 8])
  (put-long (run-with 8)))
