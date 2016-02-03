#lang magnolisp
(require "lib-cxx-runner.rkt")

(define-syntax (mfold stx)
  (syntax-case stx ()
    [(_ op init lst n)
     (let ([i (syntax-e #'n)])
       (if (= i 0)
           #'init
           #`(let ()
              (var tmp lst)
              (if (IntList-empty? tmp)
                  init
                  (op (IntList-head tmp) 
                      (mfold op init (IntList-tail tmp) 
                             #,(sub1 i)))))))]))
           
(function (sum-2 lst)
  (mfold int-add 0 lst 2))

(function (sum-3 lst)
  (mfold int-add 0 lst 3))

(function (run) #:: (export [type (-> Void)] [expected 0 1 4 4 4 0 1 4 10 10])
  (define lst-0 (IntList-new))
  (define lst-1 (IntList-push-back lst-0 1))
  (define lst-2 (IntList-push-back lst-1 3))
  (define lst-3 (IntList-push-back lst-2 6))
  (define lst-4 (IntList-push-back lst-3 99))
  
  (put-int (sum-2 lst-0))
  (put-int (sum-2 lst-1))
  (put-int (sum-2 lst-2))
  (put-int (sum-2 lst-3))
  (put-int (sum-2 lst-4))
  
  (put-int (sum-3 lst-0))
  (put-int (sum-3 lst-1))
  (put-int (sum-3 lst-2))
  (put-int (sum-3 lst-3))
  (put-int (sum-3 lst-4)))

(run)
