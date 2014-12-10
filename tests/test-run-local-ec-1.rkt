#lang magnolisp
(require "lib-cxx-runner.rkt")

(function (five) 5)
(function (seven) 7)

(function (run)
  #:: (export [type (-> Void)] [expected 7])
  (put-int
   (let/local-ec outer-k
     (when #f
       (app/local-ec outer-k (five)))
     (begin-return
      (when #f
        (return (inc (five))))
      (let/local-ec inner-k
        (app/local-ec outer-k (seven)))
      (inc (seven))))))

(run)
