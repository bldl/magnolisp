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
     (let/local-ec middle-k
      (when #f
        (app/local-ec middle-k (inc (five))))
      (let/local-ec inner-k
        (if (non-zero? (seven))
            (app/local-ec outer-k (seven))
            (app/local-ec inner-k (inc (inc (seven))))))
      (inc (seven))))))

(run)
