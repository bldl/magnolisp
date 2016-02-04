#lang magnolisp

(typedef int #:: (foreign))

(define main
  (annotate ([build build-dummy])
    (annotate (export)
      (annotate (^(-> int int))
        (annotate ([build build-another])
          (lambda (x)
            x))))))

(main 7)
