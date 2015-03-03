#lang magnolisp

(typedef int #:: (foreign))

(define main
  (let-annotate ([build build-dummy])
    (let-annotate (export)
      (let-annotate (^(-> int int))
        (let-annotate ([build build-another])
          (lambda (x)
            x))))))

(main 7)
