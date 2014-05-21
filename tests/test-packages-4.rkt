#lang magnolisp

#|

A "packages" example adapted from Racket docs.

A variant of test-packages-3.rkt, but with a different selection of
annotations.

|#

(require compatibility/package)

(typedef string #an(foreign))

(define-package big-russian-doll (middle-russian-doll)
  (define-package middle-russian-doll (little-russian-doll)
    (function (little-russian-doll) #an(export ^(fn string))
      "Anastasia")))

(function (g)
  (do (open-package big-russian-doll)
      (open-package middle-russian-doll)
      (return (little-russian-doll))))

(g)
