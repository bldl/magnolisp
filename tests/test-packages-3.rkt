#lang magnolisp/2014

#|

A "packages" example adapted from Racket docs.

|#

(require compatibility/package)

(typedef string #an(foreign))

(define-package big-russian-doll (middle-russian-doll)
  (define-package middle-russian-doll (little-russian-doll)
    (define (little-russian-doll) (cast string "Anastasia"))))

(function (g) #an(export)
  (open-package big-russian-doll)
  (open-package middle-russian-doll)
  (little-russian-doll))

(g)
