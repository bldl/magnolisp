#lang magnolisp

(function (f x)
  (#:annos ^(fn Int Int) export)
  x)

(function (add-prim x y z) #an(foreign ^(fn Int Int Int Int))
  (+ x y z))

(function (g x)
  (#:annos ^(fn Int Int) export (build use-g))
  (add-prim x (h) (seven)))

(function (private-f x y z)
  (add-prim (nine) x y))

(define-values () (values)) ;; okay, ignored

;; type annos do not currently work here, though
(define-values (f-1 f-2) ;; okay, exactly three values
  (values (lambda (x) x)
          (lambda () 2)))
(anno! f-1 ^(fn Int Int))
(anno! f-2 ^(fn Int))

(function (use-fs)
  (#:annos export
           (build x y (fal #f) (+= both #t #f)
                  (x-h (#:hex #xff)) ;;(+= mixed-bad 1 "2")
                  (z 1) (+= w "a" "b") ;;(bad a^)
                  (v x) (+= ww a b c d e f g h)
                  (v x)))
  (f-1 (f-2)))

(require (rename-in "lib-modules-2.rkt" [six h]))

(provide (all-defined-out) (prefix-out t. (all-defined-out)))
