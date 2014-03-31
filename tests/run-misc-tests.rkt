#lang racket

#|
|#

(require magnolisp/compiler-api rackunit)

(define-syntax (this-source stx)
  (datum->syntax stx (syntax-source stx) stx))

(define (compile-mgl-mod mod)
  (let ((st (compile-modules mod #:relative-to (this-source))))
    (generate-files
     st '((build (gnu-make qmake c ruby))
          (cxx (cc hh)))
     #:out (open-output-nowhere)
     )))

(define (check-compile-mgl-mod mod)
  (check-not-exn
   (thunk (compile-mgl-mod mod))
   (format "failed to compile program ~a" mod)))

(define (check-not-compile-mgl-mod mod exn-predicate)
  (check-exn
   exn-predicate
   (thunk (compile-mgl-mod mod))
   (format "no expected failure compiling program ~a" mod)))

(module m1 magnolisp
  (function (f)
    (#:annos export (type (fn predicate)))
    true)
  (provide f))

(check-compile-mgl-mod '(submod "." m1))

(module m2 magnolisp
  (function (f)
    (#:annos export
             (build (+= mixed-bad 1 "2"))
             (type (fn predicate)))
    true)
  (define x 1))

(check-not-compile-mgl-mod
 '(submod "." m2)
 #rx"type mismatch")

(module m3 magnolisp
  (function (f) (#:annos export) 1))

(check-not-compile-mgl-mod
 '(submod "." m3)
 #rx"program is not fully typed")

