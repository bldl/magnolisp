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
     #:out (open-output-nowhere))))

(define (check-compile-mgl-mod mod)
  (check-not-exn
   (thunk (compile-mgl-mod mod))
   (format "failed to compile program ~a" mod)))

(define (check-not-compile-mgl-mod mod exn-predicate)
  (check-exn
   exn-predicate
   (thunk (compile-mgl-mod mod))
   (format "no expected failure compiling program ~a" mod)))

(module m1 magnolisp/2014
  (function (f)
    (#:annos export (type (fn Bool)))
    #t)
  (provide f))

(check-compile-mgl-mod '(submod "." m1))

(module m2 magnolisp/2014
  (function (f)
    (#:annos export
             (build (+= mixed-bad 1 "2"))
             (type (fn Bool)))
    #t)
  (define x 1))

(check-not-compile-mgl-mod
 '(submod "." m2)
 #rx"type mismatch")

(module m3 magnolisp/2014
  (function (f) (#:annos export) 1)
  (define x 3)) ;; xxx if do not have this last (unused) definition, then some things are not defined, and we do not get syntax-source-module for #%module-begin either, may be related

(check-not-compile-mgl-mod
 '(submod "." m3)
 #rx"program is not fully typed")

(module m4 magnolisp/2014
  (function (f)
    (#:annos export (type (fn Bool)))
    (do
      (var x #t)
      (var y (x))
      (return x)))
  (define x 3))

(check-not-compile-mgl-mod
 '(submod "." m4)
 #rx"application target")
