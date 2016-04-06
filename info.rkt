#lang info
(define name "Magnolisp")
(define blurb
  '("A toy programming language."))
(define scribblings '(("manual-src/manual.scrbl" () (experimental))))
(define racket-launcher-libraries '("compiler-cli.rkt"))
(define racket-launcher-names '("mglc"))
(define deps '(("base" #:version "6.3") "data-lib" "scribble-lib"))
(define build-deps '("at-exp-lib" "racket-doc" "rackunit-lib"))
(define compile-omit-paths '("dist" "doc" "etc" "failing" "gh-pages" "retired" "tests"))
(define test-omit-paths '("dist" "doc" "etc" "failing" "gh-pages" "retired"))
