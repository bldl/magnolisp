#lang info
(define collection "magnolisp")
(define name "Magnolisp")
(define blurb
  '("A toy programming language."))
(define scribblings '(("manual-src/manual.scrbl")))
(define racket-launcher-libraries '("compiler-cli.rkt"))
(define racket-launcher-names '("mglc"))
(define compile-omit-paths '("dist" "etc" "failing" "gh-pages" "retired" "tests"))
(define deps '(("base" #:version "6.0")))
