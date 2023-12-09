#lang racket

(require (relative-in magnolisp
                      "compiler-api.rkt"))

(module sub magnolisp
  (provide int-identity)

  (typedef Int #:: ([foreign int]))

  (define (int-identity x)
    #:: (export [type (-> Int Int)])
    x))

(require 'sub)

(* 2 (int-identity 5))

(let ([st (compile-modules '(submod "." sub) #:relative-to (syntax-source #'here))])
  (pretty-display st)
  (generate-files st '((cxx (parts cc))) #:banner #f #:out (current-output-port)))
