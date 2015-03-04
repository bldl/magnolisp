#lang magnolisp

(require (only-in racket/base string->number))

(typedef int #:: (foreign))
(typedef CString #:: ([foreign |char const*|]))

(define (atoi s)
  #:: (foreign ^(-> CString int))
  ;; not the same semantics if does not fully parse as a number
  (string->number s))

(define (string-identity s)
  #:: (export ^(-> CString CString))
  s)
  
(define (main)
  #:: (export)
  (atoi "77 
foo\\ \" \r \n bar \t \000\001 baz"))

(main)
