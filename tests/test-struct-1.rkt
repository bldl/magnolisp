#lang magnolisp

(require "../struct.rkt" racket/match)

(define #:type Id #:: ([foreign std::string]))

(define-foreign-struct Var ([id :: Id]) #:transparent)

(define (main-ctor) #:: (export)
  (make-Var "x"))

(define (main-pred-true) #:: (export)
  (Var? (Var "x")))

(define (main-pred-false) #:: (export)
  (Var? (Var-id (Var "x"))))

(define (main-accessor) #:: (export)
  (Var-id (Var "x")))

(define (main-match x) #:: (export ^(-> Var Id))
  (match x
    [(Var y) y]
    [_ "z"]))

(define (main-match-call) #:: (export)
  (main-match (Var "x")))

(main-ctor)
(main-pred-true)
(main-pred-false)
(main-accessor)
(main-match-call)
