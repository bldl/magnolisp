#lang magnolisp

(require "../struct.rkt" racket/match)

(define #:type Id #:: ([foreign std::string]))

(define-foreign-struct Var ([id :: Id]) #:transparent)

(function (main-ctor) #:: (export)
  (make-Var "x"))

(function (main-pred-true) #:: (export)
  (Var? (Var "x")))

(function (main-pred-false) #:: (export)
  (Var? (Var-id (Var "x"))))

(function (main-accessor) #:: (export)
  (Var-id (Var "x")))

(function (main-match x) #:: (export ^(-> Var Id))
  (match x
    [(Var y) y]
    [_ "z"]))

(function (main-match-call) #:: (export)
  (main-match (Var "x")))

(main-ctor)
(main-pred-true)
(main-pred-false)
(main-accessor)
(main-match-call)
