#lang magnolisp

(require "../struct.rkt" racket/match)

(define #:type Ast #:: (foreign))
(define #:type Id #:: ([foreign std::string]))

(define-foreign-struct (Var :: Ast) ([id :: Id]) #:transparent)
(define-foreign-struct (VoidExpr :: Ast) () #:transparent)

(function (main-ctor-Var) #:: (export)
  (make-Var "x"))

(function (main-ctor-VoidExpr) #:: (export ^(-> Ast))
  (make-VoidExpr))

(function (main-pred-Var) #:: (export)
  (Var? (Var "x")))

(function (main-pred-Id) #:: (export)
  (Var? (Var-id (Var "x"))))

(function (main-pred-VoidExpr) #:: (export)
  (Var? (VoidExpr)))

(function (main-accessor) #:: (export)
  (Var-id (Var "x")))

(function (main-match x) #:: (export ^(-> Ast Id))
  (match x
    [(Var y) y]
    [_ "z"]))

(function (main-match-call-Var) #:: (export)
  (main-match (Var "x")))

(function (main-match-call-VoidExpr) #:: (export)
  (main-match (VoidExpr)))

(main-ctor-Var)
(main-ctor-VoidExpr)
(main-pred-Var)
(main-pred-Id)
(main-pred-VoidExpr)
(main-accessor)
(main-match-call-Var)
(main-match-call-VoidExpr)
