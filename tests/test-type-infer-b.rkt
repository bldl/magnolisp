#lang magnolisp

(typedef Int #:: ([foreign int]))
(typedef List #:: ([foreign std::vector]))

(function (car lst) #:: (foreign ^(-> (<> List Int) Int)))

(function (cons x lst)
  #:: (foreign ^(exists Elem (-> Elem (<> List Elem) (<> List Elem)))))

(function (main lst) #:: (export)
  (car (cons 5 lst)))
