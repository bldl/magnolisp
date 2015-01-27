#lang magnolisp

(typedef Int #:: ([foreign int]))
(typedef Long #:: ([foreign long]))

(typedef List #:: ([foreign std::vector]))

(function (car lst) #:: 
  (foreign [type (for-all E (-> (<> List E) E))]))

(function (main x-lst y-lst) #:: (export)
  (var x #:: ([type Int]) (car x-lst))
  (var y #:: ([type Long]) (car y-lst))
  y)
