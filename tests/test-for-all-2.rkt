#lang magnolisp

(typedef Int #:: ([foreign int]))
(typedef String #:: ([foreign std::string]))

(typedef Box #:: (foreign))

(function (box v) #:: 
  (foreign [type (for-all E (-> E (<> Box E)))]))

(function (unbox box) #::
  (foreign [type (for-all E (-> (<> Box E) E))]))

(function (main x y) 
  #:: (export [type (-> Int String String)])
  (define boxed-x (box x))
  (define boxed-y (box y))
  (unbox boxed-y))
