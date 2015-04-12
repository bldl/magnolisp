#lang magnolisp

(typedef Int #:: ([foreign int]))
(typedef Long #:: ([foreign long]))

(typedef Maybe #:: (foreign))

(function (just v) 
  #:: (foreign [type (for-all E (-> E (<> Maybe E)))]))

(function (nothing) 
  #:: (foreign [type (for-all E (-> (<> Maybe E)))]))

(function (just? m)
  #:: (foreign [type (for-all E (-> (<> Maybe E) Bool))]))

(function (nothing? m)
  #:: (foreign [type (for-all E (-> (<> Maybe E) Bool))]))

(function (just-get m)
  #:: (foreign [type (for-all E (-> (<> Maybe E) E))]))

(function (main)
  #:: (export)
  (define x (cast Int 5))
  (define y (cast Long 6))
  (define maybe-x (just x))
  (define maybe-y (nothing))
  (when (just? maybe-x)
    (set! maybe-y (just y)))
  (when (nothing? maybe-y)
    (set! maybe-x (just 7)))
  (set! maybe-x (nothing))
  (just-get maybe-y))
