#lang magnolisp

(typedef Int #:: ([foreign int]))
(typedef String #:: ([foreign std::string]))
(typedef Map #:: (foreign))

(function (c?) #:: (foreign ^(-> Bool)))

(function (Map-new) #:: (foreign))

(function (Map-put! h k v)
  #:: ([type (-> (<> Map Int String) Int String Void)] foreign))

(function (main k v) #:: (export)
  (define h (Map-new))
  (when (c?)
    (Map-put! h k v))
  h)
