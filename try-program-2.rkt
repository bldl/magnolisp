#lang magnolisp

#|

The compiler ignores top-level expressions and statements, but the
interpreter evaluates them and displays the results. This behavior is
useful for a "simulator" environment.

|#

^int #^(doc "one") #^(one 1) 1

(pass)
