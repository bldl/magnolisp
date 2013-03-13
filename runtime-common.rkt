#lang racket/base

#|

There are differences in how much we want to macro expand depending on
whether we are interpreting or compiling. For interpreting we want
everything expanded into Racket, whereas we can leave in more syntax
for the purposes of translating into high-level C++. Here we have the
commonalities.

|#

(require "util.rkt")

(provide begin-for-syntax
         define-for-syntax
         define-syntax
         define-syntax-rule
         provide
         require)

;;; 
;;; declarations
;;; 

;;; 
;;; statements
;;; 

