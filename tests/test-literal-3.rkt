#lang magnolisp

(define #:type Symbol 
  #:: ([foreign MySymbolInCxx]
       [literal (display type-id) "(" (cxx-str datum) ")"]))

(define (symbol-identity s)
  #:: (^(-> Symbol Symbol))
  s)
  
(define (main) #:: (export)
  (symbol-identity 'my-symbol))

(main)
