#lang racket/base

#|

Routines for parsing annotation values into AST nodes.

|#

(require "ast-magnolisp.rkt" "app-util.rkt" "util.rkt"
         racket/contract syntax/parse)

;;; 
;;; entry point flag
;;; 

(define-syntax-class cxx-id
  #:description "C++ identifier"
  (pattern name:id
           #:fail-unless (string-cxx-id?
                          (symbol->string
                           (syntax-e #'name))) #f))

(define* (parse-cxx-name-anno anno-stx)
  (syntax-parse anno-stx
    (_:id
     #t)
    ((_ name:cxx-id)
     #'name)))

;;; 
;;; C++ type
;;; 

(define (cxx-name-from-magnolisp-name-or-fail id-ast anno-stx)
  (define s (symbol->string (Id-name id-ast)))
  (define cxx-s (string->maybe-cxx-id s))
  (unless cxx-s
    (raise-language-error
     #f "cannot derive C++ name from Magnolisp name"
     anno-stx))
  cxx-s)

;; xxx for now we only support C++ type names - to support a range of type specifiers, such as pointer types - although can already specify almost anything, even |char const *|
(define-with-contract*
  (-> Id? syntax? Type?)
  (parse-cxx-type id-ast anno-stx)
  
  (syntax-parse anno-stx
    (_:id
     (define cxx-s (cxx-name-from-magnolisp-name-or-fail id-ast anno-stx))
     (syntaxed anno-stx CxxNameT
               (datum->syntax #f (string->symbol cxx-s))))
    ((_ name:id)
     (syntaxed #'name CxxNameT #'name))))
