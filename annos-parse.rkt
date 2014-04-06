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

(define-with-contract*
  (-> identifier? hash? (or/c identifier? boolean?))
  (parse-export id-stx h)
  (define anno-stx (hash-ref h 'export #f))
  (and anno-stx (parse-cxx-name-anno anno-stx)))

;;; 
;;; C++ types
;;; 

(define (cxx-name-from-magnolisp-name-or-fail id-stx anno-stx)
  (define s (symbol->string (syntax-e id-stx)))
  (define cxx-s (string->maybe-cxx-id s))
  (unless cxx-s
    (raise-language-error
     #f "cannot derive C++ name from Magnolisp name"
     anno-stx))
  cxx-s)

;; xxx for now we only support C++ type names - to support a range of type specifiers, such as pointer types - although can already specify almost anything, even |char const *|
(define* (parse-cxx-type id-stx anno-stx)
  (syntax-parse anno-stx
    (_:id
     (define cxx-s (cxx-name-from-magnolisp-name-or-fail id-stx anno-stx))
     (syntaxed anno-stx CxxNameT
               (datum->syntax #f (string->symbol cxx-s))))
    ((_ name:id)
     (syntaxed #'name CxxNameT #'name))))

;;; 
;;; Magnolisp types
;;; 

(define* (parse-type anno-stx)
  (define (parse-name name-stx)
    (syntax-parse name-stx
      #:context anno-stx
      (name:id
       (syntaxed name-stx NameT #'name))))
  
  (syntax-parse anno-stx
    ((_ ((~datum fn) p-type ... r-type))
     (syntaxed (cdr (syntax-e anno-stx))
               FunT (map parse-name (syntax->list #'(p-type ...)))
               (parse-name #'r-type)))
    ((_ name:id)
     (parse-name #'name))))
