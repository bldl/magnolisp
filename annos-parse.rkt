#lang racket

#|

Routines for parsing annotation values into AST nodes.

|#

(require "ast-magnolisp.rkt" "compiler-util.rkt" "util.rkt")
(require syntax/parse)

;;; 
;;; utilities
;;; 

(define (anno->datum h id-stx anno-name default-v pred?
                     #:expect [expect-s #f])
  (define anno-stx (hash-ref h anno-name #f))
  (if (not anno-stx)
      default-v
      (syntax-case anno-stx ()
        (n
         (identifier? #'n)
         (begin
           (assert (eq? anno-name (syntax-e #'n)))
           #t))
        ((_ v-pat)
         (begin
           (define v-stx #'v-pat)
           (define v (syntax->datum v-stx))
           (unless (pred? v)
             (raise-language-error
              anno-name
              (cond
               (expect-s
                (format "expected ~a value" expect-s))
               ((object-name pred?) =>
                (lambda (x)
                  (format "expected ~a value" x)))
               (else
                "illegal value"))
              anno-stx v-stx
              #:continued
              (format "(annotation of definition ~a)" (syntax-e id-stx))))
           v))
        (_
         (raise-language-error
          anno-name
          "expected a single value for annotation"
          anno-stx
          #:continued
          (format "(annotation of definition ~a)" (syntax-e id-stx)))))))

;;; 
;;; entry point flag
;;; 

(define* (parse-entry-point id-stx h)
  (define entry-point
    (anno->datum h id-stx 'export #f boolean?
                 #:expect "boolean literal"))
  entry-point)

;;; 
;;; C++ types
;;; 

(define-syntax-class cxx-id
  #:description "C++ identifier"
  (pattern name:id
           #:fail-unless (string-cxx-id?
                          (symbol->string
                           (syntax-e #'name))) #f))

;; xxx for now we only support C++ type names - to support a range of type specifiers, such as pointer types
(define* (parse-cxx-type id-stx anno-stx)
  (syntax-parse anno-stx
    (name:id
     (define s (symbol->string (syntax-e id-stx)))
     (define cxx-s (string->maybe-cxx-id s))
     (unless cxx-s
       (raise-language-error
        #f "cannot derive C++ name from Magnolisp name"
        anno-stx))
     (syntaxed #'name CxxNameT
               (datum->syntax #f (string->symbol cxx-s))))
    ((_ name:cxx-id)
     (syntaxed #'name CxxNameT #'name))))

;;; 
;;; Magnolisp types
;;; 

(define* (parse-type id-stx anno-stx)
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
