#lang racket

#|

Routines for parsing annotation values into AST nodes.

|#

(require "ast-magnolisp.rkt" "compiler-util.rkt" "util.rkt")

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
