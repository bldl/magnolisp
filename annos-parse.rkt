#lang racket

#|

Routines for parsing annotation values into AST nodes.

|#

(require "ast-magnolisp.rkt" "util.rkt")

(define (raise-anno-syntax-error id-stx anno-name v-expr
                                 #:sub-expr [sub-expr #f]
                                 #:expect [expect-s #f])
  (raise-syntax-error
   anno-name
   (format "annotation ~a of ~a~a"
           anno-name
           (syntax->datum id-stx)
           (if expect-s
               (format " (expected ~a)" expect-s) ""))
   v-expr
   sub-expr))

(define (anno->datum h id-stx anno-name default-v pred?
                     #:expect [expect-s #f])
  (define v-stx (hash-ref h anno-name #f))
  (define v default-v)
  (when v-stx
    (set! v (syntax->datum v-stx))
    (unless (pred? v)
      (raise-anno-syntax-error id-stx anno-name v-stx #:expect expect-s)))
  v)

(define (parse-sub-type id-stx stx t)
  (match t
    ((? symbol?) 
     (TypeName t))
    (else
     (raise-anno-syntax-error id-stx 'type stx
                              #:expect "type specifier"))))

;; There are no generic types, and also no structure type specifiers,
;; although it is possible to refer to a structure type by name.
;; There are also no first class operations, so an operation never
;; takes a operation type as an argument.
(define (parse-def-type id-stx stx)
  (define t (syntax->datum stx))
  (match t
    ((? symbol?) 
     (TypeName t))
    ((list 'fn ats ... rt) 
     (let ((p (fix parse-sub-type id-stx stx)))
       (FunT (map p ats) (p rt))))
    (else
     (raise-anno-syntax-error id-stx 'type stx
                              #:expect "type specifier"))))

;; Creates a DefInfo hasheq by parsing any annotations in syntax
;; properties. Missing information is given the default value,
;; typically #f. Unrecognized annotations are ignored, although we
;; could consider storing them in the DefInfo as they are, as syntax.
(define (parse-definfo id-stx def-stx h)
  (unless (identifier? id-stx)
    (raise-syntax-error #f "definition must be named by an identifier"
                        def-stx id-stx))
  
  (define type
    (let ((stx (hash-ref h 'type #f)))
      (cond
       ((not stx) AnyT)
       (else (parse-def-type id-stx stx)))))

  (make-hasheq
   `(
     (type . ,type)
     )))

(define* (parse-entry-point id-stx h)
  (define entry-point
    (anno->datum h id-stx 'export #f boolean?
                 #:expect "boolean literal"))
  entry-point)

