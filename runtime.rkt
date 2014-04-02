#lang racket/base

#|

This module implements the (default) concrete syntax of the Magnolisp
language, and such language is not meant to be used in macro
programming. This is because the runtime language must be restricted
enough to be easily analyzable, and compilable to C++.

|#

(require
 "annos-store.rkt" "util.rkt"
 racket/stxparam
 (for-syntax "annos-util.rkt" "util.rkt"
             racket/base racket/syntax)) 

;; #<undefined>
(define undefined (letrec ((x x)) x))

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as
;; (#%magnolisp 'pass) or (#%magnolisp 'call p) or such.
(define* #%magnolisp (lambda args undefined))

;; A form that annotates not an identifier, but any datum. The
;; annotations are stored as a list of syntax in a syntax property of
;; the datum.
(define-syntax* (anno stx)
  (syntax-case stx ()
    ((_ a ... e)
     (let ()
       (define as (syntax->list #'(a ...)))
       (unless as
         (raise-syntax-error
          #f "expected a list of annotations"
          stx #'(a ...)))
       (if (null? as)
           #'e
           (syntax-add-annos/stx-list #'e as #:from stx))))))

;; This is to support annotation metaprogramming. You might want to
;; define macros that emit (anno! ...) forms for explicitly recording
;; annotations for some associated binding.
(define-syntax* (anno! stx)
  (syntax-case stx ()
    ((_ id a ...)
     (identifier? #'id)
     (let ()
       (define as (syntax->list #'(a ...)))
       (unless as
         (raise-syntax-error
          #f "expected a list of annotations"
          stx #'(a ...)))
       (unless (null? as) (set-definfo! #'id as))
       (syntax/loc stx (begin))))))

(define-for-syntax (decl-for-id id)
  (with-syntax ((impl-id (format-id id "~a-impl" (syntax-e id)))
                (id id))
    #'(define-syntax* id
        (syntax-rules ()
          ((_ n (#:annos a (... ...)) b (... ...))
           (impl-id n (a (... ...)) b (... ...)))
          ((_ n b (... ...))
           (impl-id n () b (... ...)))))))

;; For each passed ID, defines syntax with an optional #:annos
;; specifier. Each ID must have an -impl binding, which expects a
;; compulsory annotation listing at the same position.
(define-syntax* (define-annos-wrapper* stx)
  (syntax-case stx ()
    ((_ ids ...)
     (let ()
       (define id-lst (syntax->list #'(ids ...)))
       #`(begin #,@(map decl-for-id id-lst))))))

(define-syntax function-impl
  (syntax-rules ()
    [(_ (f p ...) (a ...))
     (function-impl (f p ...) (a ...) (void))]
    [(_ (f p ...) (a ...) b)
     (begin
       (define f (#%plain-lambda (p ...) b))
       (anno! f a ...))]))
    
(define-annos-wrapper* function)

(define-syntax var-impl
  (syntax-rules ()
    ((_ n (a ...) v)
     (begin
       (define n v)
       (anno! n a ...)))))

(define-annos-wrapper* var)

(define-syntax-rule
  (let-var-impl n (a ...) v b ...)
  (let ((n v))
    (anno! n a ...)
    b ...))

(define-annos-wrapper* let-var)

(define-syntax* (lit-of stx)
  (syntax-case stx ()
    ((_ t d)
     (with-syntax ((n (generate-temporary 'lit)))
       (syntax/loc stx
         (let-var-impl n ((type t)) d n))))))

(define-syntax typedef-impl
  (syntax-rules ()
    ((_ t (a ...))
     (begin
       (define t (#%magnolisp 'foreign-type))
       (anno! t a ...)))))

(define-annos-wrapper* typedef)

(define-syntax* (let/local-ec stx)
  (syntax-case stx ()
    ((_ . rest)
     (syntax-property
      (syntax/loc stx (let/ec . rest))
      'local-ec #t))))

(define-syntax* (apply/local-ec stx)
  (syntax-case stx ()
    ((_ k e)
     (syntax-property
      (syntax/loc stx (k e))
      'local-ec #t))))

(define-syntax-parameter* return
  (syntax-rules ()))

(define-syntax-rule*
  (do body ...)
  (let/local-ec k
    (syntax-parameterize
     ((return
       (syntax-rules ()
         ((_ v) (apply/local-ec k v)))))
     body ...
     (values))))

(define-syntax* (begin-racket stx)
  (syntax-case stx ()
    ((_ e ...)
     (syntax-property
      (syntax/loc stx (let () e ...))
      'in-racket #t))))

(define-syntax* (begin-for-racket stx)
  (syntax-case stx ()
    ((_ e ...)
     (syntax-property
      (syntax/loc stx (begin e ...))
      'in-racket #t))))

(define-syntax-rule*
  (define-for-racket rest ...)
  (begin-for-racket (define rest ...)))
