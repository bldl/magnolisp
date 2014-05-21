#lang racket/base

#|

This module implements the (default) surface syntax of the Magnolisp
language, and such language is not meant to be used in macro
programming. This is because the runtime language must be restricted
enough to be easily analyzable, and compilable to C++.

|#

(require "util.rkt"
         racket/stxparam
         (for-syntax "app-util.rkt" "util.rkt"
                     racket/base racket/syntax syntax/parse)) 

;; A value not meant to be referenced. (Starting from Racket v6.0.1,
;; cannot use #<undefined>.)
(define undefined
  (let ()
    (struct undefined ())
    (undefined)))

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as
;; (#%magnolisp 'pass) or (#%magnolisp 'call p) or such.
(define* #%magnolisp (lambda args undefined))

;; Function type expression.
(define-syntax-rule* (fn at ... rt)
  (#%magnolisp 'fn at ... rt))

;; Type annotation.
(define-syntax-rule* (type t)
  (#%magnolisp 'anno 'type t))

(define-syntax* (export stx)
  (syntax-parse stx
    [_:id
     #'(#%magnolisp 'anno 'export #t)]
    [(_ name:id)
     #'(#%magnolisp 'anno 'export #'name)]))

(define-syntax* (foreign stx)
  (syntax-parse stx
    [_:id
     #'(#%magnolisp 'anno 'foreign #t)]
    [(_ name:id)
     #'(#%magnolisp 'anno 'foreign #'name)]))

(define-syntax* (build stx)
  (syntax-case stx ()
    [(_ x ...)
     #`(#%magnolisp 'anno 'build (quote-syntax #,stx))]))

(define-syntax* (let/annotate stx)
  (syntax-case stx ()
    [(_ (a ...) e)
     (let ()
       (define a-lst (syntax->list #'(a ...)))
       (define id-lst (map (lambda (a) (generate-temporary 'dummy)) a-lst))
       (define n-stx 
         (with-syntax ([(id ...) id-lst])
           (syntax/loc stx
             (let-values ([(id) a] ...) e))))
       (syntax-property n-stx 'annotate #t))]))

;; A form that annotates not an identifier, but any expression. The
;; annotations are stored as expressions in a `let` wrapper.
(define-syntax-rule*
  (anno a ... e)
  (let/annotate (a ...) e))

(define-for-syntax (decl-for-id id)
  (with-syntax ([impl-id (format-id id "~a-impl" (syntax-e id))]
                [id id])
    #'(define-syntax* id
        (syntax-rules ()
          [(_ n (#:annos a (... ...)) b (... ...))
           (impl-id n (a (... ...)) b (... ...))]
          [(_ n b (... ...))
           (impl-id n () b (... ...))]))))

;; For each passed ID, defines syntax with an optional #:annos
;; specifier. Each ID must have an -impl binding, which expects a
;; compulsory annotation listing at the same position.
(define-syntax* (define-annos-wrapper* stx)
  (syntax-case stx ()
    [(_ ids ...)
     (let ()
       (define id-lst (syntax->list #'(ids ...)))
       #`(begin #,@(map decl-for-id id-lst)))]))

(define-syntax function-impl
  (syntax-rules ()
    [(_ (f p ...) (a ...))
     (function-impl (f p ...) (a ...) (void))]
    [(_ (f p ...) (a ...) b)
     (define f 
       (let/annotate (a ...)
         (#%plain-lambda (p ...) b)))]))
    
(define-annos-wrapper* function)

(define-syntax var-impl
  (syntax-rules ()
    [(_ n (a ...) v)
     (define n
       (let/annotate (a ...)
         v))]))

(define-annos-wrapper* var)

(define-syntax-rule
  (let-var-impl n (a ...) v b ...)
  (let ([n (let/annotate (a ...) v)])
    b ...))

(define-annos-wrapper* let-var)

(define-syntax-rule*
  (lit-of t d)
  (let/annotate ([type t]) d))

(define-syntax typedef-impl
  (syntax-rules ()
    [(_ t (a ...))
     (define t 
       (let/annotate (a ...)
         (#%magnolisp 'foreign-type)))]))

(define-annos-wrapper* typedef)

(define-syntax* (let/local-ec stx)
  (syntax-case stx ()
    [(_ . rest)
     (syntax-property
      (syntax/loc stx (let/ec . rest))
      'local-ec #t)]))

(define-syntax* (apply/local-ec stx)
  (syntax-case stx ()
    [(_ k e)
     (syntax-property
      (syntax/loc stx (k e))
      'local-ec #t)]))

(define-syntax-parameter* return
  (syntax-rules ()))

(define-syntax-rule*
  (do body ...)
  (let/local-ec k
    (syntax-parameterize
     ([return
       (syntax-rules ()
         [(_ v) (apply/local-ec k v)])])
     body ...
     (values))))

(define-syntax* (begin-racket stx)
  (syntax-case stx ()
    [(_ e ...)
     (syntax-property
      (syntax/loc stx (let () e ...))
      'in-racket #t)]))

(define-syntax* (begin-for-racket stx)
  (syntax-case stx ()
    [(_ e ...)
     (syntax-property
      (syntax/loc stx (begin e ...))
      'in-racket #t)]))

(define-syntax-rule*
  (define-for-racket rest ...)
  (begin-for-racket (define rest ...)))