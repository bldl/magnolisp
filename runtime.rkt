#lang racket

#|

This runtime implements Magnolisp language, and such language is not
meant to be used in macro programming. This is because the runtime
language must be restricted enough to be easily analyzable, and
compilable to C++.

We cannot introduce new core language into Racket, and so we must be
able to express foreign syntax in terms of Racket syntax, without
using macros or runtime values. This is not really a problem since
unique identifiers for functions can be given here, and our syntax can
be expressed in terms of application of such functions. We use '%core'
as our special function. We can also record annotations for our
declared names, into a separate table.

We cannot do much quoting to make sure that we retain binding
information, and to make sure that macros get expanded. Again, no
problem, as we can use 'lambda' as a container for code.

Be mindful of not using the Magnolisp reader extensions here, since
this code is not in Magnolisp, only for Magnolisp.

|#

(require
 "annos-util.rkt" "annos-store.rkt" "util.rkt"
 racket/stxparam
 (for-syntax "util.rkt" racket/syntax)) 

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as (%core
;; 'pass) or (%core 'call p) or such.
(define* %core list)

(define (make-undefined)
  (letrec ((x x)) x))

;; #<undefined>
(define undefined (make-undefined))

;; A form that cooperates with the reader extension.
(define-syntax* (anno stx)
  (syntax-case stx ()
    ((_ n v e)
     (identifier? #'n)
     (add-anno #'e (syntax-e #'n) #'v #:from stx))))

;; This is to support annotation metaprogramming. You might want to
;; define macros that emit (anno! ...) forms for explicitly recording
;; annotations for some associated binding.
(define-syntax* (anno! stx)
  (syntax-case stx ()
    ((_ id a ...)
     (identifier? #'id)
     (let ()
       (define as (syntax->list #'(a ...)))
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
    ((_ (f p ...) (a ...))
     (function-impl (f p ...) (a ...) (void)))
    ((_ (f p ...) (a ...) b)
     (begin
       (define (f p ...) b)
       (anno! f a ...)))))
    
(define-annos-wrapper* function)

(define-syntax typedef-impl
  (syntax-rules ()
    ((_ t (a ...))
     (begin
       (define t (%core 'foreign-type))
       (anno! t a ...)))
    #; ((_ t (a ...) v) ;; xxx to be supported
     (begin
       (define t (%core 'type-alias v))
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
