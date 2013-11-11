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

|#

(require
 "annos-util.rkt" "annos-store.rkt" "util.rkt"
 (for-syntax "util.rkt")) 

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

;; Does a superficial parse of an annotation key and value, returning
;; a key and syntax for the value.
(define-for-syntax* (anno->pair stx)
  (syntax-case stx ()
    (k
     (identifier? #'k)
     (values (syntax-e #'k) (syntax/loc stx #t)))
    ((k)
     (identifier? #'k)
     (values (syntax-e #'k) (syntax/loc stx #t)))
    ((k v)
     (identifier? #'k)
     (values (syntax-e #'k) #'v))
    ((k v ...)
     (identifier? #'k)
     (values (syntax-e #'k) #'(v ...)))))

;; This is to support annotation metaprogramming. You might want to
;; define macros that emit (anno! ...) forms for explicitly recording
;; annotations for some associated binding.
(define-syntax* (anno! stx)
  (syntax-case stx ()
    ((_ id k v ...)
     (and (identifier? #'id) (identifier? #'k))
     (let-values (((k-sym v-stx) (anno->pair #'(k v ...))))
       (set-definfo! #'id k-sym v-stx)
       (syntax/loc stx (void))))
    ((_ id (k v ...))
     #'(anno! id k v ...))))

;; A macro for recording a sequence of annotations for a single
;; binding. This can be useful within definition forms.
(define-syntax-rule* (anno-seq! id (a ...))
  (begin (anno! id a) ...))

(define-syntax-rule*
  (function as f (p ...) b ...)
  (begin
    (anno-seq! f as)
    (define (f p ...) b ...)))
