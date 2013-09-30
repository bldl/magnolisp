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
 "annos.rkt" "definfo-store.rkt" "util.rkt"
 (for-syntax "util.rkt")) 

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as (%core
;; 'pass) or (%core 'call p) or such.
(define* %core list)

(define (make-undefined)
  (letrec ((x x)) x))

;; #<undefined>
(define undefined (make-undefined))

;; Annotated form 'e'. We collect annotations into a syntax property
;; of the annotated form. This at least gets uninteresting unnotations
;; out of the way. Even the annotating form could itself be annotated,
;; and the annotations must be collated. Inner annotation takes
;; precedence in case of conflict. Inner corresponds to later, when it
;; comes the reader producing annotation forms.
(define-syntax* (anno stx)
  (syntax-case stx ()
    ((_ n v e)
     (identifier? #'n)
     (add-anno #'e (syntax-e #'n) #'v #:from stx))))

(define-syntax* (function stx)
  (syntax-case stx ()
    ((_ (f xs ...) b ...)
     (begin
       (record-definfo! #'f stx)
       #'(define (f xs ...) b ...)))))
