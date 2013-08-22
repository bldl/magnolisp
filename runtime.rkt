#lang racket

#|

This runtime implements Magnolisp language, and such language is not
meant to be used in macro programming. Hence, rather interestingly, we
want to specialize the generated code for its intended runtime.

We will want some support for declaring macros that have different
expansion choices.

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

If this approach turns out to lack sufficient power, then we must
allow our own core language, and make careful use of
'local-expand' (or similar) to avoid the macro expander getting
confused by our core language.

To record metadata for the compiler, we use code that runs in phase
level 1. Since it lives in phase 1, the respective module's
#%module-begin will be executed in the same phase, and will hence have
access to the information (via the same variables at the same phase
level).

|#

(require
 "annos.rkt" "compiler-metadata.rkt" "define-2.rkt" "util.rkt"
 (for-syntax "metadata-defs.rkt" "settings.rkt" "util.rkt")) 

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as (%core
;; 'pass) or (%core 'call p) or such.
(define* %core list)

(define (make-undefined)
  (letrec ((x x)) x))

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
       (parse-record-definfo! #'f stx)
       #'(define (f xs ...) b ...)))))

;; DEPRECATED
;; This is only intended for local variable declarations, as Magnolisp
;; may not end up having any other kind. As AnyT can take on any type,
;; local type inference will be able to unify it with the value type.
(define-syntax* (var stx)
  (syntax-case stx ()
    ((_ (n t))
     (if-not-compiling
      #'(define n undefined)
      #'(begin
          (define n undefined)
          (begin/save-type n t))))
    ((_ (n t) v)
     (if-not-compiling
      #'(define n v)
      #'(begin
          (define n v)
          (begin/save-type n t))))
    ((_ n v)
     (if-not-compiling
      #'(define n v)
      #'(begin
          (define n v)
          (begin/save-type n))))
    ))
