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
as our special function.

We cannot do much quoting to make sure that we retain binding
information, and to make sure that macros get expanded. Again, no
problem, as we can use 'lambda' as a container for code.

If this approach turns out to lack sufficient power, then we must
allow our own core language, and make careful use of
'local-expand' (or similar) to avoid the macro expander getting
confused by our core language.

To record metadata for the compiler, we use begin-for-syntax to
produce code that runs in phase level 1. Since it lives in phase 1,
the respective module's #%module-begin will be executed in the same
phase, and will hence have access to the information (via the same
variables at the same phase level).

|#

(require
 "define-2.rkt" "util.rkt"
 (for-syntax "compiler-metadata.rkt" "settings.rkt")) 

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as (%core
;; 'pass) or (%core 'call p) or such.
(define* %core list)

(define (make-undefined)
  (letrec ((x x)) x))

(define undefined (make-undefined))

;; Do nothing. Do not think we actually need new core language for this.
(define-syntax-rule*-2 (pass)
  (void)
  (%core 'pass))

;; Evaluate twice, presumably for side effects.
(define-syntax-rule* (twice x)
  (begin x x))

;; This is only intended for local variable declarations. As AnyT can
;; take on any type, local type inference will be able to unify it
;; with the value type.
(define-syntax* (var stx)
  (syntax-case stx ()
    ((_ (n t))
     (if-not-compiling
      #'(define n undefined)
      #`(begin
          ;; xxx begin-for-syntax can only be used at module level, and we must instead use local-begin-for-syntax
          (begin-for-syntax
           (record-type! #'n (TypeName 't)))
          (define n undefined))))
    ((_ (n t) v)
     (if-not-compiling
      #'(define n v)
      #`(begin
          (begin-for-syntax
           (record-type! #'n (TypeName 't)))
          (define n v))))
    ((_ n v)
     (if-not-compiling
      #'(define n v)
      #`(begin
          (begin-for-syntax
           (record-type! #'n AnyT))
          (define n v))))
    ))
