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

|#

(require
 (for-syntax "compiler-metadata.rkt")
 (for-syntax "settings.rkt")
 "util.rkt")

;; Yes we are providing this. If the programmer wants to hack our core
;; language, they may. The idea is to express core language as (%core
;; 'pass) or (%core 'call p) or such.
(define* %core list)

(define-syntax define-syntax-rule*-2
  (syntax-rules ()
    ((_ (name rest ...) body-e body-c)
     (define-syntax* (name stx)
       (syntax-case stx ()
         ((_ rest ...)
          (if compile?
              #'body-c
              #'body-e)))))))

(begin-for-syntax
 (require (for-syntax racket/base "settings.rkt"))

 ;; Syntax for use within a macro.
 ;; E.g.
 ;; (define-syntax m
 ;;  (syntax-rules-2 () ((_) 1 2) ((_ _) 3 4)))
 ;; (m) ; => 1
 ;; ((syntax-rules-2 () ((_) 1 2)) #'(5)) ; => #'1
 ;; ((syntax-rules-2 () ((_) 1 2) ((_ _) 3 4)) #'(5 x)) ; => #'3
 (define-syntax (syntax-rules-2 stx)
   (syntax-case stx ()
     ((_ (kw ...) (pat body-e body-c) ...)
      (if compile?
          #'(syntax-rules (kw ...) (pat body-c) ...)
          #'(syntax-rules (kw ...) (pat body-e) ...)))))
 
 ) ;; end begin-for-syntax

(define (make-undefined)
  (define x x)
  x)

(define undefined (make-undefined))

;; Do nothing. Do not think we actually need new core language for this.
(define-syntax-rule*-2 (pass)
  (void)
  (%core 'pass))

;; Evaluate twice, presumably for side effects.
(define-syntax-rule* (twice x)
  (begin x x))

;; xxx Needs to save type information, and must still create a Racket
;; binding.
#;
(define-syntax* var
  (syntax-rules-2 ()
                  ((_ (n t))
                   (define n undefined)
                   (%core 'var n t))))