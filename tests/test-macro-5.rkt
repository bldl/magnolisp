#lang magnolisp

#|

Conditional compilation, #if ... #elif ... #endif style.

|#

(require (for-syntax racket/syntax))

(define-syntax (static-cond stx)
  (syntax-case stx (else)
    [(_) #'(void)]
    [(_ [else e]) #'e]
    [(_ [c e] . more)
     (if (syntax-local-eval #'c)
         #'e
         #'(static-cond . more))]))

(begin-for-syntax
 (define on-harmattan #f)
 (define on-bb10 #f)
 (define on-sailfish #t)
 (define on-console #f))

(typedef World #:: (foreign))

(define (init-qt-ui w) #::
  ([type (-> World World)] foreign)
  w)

(define (init-ncurses-ui w)
  #:: ([type (-> World World)] foreign)
  w)

(define (init-any-ui w)
  #:: (export ^(-> World World))
  (static-cond
   [(or on-bb10 on-harmattan on-sailfish)
    (init-qt-ui w)]
   [on-console
    (init-ncurses-ui w)]
   [else
    w]))

(init-any-ui 7)
