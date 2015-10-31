#lang magnolisp/2014

#|

Conditional compilation, #if ... #elif ... #endif style.

|#

(require (for-syntax racket/syntax))

(define-syntax (static-cond stx)
  (syntax-case stx (else)
    [(_) #'(begin)]
    [(_ [else e]) #'e]
    [(_ [c e] . more)
     (if (syntax-local-eval #'c)
         #'e
         #'(static-cond . more))]))

(begin-for-syntax
 (define on-harmattan #f)
 (define on-bb10 #f)
 (define on-sailfish #f)
 (define on-console #t))

(typedef World (#:annos foreign))

(function (init-qt-ui w) (#:annos (type (fn World World)) foreign)
  w)

(function (init-ncurses-ui w) (#:annos (type (fn World World)) foreign)
  w)

(function (init-any-ui w) #an(export ^(fn World World))
  (static-cond
   [(or on-bb10 on-harmattan on-sailfish)
    (init-qt-ui w)]
   [on-console
    (init-ncurses-ui w)]
   [else
    w]))

(init-any-ui 7)
