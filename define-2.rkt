#lang racket

#|

Utilities for defining dual behavior for macros, depending on which
core language is being targeted.

|#

(require "util.rkt"
         (for-syntax "settings.rkt"))

(define-syntax* define-syntax-rule*-2
  (syntax-rules ()
    ((_ (name rest ...) body-e body-c)
     (define-syntax* (name stx)
       (syntax-case stx ()
         ((_ rest ...)
          (if compile?
              #'body-c
              #'body-e)))))))

;; Syntax for macro implementation.
(begin-for-syntax
 (require "util.rkt"
          (for-syntax racket/base "settings.rkt"))

 ;; Syntax for use within a macro.
 ;; E.g.
 ;; (define-syntax m
 ;;  (syntax-rules-2 () ((_) 1 2) ((_ _) 3 4)))
 ;; (m) ; => 1
 ;; ((syntax-rules-2 () ((_) 1 2)) #'(5)) ; => #'1
 ;; ((syntax-rules-2 () ((_) 1 2) ((_ _) 3 4)) #'(5 x)) ; => #'3
 (define-syntax* (syntax-rules-2 stx)
   (syntax-case stx ()
     ((_ (kw ...) (pat body-e body-c) ...)
      (if compile?
          #'(syntax-rules (kw ...) (pat body-c) ...)
          #'(syntax-rules (kw ...) (pat body-e) ...)))))
 
 ) ;; end begin-for-syntax
