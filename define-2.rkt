#lang racket

#|

Utilities for defining dual behavior for macros, depending on which
core language is being targeted.

|#

(require "util.rkt"
         (for-syntax "settings.rkt"))

;; begin-for-syntax cannot be used in a local context. This variant of
;; it may be used in both local and module-level contexts. Note that
;; this probably eventually gets translated to let-syntaxes, when in a
;; local context. This is also of limited use, as local macros do not
;; get preserved in compiled code.
(define-syntax-rule*
  (local-begin-for-syntax e ...)
  (define-syntaxes ()
    (begin
      e ...
      (values))))

(define-syntax* (if-compiling/rt stx)
  (syntax-case stx ()
    ((_ t e)
     (if compile? #'t #'e))))

(define-syntax* (if-not-compiling/rt stx)
  (syntax-case stx ()
    ((_ t e)
     (if (not compile?) #'t #'e))))

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

 (define-syntax-rule* (if-compiling t e)
   (if compile? t e))

 (define-syntax-rule* (if-not-compiling t e)
   (if (not compile?) t e))

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
