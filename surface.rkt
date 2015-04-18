#lang racket/base

#|

This module implements the (default) syntactic forms of the Magnolisp
language.

|#

(require "core.rkt" "util.rkt"
         racket/stxparam
         (for-syntax "app-util.rkt" "util.rkt"
                     racket/base racket/syntax 
                     syntax/parse)) 

;; Aliases.
(provide (rename-out [exists ∃] [for-all ∀]))

;; Function type expression.
(define-syntax-rule* (-> at ... rt)
  (CORE 'fn at ... rt))

;; ∃t,... u type expression, where `t` are type parameter names, and
;; `u` is a type expression.
(define-syntax-rule* (exists t ... u)
  (CORE 'exists (let ((t #f) ...) u)))

;; ∀t,... u type expression, where `t` are type parameter names, and
;; `u` is a type expression.
(define-syntax-rule* (for-all t ... u)
  (CORE 'for-all (let ((t #f) ...) u)))

;; `t` is a type name, and each `u` is a parameter type expression.
(define-syntax-rule* (<> t u ...)
  (CORE 'parameterized t u ...))

;; Type annotation.
(define-syntax-rule* (type t)
  (CORE 'anno 'type t))

(define-syntax* (export stx)
  (syntax-parse stx
    [_:id
     #'(CORE 'anno 'export #t)]
    [(_ name:id)
     #'(CORE 'anno 'export #'name)]))

(define-syntax* (foreign stx)
  (syntax-parse stx
    [_:id
     #'(CORE 'anno 'foreign #t)]
    [(_ name:id)
     #'(CORE 'anno 'foreign #'name)]))

(define-syntax* (literal stx)
  (syntax-case stx ()
    [(_ ...)
     #`(CORE 'anno 'literal (quote-syntax #,stx))]))

(define-syntax* (build stx)
  (syntax-case stx ()
    [(_ ...)
     #`(CORE 'anno 'build (quote-syntax #,stx))]))

(define-syntax* (expected stx)
  (syntax-case stx ()
    [(_ x ...)
     #'(CORE 'anno 'expected (quote-syntax (x ...)))]))

;; A form that annotates not an identifier, but any expression.
(define-syntax* (let-annotate stx)
  (syntax-case stx ()
    [(_ () e)
     #'e]
    [(_ (a ...) e)
     (syntax-property 
      (syntax/loc stx
        (let-values ([() (begin a (values))] ...) e))
      'annotate #t)]))

(define-syntax-rule*
  (cast t d)
  (let-annotate ([type t]) d))

(begin-for-syntax
  (define-splicing-syntax-class maybe-annos
    #:description "annotations for a definition"
    #:attributes (bs)
    (pattern
     (~optional
      (~seq #:: (~and (a:expr ...) as)))
     #:attr bs (if (attribute as) #'as #'()))))

(define-syntax-rule* (abstract-type)
  (CORE 'foreign-type))

(provide (rename-out [my-define define]))

(define-syntax (my-define stx)
  (syntax-parse stx
    [(_ n:id as:maybe-annos v:expr)
     #'(define n
         (let-annotate as.bs 
             v))]
    [(_ (f:id p:id ...) as:maybe-annos)
     #'(define f
         (let-annotate as.bs
             (#%plain-lambda (p ...) (void))))]
    [(_ (f:id p:id ...) as:maybe-annos b:expr ...+)
     #'(define f
         (let-annotate as.bs 
             (#%plain-lambda (p ...) b ...)))]
    [(_ #:type t:id as:maybe-annos)
     #'(define t 
         (let-annotate as.bs 
             (abstract-type)))]
    [(_ #:function f:id as:maybe-annos)
     (with-syntax ([body 
                    (syntax-property
                     #'(#%plain-lambda _ (void))
                     'for-target 'racket)])
       #'(define f
           (let-annotate as.bs 
               body)))]
    ))

;; DEPRECATED
(define-syntax* (function stx)
  (syntax-parse stx
    [(_ (f:id p:id ...) as:maybe-annos)
     #'(define f
         (let-annotate as.bs
             (#%plain-lambda (p ...) (void))))]
    [(_ (f:id p:id ...) as:maybe-annos b:expr ...+)
     #'(define f
         (let-annotate as.bs 
             (#%plain-lambda (p ...) b ...)))]))

;; DEPRECATED
(define-syntax* (var stx)
  (syntax-parse stx
    [(_ n:id as:maybe-annos v:expr)
     #'(define n
         (let-annotate as.bs 
             v))]))

;; DEPRECATED
(define-syntax* (typedef stx)
  (syntax-parse stx
    [(_ t:id as:maybe-annos)
     #'(define t 
         (let-annotate as.bs 
             (abstract-type)))]))

(define-syntax* (declare stx)
  (syntax-parse stx
    [(_ n:id e:expr)
     #'(define-values ()
         (begin
           (CORE 'declare n e)
           (values)))]
    [(_ (f:id p:id ...) as:maybe-annos)
     #'(declare f
         (let-annotate as.bs
             (#%plain-lambda (p ...) (void))))]
    [(_ #:type t:id as:maybe-annos)
     #'(declare t 
         (let-annotate as.bs 
             (abstract-type)))]
    ))

(define-syntax* (let/local-ec stx)
  (syntax-parse stx
    [(_ . rest)
     (syntax-property
      (syntax/loc stx (let/ec . rest))
      'local-ec #t)]))

(define-syntax* (app/local-ec stx)
  (syntax-parse stx
    [(_ k:id e:expr)
     (syntax-property
      (syntax/loc stx (k e))
      'local-ec #t)]))

(define-syntax-parameter* return
  (syntax-rules ()))

(define-syntax-rule*
  (begin-return body ...)
  (let/local-ec k
    (syntax-parameterize
     ([return
       (syntax-rules ()
         [(_ v) (app/local-ec k v)])])
     body ...)))

(define-syntax* (if-target stx)
  (syntax-parse stx
    [(_ name:id t:expr e:expr)
     (syntax-property 
      #'(if #f t e)
      'if-target (syntax-e #'name))]))

(define-syntax-rule* (if-cxx t e)
  (if-target cxx t e))

(define-syntax (flag-as-for-racket stx)
  (syntax-parse stx
    [(_ form)
     (syntax-property #'form 'for-target 'racket)]))

(define-syntax-rule* (begin-racket form ...)
  (flag-as-for-racket (begin form ...)))

(define-syntax-rule* (let-racket e ...)
  (flag-as-for-racket (let () e ...)))

(define-syntax* (let-racket/require stx)
  (define-syntax-class sym-spec
    #:description "import spec for `let-racket/require`"
    #:attributes (spec)
    (pattern
     (sym:id ...)
     #:attr spec (with-syntax ((rb (datum->syntax stx 'racket/base)))
                   #'(only-in rb sym ...)))
    (pattern
     (sym:id ... #:from mp:expr)
     #:attr spec #'(only-in mp sym ...)))
  
  (syntax-parse stx
    ((_ (req:sym-spec ...) e:expr ...+)
     #'(let-racket
        (local-require req.spec ...)
        e ...))))

(define-syntax* primitives
  (syntax-rules (::)
    [(_) (begin)]
    [(_ [#:type t] . more)
     (begin 
       (typedef t #:: (foreign))
       (primitives . more))]
    [(_ [#:function form :: t] . more)
     (begin 
       (my-define form #:: ([type t] foreign))
       (primitives . more))]))
