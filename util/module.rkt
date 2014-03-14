#lang racket/base

(require racket/contract racket/generic racket/match racket/stxparam
         (for-syntax racket/base racket/syntax
                     syntax/define syntax/parse))                     

(provide define* define-for-syntax* define-syntax*)

(define-syntax (define* stx)
  (syntax-case stx ()
    [(_ . rest)
     (begin
       (define-values (id rhs)
         (normalize-definition stx #'lambda #t #t))
       #`(begin
           (define #,id #,rhs)
           (provide #,id)))]))

(define-syntax (define-for-syntax* stx)
  (syntax-case stx ()
    [(_ . rest)
     (begin
       (define-values (id rhs)
         (normalize-definition stx #'lambda #t #t))
       #`(begin-for-syntax
           (define #,id #,rhs)
           (provide #,id)))]))

(define-syntax define-syntax*
  (syntax-rules ()
    ((_ (name stx) body ...)
     (begin
       (define-syntax (name stx) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define-syntax name body ...)
       (provide name)))))

(define-syntax* define-syntax-rule*
  (syntax-rules ()
    ((_ (name rest ...) body)
     (begin
       (define-syntax-rule (name rest ...) body)
       (provide name)))))

(define-syntax-rule*
  (define-syntax-parameter* id e)
  (begin
    (define-syntax-parameter id e)
    (provide id)))

(define-syntax-rule*
  (define-values* (n ...) vals)
  (begin
    (define-values (n ...) vals)
    (provide n ...)))

(define-syntax-rule*
  (concrete-struct* nm rest ...)
  (begin
    (struct nm rest ...)
    (provide (struct-out nm))))

(define-syntax-rule*
  (abstract-struct* nm rest ...)
  (begin
    (struct nm rest ... #:constructor-name ctor)
    (provide (except-out (struct-out nm) ctor))))

(define-syntax-rule*
  (singleton-struct* nm (the-nm e ...) rest ...)
  (begin
    (struct nm rest ... #:constructor-name ctor)
    (define the-nm (ctor e ...))
    (provide (except-out (struct-out nm) ctor) the-nm)))

(define-syntax-rule*
  (define-match-expander* id rest ...)
  (begin
    (define-match-expander id rest ...)
    (provide id)))

(define-syntax* define-with-contract
  (syntax-rules ()
    ((_ contract (name . rest) body ...)
     (define (name . rest) body ...))
    ((_ contract name value)
     (define name value))))

(define-syntax* define*-with-contract
  (syntax-rules ()
    ((_ contract (name . rest) body ...)
     (define* (name . rest) body ...))
    ((_ contract name value)
     (define* name value))))

(define-syntax* define-with-contract*
  (syntax-rules ()
    ((_ contract (name . rest) body ...)
     (begin
       (define (name . rest) body ...)
       (provide (contract-out [name contract]))))
    ((_ contract name value)
     (begin
       (define name value)
       (provide (contract-out [name contract]))))))

(define-syntax-rule*
  (require* n ...)
  (begin
    (require n ...)
    (provide (all-from-out n ...))))

(define-syntax* (define-generics* stx)
  (define-splicing-syntax-class opts
    (pattern
     (~seq (~or (~optional (~seq #:defaults _:expr))
                (~optional (~seq #:fast-defaults _:expr))
                (~optional (~seq #:fallbacks _:expr))
                (~optional (~seq #:defined-predicate _:id))
                (~optional (~seq #:defined-table _:id))
                (~seq #:derive-property _:expr _:expr)) ...)))
  (syntax-parse stx
    [(_ name pre:opts (meth:id . formals) ... post:opts)
     (with-syntax ([gen (format-id stx "gen:~a" #'name)]
                   [pred (format-id stx "~a?" #'name)]
                   [c (format-id stx "~a/c" #'name)])
       ;;#''(name gen pred c pre (meth . formals) ... post)
       #`(begin
           (define-generics name
             #,@(syntax->list #'pre)
             (meth . formals) ...
             #,@(syntax->list #'post))
           (provide gen pred c meth ...)))]))

#|

Copyright 2008 the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
