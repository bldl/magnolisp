#lang racket/base

(require racket/contract racket/stxparam (for-syntax racket/base))

(provide define* define-for-syntax* define-syntax*)

(define-syntax define*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin
       (define name body ...)
       (provide name)))))

(define-syntax define-for-syntax*
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin-for-syntax
       (define (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin-for-syntax
       (define (name arg ...) body ...)
       (provide name)))
    ((_ name body ...)
     (begin-for-syntax
       (define name body ...)
       (provide name)))))

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
       (provide/contract [name contract])))
    ((_ contract name value)
     (begin
       (define name value)
       (provide/contract [name contract])))))

(define-syntax-rule*
  (require* n ...)
  (begin
    (require n ...)
    (provide (all-from-out n ...))))

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
