#lang racket

#|

'define' with 'return' support.

E.g.

  (define/r (f x)
    (when (= x 0) (return x))
    (+ x 1))

  (define/r (g x y)
    (when (= x 0) (return x y))
    (values (+ x 1) (+ y 1)))

  (f 0) ; => 0
  (f 1) ; => 2
  (g 0 5) ; => 0 5
  (g 1 5) ; => 2 6

|#

(require "module.rkt")

(require racket/stxparam)

(define-syntax-parameter return
  (syntax-rules ()))
(provide return)

(define-syntax* define/r
  (syntax-rules ()
    ((define/r sig body ...)
     (define sig
       (let/cc k
               (syntax-parameterize
                ((return (syntax-rules ()
                           ((_ . rest)
                            (k . rest)))))
                body ...))))))

(define-syntax* define*/r
  (syntax-rules ()
    ((_ (name arg ... . rest) body ...)
     (begin
       (define/r (name arg ... . rest) body ...)
       (provide name)))
    ((_ (name arg ...) body ...)
     (begin
       (define/r (name arg ...) body ...)
       (provide name)))
    ))

#|

Copyright 2013 the authors.

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
