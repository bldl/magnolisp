#lang racket/base

#|

A 'block-expr' expression/statement with 'return' support. Any
value(s) produced by a block must always be returned. In a statement
context, no values should be returned, and fall-through is OK. In an
expression context, the number of values returned should match the
context.

|#

(require racket/stxparam)

(provide block-expr return)

(define-syntax-parameter return
  (syntax-rules ()))

(define-syntax-rule
  (block-expr body ...)
  (let/ec k
    (syntax-parameterize
     ((return (syntax-rules ()
                ((_ . rest)
                 (k . rest)))))
     body ...
     (values))))

(module+ main
  (block-expr) ;; statement
  (block-expr (return) 1) ;; statement
  (define-values () (block-expr)) ;; expression, 0 values
  (define-values () (block-expr (return))) ;; expression, 0 values
  (block-expr (return 1)) ;; expression, 1 value
  (block-expr (return 1 2)) ;; expression, 2 values
  (define (f x)
    (block-expr
     (when x
       (return 'true))
     (unless x
       (return 'false))))
  (f #t)
  (f #f)
  )

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
