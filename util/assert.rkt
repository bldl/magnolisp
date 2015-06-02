#lang racket/base

#|

|#

(provide raise-assertion-error assert
         cond-or-fail case-or-fail)

(define (raise-assertion-error src fmt . v)
  (apply error src (string-append "assertion failed: " fmt) v))

(define-syntax-rule (assert e)
  (unless e
    (raise-assertion-error 'assert "~s" (quote e))))

(define-syntax cond-or-fail
  (syntax-rules (else)
    [(_ clause ... (else body ...))
     (cond clause ... (else body ...))]
    [(_ clause ...)
     (cond
      clause ...
      (else
       (raise-assertion-error
        'cond-or-fail "no matching `cond` clause")))]))

(define-syntax case-or-fail
  (syntax-rules (else)
    [(_ val-expr clause ... (else body ...))
     (case val-expr clause ... (else body ...))]
    [(_ val-expr clause ...)
     (let ([v val-expr])
       (case v
         clause ...
         (else
          (raise-assertion-error
           'case-or-fail 
           "no matching `case` clause for ~s" v))))]))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

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
