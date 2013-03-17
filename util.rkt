#lang racket

(require "util/module.rkt")
(provide (all-from-out "util/module.rkt"))

(require* "util/print.rkt")
(require* "util/let.rkt")
;;(require* "util/return.rkt")

(define-syntax-rule* (if-not c t e)
  (if (not c) t e))

(define-syntax* lets
  (syntax-rules (then then-if then-if-not
                 then-let then-if-let then-if-not-let)
    ((_ then e) e)
    ((_ then e rest ...) (begin e (lets rest ...)))
    ((_ then-if e) e)
    ((_ then-if e rest ...) (and e (lets rest ...)))
    ((_ then-if-not e) (not e))
    ((_ then-if-not e rest ...) (and (not e) (lets rest ...)))
    ((_ then-let n e rest ...) (let ((n e)) (lets rest ...)))
    ((_ then-if-let n e rest ...) (let ((n e)) (and n (lets rest ...))))
    ((_) (void))
    ((_ rest ...) (begin rest ...))))

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

;; Like 'map', but if 'f' returns a value indicating failure, then
;; stops and returns #f. By default any false value indicates failure.
;; Does not accept multiple list arguments.
(define* (map-while f lst (failed? false?))
  (let next ((res-lst '())
             (lst lst))
    (if (null? lst)
        (reverse res-lst)
        (let* ((elem (car lst))
               (res (f elem)))
          (if (failed? res)
              #f
              (next (cons res res-lst) (cdr lst)))))))

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
