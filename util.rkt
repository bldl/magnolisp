#lang racket

(require "util/module.rkt")
(provide (all-from-out "util/module.rkt"))

(require* "util/print.rkt")
(require* "util/let.rkt")

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

;; True iff any of the specified predicates matches the specified
;; datum. Implemented using a macro for efficiency.
(define-syntax-rule*
  (any-pred-holds p ... x)
  (or (p x) ...))

;; True iff all of the specified predicates match the specified
;; datum. Implemented using a macro for efficiency.
(define-syntax-rule*
  (every-pred-holds p ... x)
  (and (p x) ...))

(define-syntax-rule*
  (no-pred-holds p ... x)
  (not (or (p x) ...)))

(define-syntax-rule*
  (apply-values f-expr gen-expr)
  (call-with-values (thunk gen-expr) f-expr))

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

(define-syntax-rule* (matches? e pat ...)
  (match e (pat #t) ... (_ #f)))

;; Confusingly, 'resolve-module-path' does not appear to (always)
;; return a 'resolved-module-path?'. This predicate may be used
;; instead. It reflects the contract for the return value of
;; 'resolve-module-path'.
(define* (resolve-module-path-result? x)
  (matches? x
   (? path?)
   (? symbol?)
   (list 'submod (or (? path?) (? symbol?)) (? symbol?) ...)))

(define* (path-basename fn)
  (define-values (p f dir?) (split-path fn))
  f)

(define* (path-basename-as-string fn)
  (path->string (path-basename fn)))

(define* (path-drop-suffix f)
  (path-replace-suffix f ""))

(define* (path-basename-only fn)
  (path-drop-suffix (path-basename fn)))

(define* (path-basename-only-as-string fn)
  (path->string (path-basename-only fn)))

(define* (string-underscorify s)
  (regexp-replace* #rx"[-]" s "_"))

(define* (symbolic-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define-syntax-rule* (assert e)
  (unless e
    (error 'assert "assertion ~s failed" (quote e))))

(define* (hash-merge! h . others)
  (for ((other others))
    (for (([k v] other))
      (hash-set! h k v)))
  (void))

(define* (hash-merge h . others)
  (for ((other others))
    (for (([k v] other))
      (set! h (hash-set h k v))))
  h)

(define* (hash-set/assocs h assocs)
  (for ((p assocs))
    (set! h (hash-set h (car p) (cdr p))))
  h)

(define* (dict-empty? d)
  (= (dict-count d) 0))

(require (for-syntax syntax/for-body))

;; Different syntax from 'for' in that 'empty-expr' is an extra
;; expression for constructing an empty dictionary. This is required
;; as a dictionary is an abstract concept.
(define-syntax* (for/dict stx)
  (syntax-case stx ()
    ((_ empty-expr (clause ...) . body)
     (with-syntax ((original stx)
                   ([(pre-body ...) post-body]
                    (split-for-body stx #'body)))
       (syntax/loc stx
         (for/fold/derived
          original
          ((d empty-expr))
          (clause ...)
          pre-body ...
          (let-values (([k v] (begin . post-body)))
            (dict-set d k v))))))))

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
