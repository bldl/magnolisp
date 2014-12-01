#lang racket/base

(require racket/dict racket/match (for-syntax racket/base))

(require "util/module.rkt")
(provide (all-from-out "util/module.rkt"))

(require* "util/print.rkt")
(require* "util/let.rkt")

(define-syntax* fix
  (syntax-rules ()
    ((_ fn arg ...)
     (lambda rest (apply fn arg ... rest)))))

(define* (compose1-> . fs)
  (apply compose1 (reverse fs)))

(define* (thread1-> v . fs)
  ((apply compose1-> fs) v))

;; As in Carl Eastlund's Mischief.
(define-syntax-rule* (values-of e)
  (call-with-values
      (lambda () (#%expression e))
    list))

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

(define-syntax-rule*
  (define-values-from-list (id ...) lst-expr)
  (define-values (id ...) (apply values lst-expr)))

(define-syntax-rule*
  (define-values-from-cons (id-1 id-2) cons-expr)
  (define-values (id-1 id-2)
    (let ((x cons-expr))
      (values (car x) (cdr x)))))

(define-syntax-rule* (matches? e pat ...)
  (match e (pat #t) ... (_ #f)))

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

(define* (raise-assertion-error src fmt . v)
  (apply error src (string-append "assertion failed: " fmt) v))

(define-syntax-rule* (assert e)
  (unless e
    (raise-assertion-error 'assert "~s" (quote e))))

(define-syntax* cond-or-fail
  (syntax-rules (else)
    [(_ clause ... (else body ...))
     (cond clause ... (else body ...))]
    [(_ clause ...)
     (cond
      clause ...
      (else
       (raise-assertion-error
        'cond-or-fail "no matching 'cond' clause")))]))

(define-syntax* case-or-fail
  (syntax-rules (else)
    [(_ val-expr clause ... (else body ...))
     (case val-expr clause ... (else body ...))]
    [(_ val-expr clause ...)
     (let ((v val-expr))
       (case v
         clause ...
         (else
          (raise-assertion-error
           'case-or-fail 
           "no matching 'case' clause for ~s" v))))]))

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

(define* (hash-merge-2 h-1 h-2)
  (cond
   ((hash-empty? h-1) h-2)
   ((hash-empty? h-2) h-1)
   (else (hash-merge h-1 h-2))))

(define* (hash-set/assocs h assocs)
  (for ((p assocs))
    (set! h (hash-set h (car p) (cdr p))))
  h)

(define* (list-map-last f lst)
  (when (null? lst)
    (error 'list-map-last "expected non-empty list"))
  (define r '())
  (let loop ((h (car lst))
             (t (cdr lst)))
    (if (null? t)
        (set! r (cons (f h) r))
        (begin
          (set! r (cons h r))
          (loop (car t) (cdr t)))))
  (reverse r))

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
