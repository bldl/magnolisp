#lang racket/base

#|

Routines for preserving syntax object location information and syntax
properties. Similar to quote-syntax/keep-srcloc, but this version
supports hash literals and allows one to choose what to preserve.

|#

(provide syntax-preserve
         keep-position? keep-position
         keep-syntax? keep-syntax
         keep-paren-shape? keep-paren-shape)

(require (for-template racket/base))

;; A selection of likely atomic key types for hashes.
(define (atom? x)
  (ormap
   (lambda (p?) (p? x))
   (list symbol? keyword? string? bytes? number? boolean? regexp?)))

(define (hash-maker-id-for x)
  (cond
   [(hash-eq? x) #'make-immutable-hasheq]
   [(hash-eqv? x) #'make-immutable-hasheqv]
   [(hash-equal? x) #'make-immutable-hash]
   [else
    (error
     'hash-maker-id-for
     "expected (or/c hash-eq? hash-eqv? hash-equal?): ~s" x)]))

(define (syntax-preserve keep? keep stx)
  (define (raise-hash-key-error h k)
    (error 'syntax-preserve
           "unsupported hash syntax literal: ~s\nkey = ~s\n~a"
           h k "only certain atomic keys are supported"))
  
  ;; Wraps any datum such that might be produced by syntax-e, so that
  ;; the result is an expression where the leaves have been
  ;; quote-syntax'ed as syntax literals. We only support hashes where
  ;; the keys are atoms (and not syntax thereof either).
  (define (wrap i n)
    (cond
     [(eq? i n)
      (let loop ([n n])
        (cond
         [(syntax? n) #`(quote-syntax #,n)]
         [(pair? n) #`(cons #,(loop (car n)) #,(loop (cdr n)))]
         [(box? n) #`(box-immutable #,(loop (unbox n)))]
         [(vector? n) #`(vector-immutable . #,(for/list ([i (in-vector n)])
                                                (loop i)))]
         [(prefab-struct-key n) =>
          (lambda (key)
            (define lst (cdr (vector->list (struct->vector n))))
            #`(make-prefab-struct '#,key #,@(map loop lst)))]
         [(hash? n)
          (define make (hash-maker-id-for n))
          #`(#,make (list #,@(for/list ([(k v) n])
                               (raise-hash-key-error n k)
                               #`(cons (quote #,k) #,(loop v)))))]
         [else #`(quote #,n)]))]
     [else n]))

  ;; Converts syntax to an expression that (when evaluated) produces
  ;; the same syntax, but also explicitly preserves metadata for
  ;; marshalling in bytecode.
  (define (convert e)
    (let loop ([e e])
      (cond
       [(pair? e)
        (define a (car e))
        (define new-a (loop a))
        (define b (cdr e))
        (define new-b (loop b))
        (if (and (eq? a new-a) (eq? b new-b))
            e
            #`(cons #,(wrap a new-a) #,(wrap b new-b)))]
       [(box? e)
        (define a (unbox e))
        (define new-a (loop a))
        (if (eq? a new-a)
            e
            #`(box-immutable #,(wrap a new-a)))]
       [(vector? e)
        (define changed? #f)
        (define new-lst
          (for/list ([v (in-vector e)])
            (define n-v (loop v))
            (if (eq? v n-v)
                v
                (begin
                  (set! changed? #t)
                  n-v))))
        (if (not changed?) 
            e
            #`(vector-immutable . #,(for/list ([i (in-vector e)]
                                               [n (in-list new-lst)])
                                      (wrap i n))))]
       [(prefab-struct-key e) =>
        (lambda (key)
          (define lst (cdr (vector->list (struct->vector e))))
          (define changed? #f)
          (define new-lst
            (for/list ([v lst])
              (define n-v (loop v))
              (if (eq? v n-v)
                  v
                  (begin
                    (set! changed? #t)
                    n-v))))
          (if (not changed?) 
              e
              #`(make-prefab-struct
                 '#,key
                 . #,(for/list ([i (in-list lst)]
                                [n (in-list new-lst)])
                       (wrap i n)))))]
       [(hash? e)
        (define changed? #f)
        (define n-lst
          (for/list ([(k v) e])
            ;; 'k' may or may not be syntax, 'v' always is. We only
            ;; support certain kinds of keys.
            (unless (atom? k)
              (raise-hash-key-error e k))
            (define n-v (loop v))
            (unless (eq? v n-v)
              (set! changed? #t))
            n-v))
        (if (not changed?)
            e
            #`(#,(hash-maker-id-for e)
               (list #,@(for/list ([(k v) e]
                                   [n-v n-lst])
                          #`(cons (quote #,k) #,(wrap v n-v))))))]
       [(syntax? e)
        (define v (syntax-e e))
        (define new-v (loop v))
        (if (and (eq? v new-v) (not (keep? e)))
            e
            (keep e (wrap v new-v)))]
       [else e])))

  (wrap stx (convert stx)))

(define (keep-position? stx)
  (and (syntax-position stx) #t))

;; As for quote-syntax/keep-srcloc.
(define (keep-position stx dat)
  #`(datum->syntax (quote-syntax #,(datum->syntax stx 'ctx))
                   #,dat
                   '#(#,(syntax-source stx)
                      #,(syntax-line stx)
                      #,(syntax-column stx)
                      #,(syntax-position stx)
                      #,(syntax-span stx))))

(define (keep-syntax? stx)
  (not (syntax? stx)))

;; Use this if not keep-position.
(define (keep-syntax stx dat)
  #`(datum->syntax (quote-syntax #,(datum->syntax stx 'ctx)) #,dat))

(define (keep-paren-shape? stx)
  (and (syntax-property stx 'paren-shape) #t))

;; As for quote-syntax/keep-srcloc.
(define (keep-paren-shape stx dat)
  #`(syntax-property #,dat 'paren-shape
                     '#,(syntax-property stx 'paren-shape)))

#|

This library is derived from the syntax/quote library of Racket
5.93.

Copyright (c) 2010-2013 PLT Design Inc.
              2014 Tero Hasu and University of Bergen

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This means that you can link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular means
that you must release the source code for the modified software.  See
share/COPYING_LESSER.txt (in Racket source) for more information.

|#
