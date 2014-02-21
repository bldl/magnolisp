#lang racket/base

#|

Routines for preserving syntax object location information and
specified syntax properties. Very similar to quote-syntax/keep-srcloc.

|#

(require (for-syntax racket/base))

(provide quote-syntax/keep-properties
         (for-syntax keep-properties))

(begin-for-syntax

  (define (syntax-for v)
    (let loop ([n v])
      (cond
       [(syntax? n) #`(quote-syntax #,n)]
       [(pair? n) #`(cons #,(loop (car n)) #,(loop (cdr n)))]
       [(box? n) #`(box #,(loop (unbox n)))]
       [(vector? n) #`(vector . #,(for/list ([i (in-vector n)])
                                    (loop i)))]
       [(prefab-struct-key n)
        #`(make-prefab-struct '#,(prefab-struct-key n)
                              . #,(for/list ([i (in-list (cdr (vector->list 
                                                               (struct->vector n))))])
                                    (loop i)))]
       [else #`(quote #,n)])))

  (define (convert e keep? keep)
    (let loop ([e e])
      (cond
       [(pair? e)
        (define a (car e))
        (define new-a (loop a))
        (define b (cdr e))
        (define new-b (loop b))
        (if (and (eq? a new-a) (eq? b new-b))
            e
            #`(cons #,new-a #,new-b))]
       [(vector? e)
        (define new-vec (for/list ([i (in-vector e)])
                          (loop i)))
        (if (for/and ([i (in-vector e)]
                      [n (in-list new-vec)])
              (eq? i n))
            e
            #`(vector . #,new-vec))]
       [(prefab-struct-key e)
        (define l (cdr (vector->list (struct->vector e))))
        (define new-l (for/list ([i (in-list l)])
                        (loop i)))
        (if (for/and ([i l]
                      [n new-l])
              (eq? i n))
            e
            #`(make-prefab-struct '#,(prefab-struct-key e) . #,new-l))]
       [(box? e)
        (define a (unbox e))
        (define new-a (loop a))
        (if (eq? a new-a)
            e
            #`(box #,new-a))]
       [(syntax? e)
        (define v (syntax-e e))
        (define new-v (loop v))
        (define has-pos? (syntax-position e))
        (define has-props? (keep? e))
        (if (and (eq? v new-v) (not has-pos?) (not has-props?))
            e
            (let ([s 
                   (if has-pos?
                       #`(datum->syntax (quote-syntax #,(datum->syntax e 'ctx))
                                         #,new-v
                                         `#(#,(syntax-source e)
                                            #,(syntax-line e)
                                            #,(syntax-column e)
                                            #,(syntax-position e)
                                            #,(syntax-span e)))
                       #`(datum->syntax (quote-syntax #,(datum->syntax e 'ctx))
                                        #,new-v))])
              (when has-props?
                (set! s (keep s e)))
              s))]
       [else #`(quote #,e)])))

  (define (keep-properties e-stx #:properties [ps '(paren-shape)])
    (define (keep? stx)
      (ormap (lambda (n) (syntax-property stx n)) ps))
    (define (keep e-stx p-stx)
      (for/fold ([stx e-stx]) ([n ps])
        (define v (syntax-property p-stx n))
        (if v
            #`(syntax-property #,stx '#,n #,(syntax-for v))
            stx)))
    (convert e-stx keep? keep))

  ) ;; begin-for-syntax

(define-syntax (quote-syntax/keep-properties stx)
  (syntax-case stx ()
    [(_ e (sym ...))
     (keep-properties #'e #:properties (syntax->datum #'(sym ...)))]
    [(_ e)
     (keep-properties #'e)]))

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
