#lang racket/base

#|
|#

(require (for-syntax racket/base))

(provide quote-syntax/keep-srcloc)

(define-syntax (quote-syntax/keep-srcloc stx)
  (define (wrap i n)
    (cond
     [(eq? i n) (let loop ([n n])
                  (cond
                   [(syntax? n) #`(quote-syntax #,n)]
                   [(pair? n) #`(cons #,(loop (car n))
                                      #,(loop (cdr n)))]
                   [(box? n) #`(box #,(loop (unbox n)))]
                   [(vector? n) #`(vector . #,(for/list ([i (in-vector n)])
                                                (loop i)))]
                   [(prefab-struct-key n)
                    #`(make-prefab-struct '#,(prefab-struct-key n)
                                          . #,(for/list ([i (in-list (cdr (vector->list 
                                                                           (struct->vector n))))])
                                                (loop i)))]
                   [else #`(quote #,n)]))]
     [else n]))
  (define (convert e src-stx)
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
       [(vector? e)
        (define new-vec (for/list ([i (in-vector e)])
                          (loop i)))
        (if (for/and ([i (in-vector e)]
                      [n (in-list new-vec)])
              (eq? i n))
            e
            #`(vector . #,(for/list ([i (in-vector e)]
                                     [n (in-list new-vec)])
                            (wrap i n))))]
       [(prefab-struct-key e)
        (define l (cdr (vector->list (struct->vector e))))
        (define new-l (for/list ([i (in-list l)])
                        (loop i)))
        (if (equal? l new-l)
            e
            #`(make-prefab-struct '#,(prefab-struct-key e)
                                  . #,(for/list ([i (in-list l)]
                                                 [n (in-list new-l)])
                                        (wrap i n))))]
       [(box? e)
        (define a (unbox e))
        (define new-a (loop a))
        (if (eq? a new-a)
            e
            #`(box #,(wrap a new-a)))]
       [(syntax? e)
        (define v (syntax-e e))
        (define new-v (loop v))
        (if (and (eq? v new-v)
                 (not (syntax-position e))
                 (not (syntax-property e 'paren-shape)))
            e
            (let ([s #`(datum->syntax (quote-syntax #,(datum->syntax e 'ctx))
                                      #,(wrap v new-v)
                                      `#(#,(if src-stx
                                               #`(unquote #,src-stx)
                                               (syntax-source e))
                                         #,(syntax-line e)
                                         #,(syntax-column e)
                                         #,(syntax-position e)
                                         #,(syntax-span e)))])
              (if (syntax-property e 'paren-shape)
                  #`(syntax-property #,s 'paren-shape '#,(syntax-property e 'paren-shape))
                  s)))]
       [else e])))
  (syntax-case stx ()
    [(_ #:source src-expr e)
     (wrap #'e (convert #'e #'src-expr))]
    [(_ e)
     (wrap #'e (convert #'e #f))]))

#|

This library is derived from the syntax/quote library of Racket
5.93.

Racket
Copyright (c) 2010-2013 PLT Design Inc.

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This means that you can link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular means
that you must release the source code for the modified software.  See
share/COPYING_LESSER.txt (in Racket source) for more information.

|#
