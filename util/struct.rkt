#lang racket/base

(provide struct-type-of
         struct-make-constructor
         struct-symbol)

(define (struct-type-of v)
  (define-values (x y) (struct-info v))
  x)

(define (struct-make-constructor v)
  (struct-type-make-constructor (struct-type-of v)))

(define (struct-symbol v)
  (define-values (sym d2 d3 d4 d5 d6 d7 d8)
    (struct-type-info (struct-type-of v)))
  sym)

;;; 
;;; dynamic-struct-copy
;;; 

#|

`dynamic-struct-copy` only works for #:transparent structures, and is
probably horribly inefficient. Better to use generics in serious code,
so that the generics is defined automatically when declaring a
copyable struct.

For example:
> (struct A (a b) #:transparent)
> (struct C A (c) #:transparent)
> (struct D A (d) #:transparent)
> (dynamic-struct-copy A (C 1 2 3) [b 5])
(C 1 5 3)
> (dynamic-struct-copy A (D 1 2 3) [b 5])
(D 1 5 3)

|#

(provide dynamic-struct-copy)

;; A list of the values of all but the first 'n' fields of structure
;; 'v'.
(define (struct-flds-all-but n v)
  (define vec (struct->vector v))
  (let loop ((i (sub1 (vector-length vec)))
             (lst null))
    (cond
     ((> i n)
      (loop (sub1 i) (cons (vector-ref vec i) lst)))
     (else
      lst))))

(require
 unstable/struct ;; exports get-struct-info for syntax
 (for-syntax racket/base racket/syntax
             syntax/id-table syntax/parse))

;; 'e' must be an expression of structure. Say it is of type T. Then
;; the result expression will also have type T. Only fields of the
;; base structure type 'bt' can be replaced. Any non-replaced fields
;; retain their values. We do not yet support #:parent, as for
;; 'struct-copy'.
(define-syntax (dynamic-struct-copy stx)
  (define-syntax-class fv
    (pattern (n:id v:expr)))
  (define (check-derived-accessors actuals derived)
    (for ((id derived))
      (unless (ormap (lambda (act-id)
                       (free-identifier=? act-id id)) actuals)
        (error 'dynamic-struct-copy
               "no accessor named ~a: ~a" id stx))))
  (syntax-parse stx
    ((_ bt e:expr f+v:fv ...)
     (let ()
       (define f-ids (syntax->list #'(f+v.n ...)))
       (define f-vals (syntax->list #'(f+v.v ...)))
       (define dup-id (check-duplicate-identifier f-ids))
       (when dup-id
         (error 'dynamic-struct-copy "duplicate field ID: ~a" dup-id))
       (define (make-accessor-id f-id)
         (datum->syntax f-id
                        (string->symbol
                         (format "~a-~a" (syntax-e #'bt) (syntax-e f-id)))))
       (define f-table
         (foldl
          (lambda (id e h)
            (define accessor-id (make-accessor-id id))
            (free-id-table-set h accessor-id e))
          (make-immutable-free-id-table)
          f-ids f-vals))
       (define f-accessors
         (free-id-table-map f-table (lambda (id v) id)))
       (define s-info (get-struct-info #'bt stx))
       (define s-accessors (list-ref s-info 3))
       (check-derived-accessors s-accessors f-accessors)
       (define v (generate-temporary 'v))
       (define s-exprs
         (map
          (lambda (acc)
            (free-id-table-ref
             f-table
             acc
             (lambda ()
              #`(#,acc #,v))))
          (reverse s-accessors)))
       (define s-num (length s-accessors))
       #`(let* ((n #,s-num)
                (#,v e)
                (t (struct-type-of #,v))
                (ctor (struct-type-make-constructor t))
                (d-args (struct-flds-all-but n #,v)))
           (apply ctor #,@s-exprs d-args))))))
