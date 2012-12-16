#lang racket

#|

We allow forms to contain annotations, hence this custom abstract data
type for them. Racket also has a datatype for syntactic forms, namely
'syntax'.

When forms are compared for equality, annotations are ignored.

Forms are not transparent, but we define a custom printer routine form
them, to match our reader extensions.

|#

(require "case.rkt")
(require "coll.rkt")
(require "util.rkt")

(define* print-annos? (make-parameter #f))

(define (syntax-loc/list stx)
  (list
   (let ((source (syntax-source stx)))
     (and (path? source)
          (let-values (((x y z) (split-path source)))
            (if (path? y) (path->string y) y))))
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define (syntax-loc/string stx)
  (format "~a:(~a:~a):p~a:w~a"
   (let ((source (syntax-source stx)))
     (or (and (path? source)
              (let-values (((x y z) (split-path source)))
                y))
         source))
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

(define (form-print x out mode)
  (when (print-annos?)
    (hash-for-each
     (form-annos x)
     (lambda (k v)
       (case-eq k
                (type
                 (write-string "^" out)
                 (write v out))
                (stx
                 (write-string "@" out)
                 (display `(stx ,(syntax-loc/string (v))) out))
                (else
                 (write-string "@" out)
                 (write `(,k ,v) out)))
       (write-string " " out))))
  (write-string "$" out)
  (let ((f (case mode
             ((#t) write)
             ((#f) display)
             (else (lambda (x out) (print x out mode))))))
    (f (form-datum x) out)))

(define form-functor
  (case-lambda
    ((x) (form-datum x))
    ((x v) (form v (form-annos x)))
    ))

(struct form (datum annos)
        #:property
        prop:equal+hash
        (list
         (lambda (a b equal?-recur)
           (equal?-recur (form-datum a) (form-datum b)))
         (lambda (a hash-recur)
           (hash-recur (form-datum a)))
         (lambda (a hash2-recur)
           (hash2-recur (form-datum a))))
        #:property
        prop:custom-write
        form-print
        #:property
        prop:procedure
        form-functor
        #:guard
        (lambda (datum annos type-name)
          (values datum
                  (or (and (hash? annos)
                           annos)
                      (and (list? annos)
                           (with-handlers ((exn:fail? (lambda (_) #f)))
                             (make-immutable-hasheq annos)))
                      (error type-name "bad annos: ~e" annos)))))

(define* (form-of pred? x)
  (pred? (form-datum x)))

(define (make-form datum (annos #hasheq()))
  (form datum annos))

(provide form? form-datum form-annos
         (rename-out (make-form form)))

(define* (form-on f x)
  (f (form-datum x) (form-annos x)))

(define* (form-on-datum f x)
  (form (f (form-datum x)) (form-annos x)))

(define* (form-on-annos f x)
  (form (form-datum x) (f (form-annos x))))

(define* (form->datum x)
  (if (not (form? x))
      x
      (let ((datum (form-datum x)))
        (cond
         ((list? datum)
          (map form->datum datum))
         ((coll? datum)
          (coll-map form->datum datum))
         (else
          (form->datum datum))))))

(define* (identifier?/form x)
  (and (form? x) (symbol? (form-datum x))))

#|
(let ((x (form 1 #hasheq((doc . "one")))))
  (parameterize ((print-annos? #t))
    (writeln x)
    (writeln (x))
    (writeln (x 2))))

(print-annos? #f)
(form 1 #hasheq((type . num) (doc . "one")))
(print-annos? #t)
(form 1 #hasheq((type . num) (doc . "one")))
|#
