#lang racket

#|

Prior to translating to Core language, we are dealing with forms only,
with little semantics associated with them.

We allow forms to contain annotations, hence this custom abstract data
type. When forms are compared for equality, annotations are ignored.

Forms are not transparent, but we define a custom printer routine form
them, to match our reader extensions.

|#

(require "util.rkt")

(define* print-annos? (make-parameter #f))

(define (form-print x out mode)
  (when (print-annos?)
    (hash-for-each
     (form-annos x)
     (lambda (k v)
       (if (eq? k 'type)
           (begin
             (write-string "^" out)
             (write v out))
           (begin
             (write-string "@" out)
             (write `(,k ,v) out)))
       (write-string " " out))))
  (write-string "$" out)
  (let ((f (case mode
             ((#t) write)
             ((#f) display)
             (else (lambda (x out) (print x out mode))))))
    (f (form-datum x) out)))

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
        #:guard
        (lambda (datum annos type-name)
          (values datum
                  (or (and (hash? annos)
                           annos)
                      (and (list? annos)
                           (with-handlers ((exn:fail? (lambda (_) #f)))
                             (make-immutable-hasheq annos)))
                      (error type-name "bad annos: ~e" annos)))))

(define (make-form datum #:annos (annos #hasheq()))
  (form datum annos))

(provide form? form-datum form-annos
         (rename-out (make-form form)))

#|
(print-annos? #f)
(form 1 #hasheq((type . num) (doc . "one")))
(print-annos? #t)
(form 1 #hasheq((type . num) (doc . "one")))
|#
