#lang racket

#|

An extended "readtable" to support type and generic annotations. We
use the prefix ^ for the former, and #^ for the latter.

If we directly stored annotations as syntax properties, there would be
two problems. (1) If you read plain sexps, annotation info will get
discarded. (2) Annotations are "hidden", and not implicitly subject to
macro expansion or binding enrichment or such things.

We instead generate wrapper forms that are subject to macro expansion.
Then just have to make sure that such wrappers go only in places where
they do not much hamper "parsing". Around definitions is probably a
good place, as partial expansion usually occurs in such contexts
automatically, until actual definitions are found. And annotations are
commonly associated with definitions.

|#

(require "util.rkt")
(require syntax/readerr syntax/stx)

(define (make-loc-stx src line col pos)
  (datum->syntax #f #f (list src line col pos #f)))

;;; 
;;; type annotations
;;; 

(define read-type-anno
  (case-lambda
   ((ch in)
    (syntax->datum (read-type-anno ch in (object-name in) #f #f #f)))
   ((ch in src line col pos)
    (let ((t (read-syntax src in)))
      (when (eof-object? t)
        (raise-read-eof-error
         "expected type to follow ^"
         src line col pos #f))
      (unless (or (identifier? t) (stx-pair? t))
        (raise-read-error
         (format "expected type specifier to follow ^ (got: ~s)" t)
         src line col pos #f))
      (let ((d (read-syntax src in)))
        (when (eof-object? d)
          (raise-read-eof-error
           (format "expected datum to follow type ~s" t)
           src line col pos #f))
        ;;(syntax-property d 'type t)
        (quasisyntax/loc (make-loc-stx src line col pos)
          (anno type (unsyntax t) (unsyntax d))))))))

;;; 
;;; generic annotations
;;; 

(define read-generic-anno
  (case-lambda
   ((ch in)
    (syntax->datum (read-generic-anno ch in (object-name in) #f #f #f)))
   ((ch in src line col pos)
    (let ((s (read-syntax src in)))
      (when (eof-object? s)
        (raise-read-eof-error
         "expected annotation to follow #^"
         src line col pos #f))
      (let ((k-v
             (or (and (identifier? s)
                      (let ((s-dat (syntax-e s)))
                        (list s-dat (datum->syntax #f #t s))))
                 (lets then-if-let s-lst (syntax->list s)
                       then-let s-len (length s-lst)
                       then-if (or (= s-len 1) (= s-len 2))
                       then-let n-stx (first s-lst)
                       then-if (identifier? n-stx)
                       then-let v-stx (if (= s-len 1)
                                          (datum->syntax #f #t s)
                                          (second s-lst))
                       (list n-stx v-stx))
                 (raise-read-error
                  (format "expected annotation to follow #^, got ~s" s)
                  src line col pos #f))))
        (let ((d (read-syntax src in)))
          (when (eof-object? d)
            (raise-read-eof-error
             (format "expected datum to follow annotation ~s" s)
             src line col pos #f))
          ;;(apply syntax-property d k-v)
          (quasisyntax/loc (make-loc-stx src line col pos)
            (anno (unsyntax-splicing k-v) (unsyntax d)))))))))

;;; 
;;; reader extension
;;; 

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'non-terminating-macro read-type-anno
   #\^ 'dispatch-macro read-generic-anno
   ))

;;; 
;;; helpers
;;; 

(define-syntax-rule* (with-magnolisp-readtable es ...)
  (parameterize ((current-readtable magnolisp-readtable))
    es ...))

;;; 
;;; tests
;;; 

(module* main #f
  (with-magnolisp-readtable
   (define in (open-input-string "1 2 3 ^Int x #^throwing ^void g"))
   (for/list ((obj (in-port read in)))
       obj)))
