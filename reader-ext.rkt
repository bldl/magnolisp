#lang racket

#|

An extended "readtable" to support type and generic annotations. We
use the prefix ^ for the former, and #^ for the latter. We follow the
Scheme tradition by turning these into forms, namely 'anno' forms, and
libraries can then decide on the semantics of each kind of annotation.
Care probably must be taken in placing the annotations so that they do
not confuse or unduly complicate the writing of macros. An annotated
identifier does not look like an identifier, for example.

|#

(require "util.rkt")
(require syntax/readerr syntax/strip-context syntax/stx)

(define (make-loc-stx src line col pos)
  (datum->syntax #f #f (list src line col pos #f)))

;; We do not need and should not have any enrichment while reading
;; syntax.
(define anno-id-stx (strip-context #'anno))

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
        (quasisyntax/loc (make-loc-stx src line col pos)
          ((unsyntax anno-id-stx) type (unsyntax t) (unsyntax d))))))))

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
            ((unsyntax anno-id-stx) (unsyntax-splicing k-v)
             (unsyntax d)))))))))

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
   (for ((s (list
             "#^external ^(fn Int Int) (function (f x))"
             "#^foo #^bar #^baz 5"
             )))
       (define in (open-input-string s))
     (for/list ((obj (in-port read in)))
         (writeln obj)))))
