#lang racket

#|

A reader extension to support generic annotations preceding arbitrary
forms. We currently use the crazy syntax #an (generic) or #at (type),
but these are not currently used, anyway. We follow the Scheme
tradition by turning these into forms, namely 'anno' forms, and
libraries can then decide on the semantics of each kind of annotation.

The reader extension also implements a shorthand for (type T), namely
^T.

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
          "expected type expression to follow ^"
          src line col pos #f))
       (unless (or (identifier? t) (stx-pair? t))
         (raise-read-error
          (format "expected type expression to follow ^ (got: ~s)" t)
          src line col pos #f))
         (quasisyntax/loc (make-loc-stx src line col pos)
           (type (unsyntax t)))))))

;;; 
;;; form-preceding annotations
;;; 

(define read-preceding-type
  (lambda (ch in src line col pos)
    (let ((t (read-syntax src in)))
      (when (eof-object? t)
        (raise-read-eof-error
         "expected type expression to follow #at"
         src line col pos #f))
      (unless (or (identifier? t) (stx-pair? t))
        (raise-read-error
         (format "expected type expression to follow #at (got: ~s)" t)
         src line col pos #f))
      (let ((d (read-syntax src in)))
        (when (eof-object? d)
          (raise-read-eof-error
           (format "expected datum to follow type ~s" t)
           src line col pos #f))
        (quasisyntax/loc (make-loc-stx src line col pos)
          ((unsyntax anno-id-stx) type (unsyntax t) (unsyntax d)))))))

(define read-preceding-generic
  (lambda (ch in src line col pos)
    (let ((s (read-syntax src in)))
      (when (eof-object? s)
        (raise-read-eof-error
         "expected annotation to follow #an"
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
                  (format "expected annotation to follow #an, got ~s" s)
                  src line col pos #f))))
        (let ((d (read-syntax src in)))
          (when (eof-object? d)
            (raise-read-eof-error
             (format "expected datum to follow annotation ~s" s)
             src line col pos #f))
          (quasisyntax/loc (make-loc-stx src line col pos)
            ((unsyntax anno-id-stx) (unsyntax-splicing k-v)
             (unsyntax d))))))))

(define read-form-with-anno
  (case-lambda
    ((ch in)
     (syntax->datum (read-form-with-anno ch in (object-name in) #f #f #f)))
    ((ch in src line col pos)
     (let ((kind-ch (read-char in)))
       (when (eof-object? kind-ch)
         (raise-read-eof-error
          "expected 't' or 'n' to follow #a"
          src line col pos #f))
       (define read-preceding
         (cond
          ((eqv? kind-ch #\t) read-preceding-type)
          ((eqv? kind-ch #\n) read-preceding-generic)
          (else
           (raise-read-error
            (format "expected 't' or 'n' to follow #a, got ~s" kind-ch)
            src line col pos #f))))
       ;; See also 'port-next-location'.
       (when col (set! col (+ col 1)))
       (when pos (set! pos (+ pos 1)))
       (read-preceding kind-ch in src line col pos)))))

;;; 
;;; reader extension
;;; 

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'non-terminating-macro read-type-anno
   #\a 'dispatch-macro read-form-with-anno
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
             "#anexternal #at(fn Int Int) (function (f x))"
             "#anfoo #anbar #anbaz 5"
             "(quote ^T)"
             )))
       (define in (open-input-string s))
     (for/list ((obj (in-port read in)))
         (writeln obj)))))
