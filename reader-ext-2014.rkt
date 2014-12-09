#lang racket/base

#|

A reader extension to implement annotation shorthand, for both
in-declaration use, and to precede any form.

#an(x ...) is short for (#:annos x ...), which are expected to appear
within declarations.

#ap(x ...) f is short for (anno x ... f), where 'f' is any arbitrary
annotated form. We follow the Scheme tradition by turning these into
forms, namely 'anno' forms, and libraries can then decide on the
semantics of each kind of annotation.

Orthogonally to the above, we also support ^T as shorthand for (type
T), where 'T' can be any type expression.

|#

(require syntax/readerr syntax/strip-context syntax/stx)

(provide magnolisp-readtable with-magnolisp-readtable)

(define (make-loc-stx src line col pos)
  (datum->syntax #f #f (list src line col pos #f)))

;; We do not need and should not have any enrichment while reading
;; syntax.
(define anno-id-stx (strip-context #'anno))
(define type-id-stx (strip-context #'type))

;;; 
;;; type annotations
;;; 

;; ^T -> (type T)
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
           ((unsyntax type-id-stx) (unsyntax t)))))))

;;; 
;;; #a annotations
;;; 

;; (x ...) -> (#:annos x ...)
(define read-annos-declaration
  (lambda (ch in src line col pos)
    (let ((t (read-syntax src in)))
      (when (eof-object? t)
        (raise-read-eof-error
         "expected datum to follow #an"
         src line col pos #f))
      (unless (stx-list? t)
        (raise-read-error
         (format "expected list to follow #an (got: ~s)" t)
         src line col pos #f))
      (quasisyntax/loc t (#:annos (unsyntax-splicing t))))))

;; (x ...) f -> (anno x ... f)
(define read-anno-form
  (lambda (ch in src line col pos)
    (let ((s (read-syntax src in)))
      (when (eof-object? s)
        (raise-read-eof-error
         "expected annotation to follow #ap"
         src line col pos #f))
      (unless (stx-list? s)
        (raise-read-error
         (format "expected list to follow #ap (got: ~s)" s)
         src line col pos #f))
      (let ((d (read-syntax src in)))
        (when (eof-object? d)
          (raise-read-eof-error
           (format "expected datum to follow #ap annotation ~s" s)
           src line col pos #f))
        (quasisyntax/loc (make-loc-stx src line col pos)
          ((unsyntax anno-id-stx) (unsyntax-splicing s)
           (unsyntax d)))))))

(define read-hash-a-form
  (case-lambda
    ((ch in)
     (syntax->datum (read-hash-a-form ch in (object-name in) #f #f #f)))
    ((ch in src line col pos)
     (let ((kind-ch (read-char in)))
       (when (eof-object? kind-ch)
         (raise-read-eof-error
          "expected 'n' or 'p' to follow #a"
          src line col pos #f))
       (define read-hash-a-content
         (cond
          ((eqv? kind-ch #\n) read-annos-declaration)
          ((eqv? kind-ch #\p) read-anno-form)
          (else
           (raise-read-error
            (format "expected 'n' or 'p' to follow #a, got ~s" kind-ch)
            src line col pos #f))))
       ;; See also 'port-next-location'.
       (when col (set! col (+ col 1)))
       (when pos (set! pos (+ pos 1)))
       (read-hash-a-content kind-ch in src line col pos)))))

;;; 
;;; reader extension
;;; 

(define magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'non-terminating-macro read-type-anno
   #\a 'dispatch-macro read-hash-a-form
   ))

;;; 
;;; helpers
;;; 

(define-syntax-rule (with-magnolisp-readtable es ...)
  (parameterize ((current-readtable magnolisp-readtable))
    es ...))

;;; 
;;; tests
;;; 

(module* main #f
  (require "util.rkt")
  (with-magnolisp-readtable
   (for ((s (list
             "^T"
             "#ap(foo bar baz) 5"
             "#ap(^T export (perms X Y)) 7"
             "#an(^T)"
             "(function (f) #an(^T export (import #f)) 5)"
             ;;"#anexternal #at(fn Int Int) (function (f x))"
             ;;"#anfoo #anbar #anbaz 5"
             )))
       (define in (open-input-string s))
     (for/list ((obj (in-port read in)))
       (writeln obj)))))
