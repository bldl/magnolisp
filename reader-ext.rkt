#lang racket

#|

An extended "readtable" to support type and generic annotations. The
language must implement matching macros to process any annotations
appearing in programs.

|#

(require "util.rkt")

(require syntax/readerr)

(define read-type-anno
  (case-lambda
   ((ch in) ;; trigger char and input port
    (let ((v (read in)))
      `(set-anno:type ,v ,(read in))))
   ((ch in src line col pos)
    ;; Any reading we do here will get the same position into the
    ;; syntax object. That is, annotations and their target will have
    ;; the same source location, unless we do something to change
    ;; that.
    (read-type-anno ch in))))

(define (make-setter-name n)
  (string->symbol
   (string-append "set-anno:" (symbol->string n))))

(define (do-read-generic-anno ch in
                              (src "<read>") (line #f) (col #f) (pos #f))
  (let ((datum (read in)))
    (if (symbol? datum)
        `(,(make-setter-name datum) () ,(read in))
        (begin
          (unless (pair? datum)
            (raise-read-error "expected pair or symbol to follow @"
                              src line col pos #f))
          (let ((n (car datum)))
            (unless (symbol? n)
              (raise-read-error "expected @(SYMBOL value ...)"
                                src line col pos #f))
            `(,(make-setter-name n) ,(cdr datum) ,(read in) ))))))

(define read-generic-anno
  (case-lambda
   ((ch in) ;; trigger char and input port
    (do-read-generic-anno ch in))
   ((ch in src line col pos) ;; for read-syntax also location info
    (do-read-generic-anno in src line col pos))))

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'terminating-macro read-type-anno
   #\@ 'terminating-macro read-generic-anno))

;;; 
;;; tests
;;; 

#;
(parameterize ((current-readtable magnolisp-readtable))
  (for-each
   (lambda (s)
     (pretty-print
      (read
       (open-input-string s))))
   (list
    ;;"@5 (1 2 3)" ;; syntax error
    ;;"@(1 2) (1 2 3)" ;; syntax error
    "@throwing f"
    "@(one-of Foo Bar Baz) x"
    "@(foo bar) 1"
    "(1 2 ^int 3 ^(list int) (1 2 3))"
    "(define @(throws Exception) ^int (f ^int x) (return x))"
    )))
