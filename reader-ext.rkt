#lang racket

#|

An extended "readtable" to support type and generic annotations.

|#

(require "util.rkt")
(require syntax/readerr syntax/stx)

;;; 
;;; location info
;;; 

(define-struct* loc (source line column position span) #:transparent)

(define* (stx-loc stx)
  (loc
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

;;; 
;;; type annotations
;;; 

(define read-type-anno
  (case-lambda
   ((ch in)
    (begin
      (read in) ;; skip type anno datum
      (read in) ;; produce actual datum
      ))
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
        (syntax-property d 'type t))))))

;;; 
;;; generic annotations
;;; 

(define read-generic-anno
  (case-lambda
   ((ch in)
    (begin
      (read in) ;; skip anno datum
      (read in) ;; produce actual datum
      ))
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
                       then-let n-sym (syntax-e n-stx)
                       then-let v-stx (if (= s-len 1)
                                          (datum->syntax #f #t s)
                                          (second s-lst))
                       (list n-sym v-stx))
                 (raise-read-error
                  (format "expected annotation to follow #^, got ~s" s)
                  src line col pos #f))))
        (let ((d (read-syntax src in)))
          (when (eof-object? d)
            (raise-read-eof-error
             (format "expected datum to follow annotation ~s" s)
             src line col pos #f))
          (writeln k-v)
          (apply syntax-property d k-v)))))))

;;; 
;;; reader extension
;;; 

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'non-terminating-macro read-type-anno
   #\^ 'dispatch-macro read-generic-anno
   ))

;; Reads all available syntax in the specified input stream. Returns a
;; list of syntax objects.
(define (read-syntaxes source-name in)
  (let* ((read (lambda (in)
                 (read-syntax source-name in))))
    (for/list ((obj (in-port read in)))
        obj)))

;; Reads all Magnolisp syntax from a file whose path is given.
;; Produces a list of syntax objects. Any #lang directive is ignored.
;; The path is cleansed to ensure a decent source file name for syntax
;; objects.
(define* (load-as-syntaxes file)
  (let* ((path (cleanse-path file)))
    (call-with-input-file path
      (lambda (in)
        (parameterize ((current-readtable magnolisp-readtable))
          (port-count-lines! in)
          (read-language in (thunk (void)))
          (read-syntaxes path in))))))

;;; 
;;; tests
;;; 

#;
(parameterize ((current-readtable magnolisp-readtable)
               (port-count-lines-enabled #t))
  (for-each
   (lambda (s)
     (let ((stx
            (read-syntax
             "<string s>"
             (open-input-string s))))
       (pretty-print
        (append
         (list (syntax->datum stx))
         (for/list ((k (syntax-property-symbol-keys stx)))
             (cons k (syntax-property stx k)))
         (list (stx-loc stx))))))
   (list
    ;; type annotation tests
    "^X x"
    "^(list Y) ys"
    ;;"^" ;; syntax error
    ;;"^5 x" ;; syntax error
    ;;"^T" ;; syntax error
    ;;"^()" ;; syntax error
    "(1 2 ^int 3 ^(list int) (1 2 3))"

    ;; generic annotation tests
    "#^throwing f"
    "#^(throwing) f"
    "#^(throwing #f) f"
    ;;"#^() f" ;; syntax error

    ;; mixed annotation tests
    "^T #^(x 1) #^(y 2) z"
    "#^(one-of (Foo Bar Baz)) x"
    "(define #^(throws Exception) (^int f ^int x) (return x))"
    )))
