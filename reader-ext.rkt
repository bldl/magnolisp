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
      (let ((s-dat (syntax-e s)))
        (let-values
            (((k v)
              (cond
               ((symbol? s-dat)
                (values s-dat (datum->syntax #f #t s)))
               ((pair? s-dat)
                (let* ((k-stx (car s-dat))
                       (k (syntax-e k-stx)))
                  (unless (symbol? k)
                    (raise-read-error
                     (format
                      "expected #^(name value), got ~s for name" k-stx)
                     src line col pos #f))
                  (let* ((v-stx (cdr s-dat)))
                    (values k
                            (if (stx-null? v-stx)
                                (datum->syntax #f #t s)
                                v-stx)))))
               (else
                (raise-read-error
                 (format "expected annotation to follow #^, got ~s" s)
                 src line col pos #f)))))
          (let ((d (read-syntax src in)))
            (when (eof-object? d)
              (raise-read-eof-error
               (format "expected datum to follow annotation ~s" s)
               src line col pos #f))
            (writeln (list k v))
            (syntax-property d k v))))))))

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
;; list of syntax objects. As a special feature adjusts the syntax by
;; translating annotation setters to syntax properties. The values are
;; stored as syntax.
(define (read-syntaxes source-name in)
  (let* ((read (lambda (in)
                 (read-syntax source-name in))))
    (for/list ((obj (in-port read in)))
        obj))) ;; xxx must adjust obj -- can use syntax-case -- should also study read-syntax/recursive, which may be useful for this sort of thing

;; Reads all Magnolisp syntax from a file. Produces a list of Racket
;; syntax objects. Any #lang directive is ignored. Filename extension
;; matters not.
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

;#;
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

    ;; generic annotation tests
    "#^throwing f"
    "#^(throwing) f"
    "#^(throwing #f) f"


    ;; mixed annotation tests
    
    ;;"#^5 (1 2 3)" ;; syntax error
    ;;"#^(1 2) (1 2 3)" ;; syntax error
    ;; "#^throwing f"
    ;; "#^(one-of Foo Bar Baz) x"
    ;; "#^(foo bar) 1"
    ;; "(1 2 ^int 3 ^(list int) (1 2 3))"
    ;; "(define #^(throws Exception) ^int (f ^int x) (return x))"
    )))
