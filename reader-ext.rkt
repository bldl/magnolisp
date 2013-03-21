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
        (when (eof-object? t)
          (raise-read-eof-error "expected datum to follow type"
                                src line col pos #f))
        (syntax-property d 'type t))))))

;;; 
;;; generic annotations
;;; 

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
            (raise-read-error "expected pair or symbol to follow #^"
                              src line col pos #f))
          (let ((n (car datum)))
            (unless (symbol? n)
              (raise-read-error "expected #^(SYMBOL value ...)"
                                src line col pos #f))
            `(,(make-setter-name n) ,(cdr datum) ,(read in) ))))))

(define read-generic-anno
  (case-lambda
   ((ch in) ;; trigger char and input port
    (do-read-generic-anno ch in))
   ((ch in src line col pos) ;; for read-syntax also location info
    (do-read-generic-anno in src line col pos))))

;;; 
;;; reader extension
;;; 

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'non-terminating-macro read-type-anno
;   #\^ 'dispatch-macro read-generic-anno
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
    ;;"#^5 (1 2 3)" ;; syntax error
    ;;"#^(1 2) (1 2 3)" ;; syntax error
    "^X x"
    "^(list Y) ys"
    ;; "#^throwing f"
    ;; "#^(one-of Foo Bar Baz) x"
    ;; "#^(foo bar) 1"
    ;; "(1 2 ^int 3 ^(list int) (1 2 3))"
    ;; "(define #^(throws Exception) ^int (f ^int x) (return x))"
    )))
