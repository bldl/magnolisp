#lang racket

#|

Utilities useful for implementing compiler back ends.

|#

(require "util.rkt")

;;; 
;;; data processing
;;; 

(define* (space-join l)
  (string-join l " "))

(define* (for-each-sep elemact sepact lst)
  (define first #t)
  (for-each
   (lambda (elem)
     (if first
         (set! first #f)
         (when sepact (sepact)))
     (when elemact (elemact elem)))
   lst))

(define* (display-as-string x)
  (format "~a" x))

(define* (write-as-string x)
  (format "~s" x))

;;; 
;;; IO
;;; 

(define-syntax on-fail
  (syntax-rules ()
    ((_ fail-expr expr)
     (with-handlers
         ((exn:fail?
           (lambda (e) fail-expr)))
       expr))))

(define* (file-read file)
  (call-with-input-file*
   file
   (lambda (in)
     (port->string in))))
     
;; Checks whether a file either does not exist or has been changed.
(define* (file-changed? file s)
  ;; Would there be a good way to write a function for comparing two
  ;; input streams? Then we could handle large files as well. ((nin
  ;; (open-input-string s))) and then compare to file input.
  (on-fail #t (not (equal? (file-read file) s))))

(define* (write-changed-file file s)
  (when (file-changed? file s)
    (call-with-output-file*
     file
     (lambda (out)
       (display s out))
     #:exists 'truncate/replace)
    (displayln file)))

(define* (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define-syntax capture
  (syntax-rules ()
    ((_ body ...)
     (capture-output (lambda () body ...)))))

(define* (write-generated-output path out writer)
  (if out
      (parameterize ((current-output-port out))
        (writer))
      (write-changed-file
       path
       (capture-output writer))))

;;;
;;; pretty printing
;;;

(define* (display-divider n [pfx #f] #:cols [cols #f])
  (define margin (if pfx (+ (string-length pfx) 1) 0))
  (define s-lst
    (for/list ((i (in-range margin (+ n 1))))
      (if cols (format "~a" (modulo i 10)) "-")))
  (when pfx
    (display pfx) (display " "))
  (displayln (apply string-append s-lst)))

(define* (display-banner pfx filename)
  (define n (let ((col (pretty-print-columns)))
              (if (exact-positive-integer? col)
                  col 40)))
  (display-divider n pfx)
  (display pfx)
  (display " ")
  (displayln filename)
  (display-divider n pfx #:cols #t))

(define* (display-generated-notice pfx)
  (display pfx)
  (displayln " generated -- do not edit"))

(define path-censor-re #rx"[^a-z0-9_]")

(define-with-contract*
  (-> path-string? string?)
  (path-h-ifdefy p)
  (string-append
   "__"
   (regexp-replace* path-censor-re
                    (string-downcase (path-basename-as-string p)) "_")
   "__"))
