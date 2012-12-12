#lang racket

#|

|#

(require "util.rkt")

(define read-type-anno
  (case-lambda
   ((ch in) ;; trigger char and input port
    (let ((anno (read in)))
      `(set-anno (type ,anno) ,(read in))))
   ((ch in src line col pos)
    ;; Any reading we do here will get the same position into the
    ;; syntax object. That is, annotations and their target will have
    ;; the same source location, unless we do something to change
    ;; that.
    (read-type-anno ch in))))

(define read-generic-anno
  (case-lambda
   ((ch in) ;; trigger char and input port
    (let ((anno (read in)))
      `(set-anno ,anno ,(read in))))
   ((ch in src line col pos) ;; for read-syntax also location info
    (read-generic-anno ch in))))

(define* magnolisp-readtable
  (make-readtable
   (current-readtable)
   #\^ 'terminating-macro read-type-anno
   #\@ 'terminating-macro read-generic-anno))
