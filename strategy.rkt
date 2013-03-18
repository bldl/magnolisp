#lang racket

#|

This is a basic Stratego-inspired term rewriting library for
Racket.

Everything here apart from failure values and 'one', 'some', and 'all'
strategies are generic, and some of those are also implemented in
terms of interfaces. Unfortunately such interfaces cannot be defined
for existing datatypes. Perhaps we should define this module as a
'unit' or somesuch parameterizable construct to allow these operations
to be freely specified.

|#

(require "util.rkt")

;;; 
;;; Subterm interface.
;;; 

;; E.g. for 'struct', can say #:property prop:subterm-all identity
(define-values* (prop:subterm-all subterm-all? subterm-all)
  (make-struct-type-property 'subterm-all))

;;; 
;;; Failure value.
;;; 

;; In many cases we can simply use #f, but this requires #f not to be a
;; valid term. Choosing #f is good for compatibility with functions that
;; return #f as a "non-value".

(define* failed #f)

(define* failed? not)

;;; 
;;; Rewrites.
;;; 

(define* (fail ast) failed)

(define* (id ast) ast)

(define* (seq . rws)
  (lambda (ast)
    (let next ((rws rws)
               (pres ast))
      (if (null? rws)
          pres
          (let* ((rw (car rws))
                 (cres ((force rw) pres)))
            (cond
             ((failed? cres)
              failed)
             (else
              (next (cdr rws) cres))))))))

(define* (alt . rws)
  (lambda (ast)
    (let next ((rws rws))
      (if (null? rws)
          failed
          (let* ((rw (car rws))
                 (cres ((force rw) ast)))
            (cond
             ((failed? cres)
              (next (cdr rws)))
             (else
              cres)))))))

(define* (try rw)
  (alt rw id))

;; To allow for such recursive definitions lazy evaluation semantics
;; are rather essential. Or should we define the strategies as macros,
;; to control the evaluation of arguments, similar to 'if' or 'and'.
(define* (repeat rw)
  (try (seq rw (delay (repeat rw)))))

;;; 
;;; One-level traversals.
;;; 

;; For the time being we only traverse atoms and list elements in AST
;; node fields, as those are considered to be direct subterms. We
;; could later expand our support to vectors, boxes, and immutable
;; hash tables, for instance.

(define (list-rw rw ast-lst)
  (map-while (force rw) ast-lst failed?))

;; This is an 'all' for lists, where elements are "subterms". As 'map'
;; in Stratego.
(define* (list-all rw)
  (lambda (ast-lst)
    (list-rw rw ast-lst)))

;; xxx one
;; xxx some

;; While most strategies here are generic, this one is only supported
;; for terms that implement the required operation.
(define* (all rw)
  (lambda (ast)
    (let ((all (subterm-all ast)))
      (all rw ast))))

;; Tries a rewrite but restores original term on success.
(define* (where rw)
  (lambda (ast)
    (let ((res ((force rw) ast)))
      (if (failed? res)
          failed
          ast))))

;;; 
;;; Tree traversals.
;;; 

(define* (topdown rw)
  (seq rw (all (delay (topdown rw)))))

(define* (bottomup rw)
  (seq (all (delay (bottomup rw))) rw))

(define* (innermost rw)
  (bottomup (try (seq rw (delay (innermost rw))))))
