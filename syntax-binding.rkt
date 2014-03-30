#lang racket/base

#|

Routines for preserving identifier-binding information as a custom
syntax property. Information is only recorded for identifiers that
have a binding at phase level 0, and only when the original definition
is also at phase level 0.

|#

(require racket/list racket/match)

(provide add-binding-properties)

;;; 
;;; generic utilities
;;; 

;; Applies the rewrite 'f' to all identifier? objects in the syntax
;; tree 'stx', which is traversed recursively. (The eq? based
;; optimizations here are similar to those in the syntax/quote library
;; of Racket.)
(define (rw-ids-in-syntax-tree f stx)
  (define (rw-syntax-pair d) ;; (-> cons? cons?)
    (cons (rw-syntax (car d)) (rw-syntax-cdr (cdr d))))
  
  (define (rw-syntax-cdr x)
    (cond
     [(null? x) x]
     [(pair? x) (rw-syntax-pair x)]
     [(syntax? x) (rw-syntax x)]
     [else
      (raise-argument-error
       'rw-syntax-cdr "syntax pair second element" x)]))
  
  (define (rw-syntax stx) ;; (-> syntax? syntax?)
    (define d (syntax-e stx))
    (cond
     [(symbol? d)
      (f stx)]
     [(pair? d)
      (define n-d (rw-syntax-pair d))
      (if (and (eq? (car d) (car n-d))
               (eq? (cdr d) (cdr n-d)))
          stx
          (datum->syntax stx n-d stx stx))]
     [(null? d)
      stx]
     [(vector? d)
      (define n-d
        (for/list ([i (in-vector d)])
          (rw-syntax i)))
      (if (for/and ([i (in-vector d)]
                    [n (in-list n-d)])
            (eq? i n))
          stx
          (datum->syntax stx n-d stx stx))]
     [(box? d)
      (define v (unbox d))
      (define n-v (rw-syntax v))
      (if (eq? v n-v)
          stx
          (datum->syntax stx (box-immutable n-v) stx stx))]
     [(prefab-struct-key d)
      (define l (cdr (vector->list (struct->vector d))))
      (define new-l (for/list ([i (in-list l)])
                      (rw-syntax i)))
      (if (for/and ([i l] [n new-l])
            (eq? i n))
          stx
          (let ((s (apply make-prefab-struct
                          (prefab-struct-key d) new-l)))
            (datum->syntax stx s stx stx)))]
     ;; xxx immutable hashes to be supported
     [else
      stx]))
  (rw-syntax stx))

;;; 
;;; add-binding-properties
;;; 

(define (mpi->datum mpi)
  (define r-mp (module-path-index-resolve mpi))
  (define path (resolved-module-path-name r-mp))
  (match path
    ((? symbol?) path)
    ((? path?) (path->string path))
    ((list (and (or (? path?) (? symbol?)) p)
           (? symbol? subs) ..1)
     (cons (if (path? p) (path->string p) p) subs))
    (else
     (raise-result-error
      'resolved-module-path-name
      "documented result" path))))

(define (mk-binding-datum b)
  (cond
   ((not b) b)
   ((eq? b 'lexical) b)
   ((list? b)
    (define source-mod (first b))
    (define source-id (second b))
    (define source-phase (fifth b))
    (and (= 0 source-phase)
         (list (mpi->datum source-mod) source-id)))
   (else
    (raise-argument-error
     'mk-binding-datum
     "identifier-binding value" b))))

(define (add-binding-properties stx)
  (define (f stx)
    (define b (identifier-binding stx 0))
    (define dat (mk-binding-datum b))
    (if dat
        (syntax-property stx 'binding dat)
        stx))
  (rw-ids-in-syntax-tree f stx))

;;; 
;;; testing
;;;

(module* main #f
  (require racket/pretty "compiler-util.rkt")
  (let ((stx #'(add-binding-properties x)))
    (pretty-print (syntax->datum/binding stx))
    (set! stx (add-binding-properties stx))
    (print-with-select-syntax-properties '(binding) stx)))
