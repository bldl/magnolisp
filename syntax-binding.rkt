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

(define (atom? x)
  (ormap
   (lambda (p?) (p? x))
   (list symbol? keyword? string? bytes? number? boolean? regexp?)))

;; Applies the rewrite 'f' to all identifier? objects in the syntax
;; tree 'stx', which is traversed recursively. (The eq? based
;; optimizations here are similar to those in the syntax/quote library
;; of Racket.)
(define (rw-ids-in-syntax-tree f stx)
  (define (rw-syntax-pair d) ;; (-> cons? cons?)
    (define a (rw-syntax (car d)))
    (define b (rw-syntax-cdr (cdr d)))
    (if (and (eq? a (car d)) (eq? b (cdr d)))
        d
        (cons a b)))
  
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
      (if (eq? d n-d)
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
          (datum->syntax stx (apply vector-immutable n-d) stx stx))]

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

     [(hash? d)
      (define make
        (cond
         [(hash-eq? d) make-immutable-hasheq]
         [(hash-eqv? d) make-immutable-hasheqv]
         [(hash-equal? d) make-immutable-hash]
         [else
          (error 'rw-ids-in-syntax-tree
                 "expected (or/c hash-eq? hash-eqv? hash-equal?): ~s" d)]))
      (define changed? #f)
      (define n-lst
        (for/list ([(k v) d])
          ;; 'k' may or may not be syntax, 'v' always is.
          (define n-k (cond
                       [(syntax? k) (rw-syntax k)]
                       [(atom? k) k] ;; cannot contain IDs
                       [else
                        (error 'rw-ids-in-syntax-tree
                               "unsupported hash key: ~s" k)]))
          (define n-v (rw-syntax v))
          (unless (and (eq? k n-k) (eq? v n-v))
            (set! changed? #t))
          (cons n-k n-v)))
      (if changed?
        (datum->syntax stx (make n-lst) stx stx)
        stx)]

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

(module* test #f
  (require racket/function rackunit "util.rkt")

  (define (contain-id? stx)
    (define has? #f)
    (define (f stx)
      (set! has? #t)
      stx)
    (rw-ids-in-syntax-tree f stx)
    has?)
  
  (define (mod-id id)
    (syntax-property id 'foo 'bar))
  
  (let ((id #'car))
    (check-not-eq? id mod-id))

  (for* ((stx (list #'1
                    #'car
                    #'(1 2 3)
                    #'(1 car 3)
                    #'#&car
                    #'(1 (2 #&(car cdr) 3) 4)
                    #'#&(1 #&(2 3) 4)
                    #'#(1 2 3)
                    #'#(1 car 3)
                    #'#(1 (2 #&#(3 car) #&cdr) ())
                    #'#s(Obj 1)
                    #'#s(Obj car)
                    #'#hasheq()
                    #'#hasheq((foo . 1))
                    #'#hasheq((foo . car))
                    #'#hasheqv((1 . 2))
                    #'#hasheqv((1 . 2) (3 . car))
                    #'#hash(("foo" . car) ("bar" . cdr))
                    ))
         (f (list identity mod-id)))
    (define n-stx (rw-ids-in-syntax-tree f stx))
    (define ids? (contain-id? stx))
    (when (or (eq? f identity) (not ids?))
      (check-eq? stx n-stx "changed although ID did not"))
    (when (and (eq? f mod-id) ids?)
      (check-not-eq? stx n-stx "did not change although ID should have"))
    (writeln (list f (syntax->datum stx) (syntax->datum n-stx) (eq? stx n-stx)))
    (check-equal? (syntax->datum stx) (syntax->datum n-stx))))
