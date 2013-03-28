#lang racket

#|

For more compact printing, we do not make annotations transparent.

|#

(require "reader-ext.rkt" "strategy.rkt" "util.rkt")
(require racket/generic)
(require (for-syntax racket/function racket/list racket/syntax))

;;; 
;;; syntax-derived annotations
;;; 

(define* (stx-annos stx)
  (let ((h (for/hasheq ((k (syntax-property-symbol-keys stx)))
                       (values k (syntax-property stx k)))))
    (set! h (hash-set h 'loc (stx-loc stx)))
    (set! h (hash-set h 'stx stx))
    h))

;;; 
;;; abstract node
;;; 

;; Note that ordering is delicate here. Any identifiers must be
;; defined before their values are accessed, regardless of phase
;; level. Forward references to module-level variables (without
;; access) are fine.

(define-struct* Ast (annos))

(define* (Ast-anno-ref ast k #:must (must #t))
  (let* ((annos (Ast-annos ast)))
    (if must
        (hash-ref annos k)
        (hash-ref annos k #f))))

(define-for-syntax (make-for-each-subterm nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  #`(define (for-each-subterm s ast)
      #,@(map
          (lambda (f-stx)
            (syntax-case f-stx (no-term just-term list-of-term)
              ((no-term fn-pat)
               (void))
              ((just-term fn-pat)
               (let* ((fn-sym (syntax-e #'fn-pat))
                      (get-stx (format-id nn-stx "~a-~a"
                                          nn-sym fn-sym)))
                 #`(s (#,get-stx ast))))
              ((list-of-term fn-pat)
               (let* ((fn-sym (syntax-e #'fn-pat))
                      (get-stx (format-id nn-stx "~a-~a"
                                          nn-sym fn-sym)))
                 #`(for-each s (#,get-stx ast))))))
          f-stx-lst)
      (void)))

(define-for-syntax (get-relevant-fields f-stx-lst)
  (filter
   identity
   (map
    (lambda (f-stx)
      (syntax-case f-stx (no-term just-term list-of-term)
        ((no-term fn-pat)
         #f)
        ((just-term fn-pat)
         (list 'just #'fn-pat (generate-temporary)))
        ((list-of-term fn-pat)
         (list 'list #'fn-pat (generate-temporary)))))
    f-stx-lst)))

;; E.g. output
;; (define (subterm-all s ast)
;;   (let-and var (s (Define-var ast))
;;            body (map-while s (Define-body ast))
;;            (struct-copy Define ast (var var) (body body))))
(define-for-syntax (make-subterm-all nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  (define r-f-lst (get-relevant-fields f-stx-lst))
  #`(define (subterm-all s ast)
      (let-and
       #,@(apply
           append
           (map
            (lambda (fld)
              (let* ((kind (first fld))
                     (fn-stx (second fld))
                     (tmp-stx (third fld))
                     (fn-sym (syntax-e fn-stx))
                     (get-stx (format-id nn-stx "~a-~a"
                                         nn-sym fn-sym)))
                (list
                 tmp-stx
                 (cond
                  ((eq? kind 'just)
                   #`(s (#,get-stx ast)))
                  ((eq? kind 'list)
                   #`(map-while s (#,get-stx ast)))))))
            r-f-lst))
       (struct-copy
        #,nn-stx ast
        #,@(map
            (lambda (fld)
              (let* ((fn-stx (second fld))
                     (tmp-stx (third fld)))
                #`(#,fn-stx #,tmp-stx)))
            r-f-lst)))))

(define-for-syntax (make-strategic nn-stx f-stx-lst)
  (list (make-for-each-subterm nn-stx f-stx-lst)
        (make-subterm-all nn-stx f-stx-lst)))

(define-syntax (define-ast* stx)
  (syntax-case stx ()
    ((_ name ((t field) ...))
     #`(begin
         (define-struct* name Ast (field ...)
           #:methods gen:strategic
           (#,@(make-strategic
                #'name
                (syntax->list #'((t field) ...))))
           #:transparent)
         (define* #,(format-id stx "new-~a" (syntax-e #'name))
           (lambda (stx . args)
             (apply name (stx-annos stx) args)))))))

;;; 
;;; concrete nodes
;;; 

(define-ast* Var ((no-term name)))
(define-ast* Literal ((no-term datum)))
(define-ast* Verbatim ((no-term text)))
(define-ast* Module ((list-of-term body)))
(define-ast* Pass ())
(define-ast* Call ((just-term proc)))
(define-ast* Define ((just-term var) (no-term kind)
                     (list-of-term body)))

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

(define* (Var-rename ast n)
  (struct-copy Var ast (name n)))
