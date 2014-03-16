#lang racket

#|

All AST node types must be defined as #:transparent. You will probably
want to define the base node type using the provided macro.

|#

(require "strategy.rkt" "util.rkt" "util/struct.rkt")
(require racket/generic unstable/struct)
(require (for-syntax racket/function racket/list racket/syntax))

;;; 
;;; syntax-derived annotations
;;; 

;; Creates a hasheq of the properties of the given syntax object
;; 'stx'.
(define* (stx-props stx)
  (for/hasheq ((k (syntax-property-symbol-keys stx)))
              (values k (syntax-property stx k))))

;;;
;;; base AST node definition
;;;

(define (ast-write v out mode)
  (define n (struct-symbol v))
  (define fvs (struct->list v #:on-opaque 'return-false))
  (unless fvs
    ;; Important not to 'write' v here, would lead to infinite
    ;; recursion. For the same reason cannot pass #:on-opaque 'error
    ;; to struct->list.
    (error 'ast-write "non-opaque struct ~a" n))
  (write (cons n (cdr fvs)) out))

(define-syntax-rule* (define-ast-base* n more ...)
  (abstract-struct* n (annos)
                    #:property prop:custom-write ast-write
                    #:transparent
                    more ...))

(define* (ast-get-annos v)
  (car (struct->list v)))

(define* (ast-get-fields v)
  (cdr (struct->list v)))

(define* (ast-set-annos v a)
  (define ctor (struct-make-constructor v))
  (apply ctor a (ast-get-fields v)))

(define* (ast-add-anno ast k v)
  (define annos (ast-get-annos ast))
  (ast-set-annos ast (hash-set annos k v)))

(define-syntax-rule* (preserve-annos v b ...)
  (let ((a (ast-get-annos v)))
    (let ((v (begin b ...)))
      (ast-set-annos v a))))

;;; 
;;; gen:strategic
;;; 

;; Note that ordering is delicate here. Any identifiers must be
;; defined before their values are accessed, regardless of phase
;; level. Forward references to module-level variables (without
;; access) are fine.

(define-for-syntax (make-all-visit-term nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  #`(define (all-visit-term s ast)
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
;; (define (all-rw-term s ast)
;;   (let-and var (s (Define-var ast))
;;            body (all-rw-list s (Define-body ast))
;;            (struct-copy Define ast (var var) (body body))))
(define-for-syntax (make-all-rw-term nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  (define r-f-lst (get-relevant-fields f-stx-lst))
  #`(define (all-rw-term s ast)
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
                   #`(all-rw-list s (#,get-stx ast)))))))
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
  (list (make-all-visit-term nn-stx f-stx-lst)
        (make-all-rw-term nn-stx f-stx-lst)))

;;; 
;;; concrete AST node definition
;;; 

(define-syntax* (define-ast* stx)
  (syntax-case stx ()
    ((_ name parent ((t field) ...) #:singleton (arg ...))
     (with-syntax ((the-name
                    (format-id stx "the-~a" (syntax-e #'name))))
       #`(singleton-struct*
          name (the-name arg ...) parent (field ...)
          #:methods gen:strategic
          (#,@(make-strategic
               #'name
               (syntax->list #'((t field) ...))))
          #:transparent)))
    ((_ name parent ((t field) ...) modif ...)
     #`(begin
         (concrete-struct* name parent (field ...)
           #:methods gen:strategic
           (#,@(make-strategic
                #'name
                (syntax->list #'((t field) ...))))
           #:transparent modif ...)))
    ))
