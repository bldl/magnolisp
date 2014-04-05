#lang racket/base

#|

Assumptions for AST node types:

- each type must be defined as #:transparent
  (this is done by the macros here)

- the first field of each node is for annotations,
  and named 'annos', and declared as 'no-term'

- no inheritance is used in defining the types

|#

(require "ast-serialize.rkt" "ast-view.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         racket/generic unstable/struct
         (for-syntax racket/base racket/function racket/list
                     racket/syntax syntax/parse)
         (for-template racket/base "ast-serialize.rkt"))

;;; 
;;; generic access
;;;

;; We could access annotations like this as well, but that would be
;; inefficient.

(define* (ast-get-fields v)
  (cdr (struct->list v)))

;;;
;;; printing
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
;;; gen:syntactifiable
;;; 

(define-for-syntax (make-syntactifiable/singleton singleton-id)
  (list
   (with-syntax ((the-name singleton-id))
     #'(define (syntactifiable-mkstx x)
         #'the-name))))

(define-for-syntax (make-syntactifiable conc-id fld-id-lst)
  (define obj-id (generate-temporary 'obj))
  (define super-id (generate-temporary 'mkstx))
  (define get-id-lst (for/list ([fld-id fld-id-lst])
                       (format-id conc-id "~a-~a"
                                  (syntax-e conc-id)
                                  (syntax-e fld-id))))
  (with-syntax ([super super-id]
                [ctor conc-id]
                [obj obj-id]
                [qs #'quasisyntax]
                [us #'unsyntax])
    (with-syntax ([(e ...)
                   (for/list ([get-id get-id-lst])
                     (with-syntax ([get get-id])
                       #'(us (super (get obj)))))])
      (list
       #'(define/generic super syntactifiable-mkstx)
       #'(define (syntactifiable-mkstx obj)
           (qs (ctor e ...)))))))
            
;;; 
;;; concrete AST node definition
;;; 

(define-syntax* (define-ast* stx)
  (define-syntax-class ft
    (pattern (~or (~datum no-term)
                  (~datum just-term)
                  (~datum list-of-term))))
  (syntax-parse stx
    [(_ name:id (view:id ...) ((t:ft fld:id) ...)
        (~optional (~seq #:singleton (arg:expr ...)))
        (~optional (~seq #:custom-write writer:expr))
        (~optional (~seq #:struct-options (opt ...+))))
     (define singleton? (attribute arg))
     (define singleton-id
       (and singleton? (format-id stx "the-~a" (syntax-e #'name))))
     (quasisyntax/loc stx
       (#,(if singleton? #'singleton-struct* #'concrete-struct*)
        name
        #,@(if singleton?
               (with-syntax ((the-name singleton-id))
                 (list #'(the-name arg ...)))
               null)
        (fld ...)
        #:property prop:custom-write #,(if (attribute writer)
                                           #'writer
                                           #'ast-write)
        #:methods gen:syntactifiable
        (#,@(if singleton?
                (make-syntactifiable/singleton singleton-id)
                (make-syntactifiable
                 #'name (syntax->list #'(fld ...)))))
        #:methods gen:strategic (#,@(make-strategic
                                     #'name
                                     (syntax->list #'((t fld) ...))))
        #,@(apply append
                  (for/list ([view-id (syntax->list #'(view ...))])
                    (generate-view-methods #'name view-id singleton?)))
        #:transparent
        #,@(if (attribute opt)
               (syntax->list #'(opt ...))
               null)))]))

;;; 
;;; testing
;;; 

(module* test #f
  (require racket rackunit)

  (define-view Ast #:fields (annos))

  (define-ast* Singleton (Ast) ((no-term annos)) #:singleton (#hasheq()))
  (define-ast* Empty (Ast) ((no-term annos)))
  (define-ast* Object (Ast) ((no-term annos) (just-term one) (list-of-term many)))

  (define empty (Empty #hasheq()))
  (define object (Object #hasheq() the-Singleton (list the-Singleton empty)))
  
  (for ([dat (list the-Singleton
                   `(,the-Singleton)
                   `(1 ,the-Singleton 3)
                   (Empty #hasheq())
                   object
                   (box object)
                   (hasheq 'empty empty 'singleton the-Singleton)
                   (Empty (hasheq 'stx #'(Empty #hasheq())))
                   (Empty (hasheq 'origin (list #'foo #'bar)))
                   )])
    (writeln `(ORIGINAL VALUE ,dat))
    (define stx (syntactifiable-mkstx dat))
    ;;(writeln stx)
    (writeln `(MARSHALLED SYNTAX ,(syntax->datum stx)))
    (define val (eval-syntax stx))
    (writeln `(UNMARSHALED VALUE ,val))
    (when (Ast? val)
      (define annos (Ast-annos val))
      (unless (hash-empty? annos)
        (writeln `(UNMARSHALED ANNOS ,annos)))))

  (void))
