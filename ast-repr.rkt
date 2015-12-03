#lang racket/base

#|

AST representation.

Assumptions for AST node types:

- each type must be defined as #:transparent
  (this is done by the macros here)

- no inheritance is used in defining the types

|#

(require "ast-serialize.rkt" "ast-view.rkt"
         "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         racket/generic racket/unsafe/ops racket/struct
         (for-syntax "util/assert.rkt"
                     racket/base racket/function racket/list
                     racket/pretty 
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

;; Produces syntax for a node-type specific implementation of a
;; gen:custom-write style function.
(define-for-syntax (mkstx-ast-write n-stx fld-id-lst)
  (define getter-lst
    (for/list ([fld fld-id-lst]
               #:unless (eq? (syntax-e fld) 'annos))
      (format-id n-stx "~a-~a" n-stx fld)))
  
  (with-syntax ([name n-stx]
                [(get ...) getter-lst])
    #'(lambda (v out mode)
        (write (list (quote name) (get v) ...) out))))

;;; 
;;; gen:strategic
;;; 

(define-syntax let-rewrite-all
  (syntax-rules ()
    [(_ s #:just new old . rest)
     (let ([new (s old)])
       (and new (let-rewrite-all s . rest)))]
    [(_ s #:many new old . rest)
     (let ([new (list-rewrite-all s old)])
       (and new (let-rewrite-all s . rest)))]
    [(_ s #:maybe new old . rest)
     (let ([new (and old (s old))])
       (and (or new (not old))
            (let-rewrite-all s . rest)))]
    [(_ s e) e]))

(begin-for-syntax
  (struct ConcFld (id qty ix) #:transparent)

  (define (term-ConcFld? fld)
    (not (eq? (ConcFld-qty fld) 'none)))

  (define (mkstx-term-visit-all nn-stx f-spec-lst)
    (define nn-sym (syntax-e nn-stx))
    #`(define (term-visit-all s ast)
        #,@(for/list ([fld f-spec-lst])
             (with-syntax ([get-expr
                            #`(unsafe-struct*-ref ast #,(ConcFld-ix fld))])
               (case-or-fail (ConcFld-qty fld)
                 [(none)
                  #'(begin)]
                 [(just)
                  #'(s get-expr)]
                 [(many)
                  #'(for-each s get-expr)]
                 [(maybe)
                  #'(let ([tmp get-expr])
                      (and tmp (s tmp)))])))
        (void)))

  (define (mkstx-r-f-struct-copy type-id obj-id r-f-lst)
    (with-syntax ([type type-id]
                  [obj obj-id]
                  [(set-fld ...)
                   (for/list ([fld r-f-lst])
                     (with-syntax ([fld (second fld)]
                                   [tmp (third fld)])
                       #'[fld tmp]))])
      #'(struct-copy type obj set-fld ...)))

  ;; E.g. output:
  ;; (define (term-rewrite-all s ast)
  ;;   (let ((old-var (Define-var ast))
  ;;         (old-body (Define-body ast)))
  ;;     (let-and
  ;;       var (s old-var)
  ;;       body (list-rewrite-all s old-body)
  ;;       (if (and (eq? old-var var) (eq? old-body body))
  ;;           ast
  ;;           (struct-copy Define ast (var var) (body body))))))
  (define (mkstx-term-rewrite-all nn-stx f-spec-lst)
    (define nn-sym (syntax-e nn-stx))

    (define r-f-lst ;; (list/c kind id new-tmp old-tmp ix)
      (for/list ([fld f-spec-lst]
                 #:when (term-ConcFld? fld))
        (define id (ConcFld-id fld))
        (define new-tmp (generate-temporary id))
        (define old-tmp (generate-temporary id))
        (list (ConcFld-qty fld) id new-tmp old-tmp
              (ConcFld-ix fld))))
    
    (define ast-id (generate-temporary 'ast))
    (with-syntax* ([s (generate-temporary 's)]
                   [ast ast-id]
                   [(bind-old ...)
                    (for/list ([fld r-f-lst])
                      (with-syntax
                        ([old (fourth fld)]
                         [get-expr
                          #`(unsafe-struct*-ref ast #,(fifth fld))])
                        #'[old get-expr]))]
                   [(bind-new ...)
                    (apply
                     append
                     (for/list ([fld r-f-lst])
                       (define kind-kw
                         (case-or-fail (first fld)
                          [(just) #'#:just]
                          [(many) #'#:many]
                          [(maybe) #'#:maybe]))
                       (define new-id (third fld))
                       (define old-id (fourth fld))
                       (list kind-kw new-id old-id)))]
                   [(eq-cmp ...)
                    (for/list ([fld r-f-lst])
                      (define kind (first fld))
                      (with-syntax ([new (third fld)]
                                    [old (fourth fld)])
                        (case kind
                          [(maybe)
                           #'(or (not old) (eq? old new))]
                          [else
                           #'(eq? old new)])))]
                   [copy (mkstx-r-f-struct-copy nn-stx ast-id r-f-lst)])
      #'(define (term-rewrite-all s ast)
          (let (bind-old ...)
            (let-rewrite-all s
             bind-new ...
             (if (and eq-cmp ...)
                 ast
                 copy))))))

  (define (mkstx-term-fields f-spec-lst)
    (define lst
      (filter term-ConcFld? f-spec-lst))
    (if (null? lst)
        #'(define (term-fields ast) null)
        (with-syntax ([(ix ...) (map ConcFld-ix lst)])
          #'(define (term-fields ast)
              (list (unsafe-struct*-ref ast ix) ...)))))
     
  (define (mkstx-set-term-fields type-id f-spec-lst)
    (define r-f-lst
      (for/list ([fld f-spec-lst]
                 #:when (term-ConcFld? fld))
        (define id (ConcFld-id fld))
        (list id (generate-temporary id))))
    (if (null? r-f-lst)
        #'(define (set-term-fields ast lst) ast)
        (with-syntax ([type type-id]
                      [(tmp ...) (map second r-f-lst)]
                      [(set-fld ...) (for/list ([fld r-f-lst])
                                       (with-syntax ([fld (first fld)]
                                                     [tmp (second fld)])
                                         #'[fld tmp]))])
          #'(define (set-term-fields ast lst)
              (let-values ([(tmp ...) (apply values lst)])
                (struct-copy type ast set-fld ...))))))

  (define (mkstx-strategic nn-stx f-spec-lst)
    `(,(mkstx-term-visit-all nn-stx f-spec-lst)
      ,(mkstx-term-rewrite-all nn-stx f-spec-lst)
      ,(mkstx-term-fields f-spec-lst)
      ,(mkstx-set-term-fields nn-stx f-spec-lst))))

;;; 
;;; gen:syntactifiable
;;; 

(define-for-syntax (mkstx-syntactifiable conc-id fld-id-lst)
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
;;; additional functional struct accessors
;;; 

;; Returns (listof syntax?).
(define-for-syntax (mkstx-extra-accessors conc-id fld-id-lst def-stx)
  (define conc-name (syntax-e conc-id))
  (with-syntax ([conc conc-id]
                [def def-stx])
    (define copy-impl
      (with-syntax ([(fld ...) fld-id-lst]
                    [c-copy (format-id conc-id "copy-~a" conc-name)])
         #'(def (c-copy obj fld ...)
             (conc fld ...))))
    
    (define (mkstx-setter-impl fld-id)
      (define fld-name (syntax-e fld-id))
      (with-syntax ([c-setter-id
                     (format-id conc-id "set-~a-~a" conc-name fld-name)]
                    [fld fld-id])
        #'(def (c-setter-id obj fld)
            (struct-copy conc obj [fld fld]))))
    
    (cons copy-impl (map mkstx-setter-impl fld-id-lst))))

;;; 
;;; equal? implementation
;;; 

(define-for-syntax (mkstx-equal+hash n-stx fld-id-lst)
  (define getter-lst
    (for/list ([fld fld-id-lst]
               #:unless (eq? (syntax-e fld) 'annos))
      (format-id n-stx "~a-~a" n-stx fld)))

  (define equal-stx
    (with-syntax ([(get ...) getter-lst])
      #'(define (equal-proc x y e?)
          (and (e? (get x) (get y)) ... #t))))
  
  (define hash-stx
    #`(define (hash-proc x h)
        (+ #,@(for/list ([c (in-naturals 1)] [get getter-lst])
                #`(* #,c (h (#,get x)))))))
  
  (define hash2-stx
    #`(define (hash2-proc x h)
        (+ #,@(for/list ([c (in-naturals 17)] [get getter-lst])
                #`(* #,c (h (#,get x)))))))
    
  ;;(write (map syntax->datum (list equal-stx hash-stx hash2-stx))) (newline)
  
  (list equal-stx hash-stx hash2-stx))

(define-for-syntax (mkstx-ast=? conc-id def-stx)
  (define conc-name (syntax-e conc-id))
  (with-syntax
    ([def def-stx]
     [expected (format "~a?" conc-name)]
     [ast? (format-id conc-id "~a?" conc-name)]
     [ast=? (format-id conc-id "~a=?" conc-name)])
    #'(def (ast=? x y)
        (cond
         [(not (ast? x))
          (raise-argument-error (quote ast=?) expected 0 x y)]
         [(eq? x y)
          #t]
         [(not (ast? y))
          (raise-argument-error (quote ast=?) expected 1 x y)]
         [else
          (equal? x y)]))))

;;; 
;;; concrete AST node definition
;;; 

(define-for-syntax (mkstx-define-ast stx provide?)
  (define-syntax-class vw
    #:description "AST node view specification"
    #:attributes (spec)
    (pattern v-id:id
             #:attr spec (list #'v-id null #f #f))
    (pattern (v-id:id v-spec:conc-vspec)
             #:attr spec (list #'v-id 
                               (attribute v-spec.fspec-lst)
                               (attribute v-spec.copy)
                               (attribute v-spec.predicate))))

  (define-syntax-class fspec
    #:description "AST node field specification"
    #:attributes (spec)
    (pattern [t:term-qty fld:id]
             #:attr spec (list #'fld (attribute t.qty))))

  (define def-stx
    (if provide? #'define* #'define))
  
  (define def-ast-stx
    (syntax-parse stx
      [(_ name:id (view:vw ...) (fld:fspec ...)
          (~optional (~seq #:singleton (arg:expr ...)))
          (~optional (~seq #:custom-write writer:expr))
          (~optional (~seq #:struct-options (opt ...))))
       (define singleton? (attribute arg))
       (define singleton-id
         (and singleton? (format-id stx "the-~a" (syntax-e #'name))))
       (define conc-id #'name)
       (define fld-lst
         (for/list ([elem (attribute fld.spec)]
                    [ix (in-naturals)])
           (ConcFld (first elem) (second elem) ix)))
       ;;(pretty-print fld-lst)
       (define fld-id-lst (map ConcFld-id fld-lst))
       
       (define (mk-conc-info-h)
         (for/hasheq ([fld fld-lst])
           (values (syntax-e (ConcFld-id fld))
                   (list (ConcFld-qty fld) (ConcFld-ix fld)))))
       
       (define struct-def
         (quasisyntax
          (#,(if provide? #'concrete-struct* #'struct)
            name
            (#,@fld-id-lst)
            #:methods gen:custom-write
            [(define write-proc #,(if (attribute writer)
                                      #'writer
                                      (mkstx-ast-write conc-id fld-id-lst)))]
            #,@(list 
                #'#:methods #'gen:equal+hash
                (with-syntax ([(m ...) 
                               (mkstx-equal+hash conc-id fld-id-lst)])
                  #'[m ...]))
            #:methods gen:syntactifiable
            (#,@(mkstx-syntactifiable conc-id fld-id-lst))
            #:methods gen:strategic (#,@(mkstx-strategic conc-id fld-lst))
            #,@(let ((view-spec-lst (attribute view.spec)))
                 (if (null? view-spec-lst)
                     null
                     (let ((h (mk-conc-info-h))
                           (view-spec-lst
                            (extend-with-implied-views view-spec-lst)))
                       (apply
                        append
                        (for/list ([view-spec view-spec-lst])
                          (generate-view-methods conc-id view-spec h))))))
            #:transparent
            #,@(if (attribute opt)
                   (syntax->list #'(opt ...))
                   null))))
       #`(begin
           #,struct-def
           #,(mkstx-ast=? conc-id def-stx)
           #,@(mkstx-extra-accessors conc-id fld-id-lst def-stx)
           #,@(if singleton?
                  (with-syntax ((the-name singleton-id)
                                (def def-stx))
                    (list #'(def the-name (name arg ...))))
                  null))]))
  ;;(pretty-print (syntax->datum def-ast-stx))
  def-ast-stx)

(define-syntax* (define-ast stx)
  (mkstx-define-ast stx #f))

(define-syntax* (define-ast* stx)
  (mkstx-define-ast stx #t))

;;; 
;;; view-based traversals
;;; 

(define-syntax-rule* (make-view-term-visit-all name)
  (make-term-visit-all (view-term-fields-getter name)))

(define-syntax-rule* (make-view-term-rewrite-all name)
  (make-term-rewrite-all (view-term-fields-getter name)
                         (view-term-fields-setter name)))

(define-syntax-rule* (make-view-term-rewrite-some name)
  (make-term-rewrite-some (view-term-fields-getter name)
                          (view-term-fields-setter name)))

(define-syntax-rule* (make-view-term-rewrite-one name)
  (make-term-rewrite-one (view-term-fields-getter name)
                         (view-term-fields-setter name)))

(define-syntax-rule (define-view-combinator* n getf)
  (define-syntax-rule* (n vn)
    (lambda (s)
      (let ([f (getf vn)])
        (lambda (ast) (f s ast))))))

(define-view-combinator* make-view-all make-view-term-rewrite-all)
(define-view-combinator* make-view-some make-view-term-rewrite-some)
(define-view-combinator* make-view-one make-view-term-rewrite-one)
