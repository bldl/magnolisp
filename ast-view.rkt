#lang racket/base

#|

Definition and implementation of "views".

E.g.,

  (define-view* Def (#:fields annos id))

and then, within a macro, generate-view-methods may be used to
generate syntax for an implementation of the relevant generic methods
for a given view.

E.g.,

  (generate-view-methods #'DefVar (list #'Def '() #f))

  =>

  (list
    #'#:methods #'gen:Def
    #'[(define (Def-copy obj annos id)
         (struct-copy DefVar obj [annos annos] [id id]))
       (define (Def-annos obj) (DefVar-annos obj))
       (define (Def-id obj) (DefVar-id obj))
       (define (set-Def-annos obj annos)
         (struct-copy DefVar obj [annos annos]))
       (define (set-Def-id obj id)
         (struct-copy DefVar obj [id id]))])

|#

(require racket/generic racket/match "util.rkt"
         (for-syntax racket/base racket/list racket/match
                     racket/pretty racket/syntax syntax/parse
                     "util/module.rkt"))

;;; 
;;; view-based comparison
;;; 

;; Generates syntax for defining a view-based comparison function.
;; I.e., only the bits of the objects that are "in view" are compared.
;; For comparing concrete objects one can simply use `equal?`.
(define-for-syntax (make-view-equal view-id fld-id-lst def-fun-id)
  (define view-name (syntax-e view-id))
  (with-syntax ([view=? (format-id view-id "~a=?" view-name)]
                [view? (format-id view-id "~a?" view-name)]
                [def-fun def-fun-id]
                [expected (format "~a?" view-name)]
                [(get ...) (for/list ([id fld-id-lst])
                             (format-id view-id "~a-~a" 
                                        view-name (syntax-e id)))])
    #'(def-fun (view=? x y)
        (cond
         [(not (view? x))
          (raise-argument-error (quote view=?) expected 0 x y)]
         [(eq? x y)
          #t]
         [(not (view? y))
          (raise-argument-error (quote view=?) expected 1 x y)]
         [else
          (and (equal? (get x) (get y)) ... #t)]))))

;;; 
;;; view-based pattern matching
;;; 

;; Returns syntax for a match expander definition, for the specified
;; view, and its specified fields. Pattern matching is positional, so
;; the order of the fields in `fld-id-lst` matters.
(define-for-syntax (make-view-pattern view-id fld-id-lst def-pat-id)
  (define view-name (syntax-e view-id))
  
  (define var-lst
    (for/list ([id fld-id-lst])
      (generate-temporary (syntax-e id))))
  
  (define getter-lst
    (for/list ([id fld-id-lst])
      (format-id view-id "~a-~a" view-name (syntax-e id))))
  
  (with-syntax ([def-pat def-pat-id]
                [pat-name view-id]
                [view? (format-id view-id "~a?" view-name)]
                [(var ...) var-lst]
                [(fld-pat ...) (map
                                (lambda (x get)
                                  #`(app #,get #,x))
                                var-lst getter-lst)])
    #'(def-pat pat-name
        (lambda (stx)
          (syntax-case stx ()
            [(_ var ...)
             #'(? view? fld-pat ...)])))))

;;; 
;;; view-based term traversal support
;;; 

;; Returns syntax for a getter and a setter, which access sub-terms.
;; The implementations are purely in terms of view operations.
(define-for-syntax (make-trav-accessors def-fun-id view-id fld-spec-lst)
  (define view-name (syntax-e view-id))
  
  (for ((fld fld-spec-lst))
    (unless (ViewFld-qty fld)
      (error 'define-view
             "no `#:none`/`#:just`/`#:many` specified for ~a's field ~a"
             view-name (syntax-e (ViewFld-id fld)))))

  (define term-fld-lst
    (for/list ([fld fld-spec-lst]
               #:when (> (ViewFld-qty fld) 0))
      fld))
  (define term-fld-count (length term-fld-lst))
              
  (with-syntax ([def def-fun-id]
                [getter (format-id view-id "~a-term-fields" view-name)]
                [setter (format-id view-id "set-~a-term-fields" view-name)])
    (define accessor-stx-lst
      (list
       (with-syntax
         ([(get-fld ...)
           (for/list ([fld term-fld-lst])
             (format-id view-id "~a-~a" view-name
                        (syntax-e (ViewFld-id fld))))])
         #'(def (getter obj)
             (list (get-fld obj) ...)))
       (cond
         [(= term-fld-count 0)
          #'(def (setter obj lst)
              obj)]
         [(= term-fld-count 1)
          (define fld (car term-fld-lst))
          (with-syntax ([fld-v (generate-temporary (ViewFld-id fld))]
                        [set-fld
                         (format-id view-id "set-~a-~a" view-name
                                    (syntax-e (ViewFld-id fld)))])
            #'(def (setter obj lst)
                (match-define (list fld-v) lst)
                (set-fld obj fld-v)))]
         [else
          (define v-id-lst
            (generate-temporaries (map ViewFld-id fld-spec-lst)))
          (with-syntax ([(t-v ...)
                         (for/list ([fld fld-spec-lst]
                                    [v-id v-id-lst]
                                    #:when (> (ViewFld-qty fld) 0))
                           v-id)]
                        [(def-nt-v ...)
                         (for/list ([fld fld-spec-lst]
                                    [v-id v-id-lst]
                                    #:unless (> (ViewFld-qty fld) 0))
                           (with-syntax ([v v-id]
                                         [get
                                          (format-id view-id
                                                     "~a-~a" view-name
                                                     (syntax-e
                                                      (ViewFld-id fld)))])
                           #'(define v (get obj))))]
                        [(v ...) v-id-lst]
                        [copy (format-id view-id "~a-copy" view-name)])
            #'(def (setter obj lst)
                (match-define (list t-v ...) lst)
                def-nt-v ...
                (copy obj v ...)))])))
    ;;(pretty-print (list view-id fld-spec-lst (map syntax->datum accessor-stx-lst)))
    accessor-stx-lst))

(define-syntax* (view-term-fields-getter stx)
  (syntax-parse stx
    [(_ name:id)
     (format-id #'name "~a-term-fields" (syntax-e #'name))]))

(define-syntax* (view-term-fields-setter stx)
  (syntax-parse stx
    [(_ name:id)
     (format-id #'name "set-~a-term-fields" (syntax-e #'name))]))

;;; 
;;; view definition
;;; 

(begin-for-syntax
  (define-syntax-class* term-qty-num
    #:attributes (num)
    (pattern #:none #:attr num 0)
    (pattern #:just #:attr num 1)
    (pattern #:many #:attr num +inf.0))

  (define-splicing-syntax-class tqty?
    #:attributes (num)
    (pattern (~optional qty:term-qty-num)
             #:attr num (attribute qty.num)))

  ;; The `qty` field value is optional, and may be #f.
  (define-datatype* (ViewFld id qty)
    ([FieldViewFld] [AccessViewFld get set]) #:transparent)

  (define (ViewFld->syntax fld)
    (match fld
      [(FieldViewFld id qty)
       #`(FieldViewFld #'#,id #,qty)]
      [(AccessViewFld id qty get set)
       #`(AccessViewFld #'#,id #,qty #'#,get #'#,set)]))
  
  (define-syntax-class vfld
    #:description "view field specification"
    #:attributes (spec)
    [pattern (#:field qty:tqty? fld:id) 
             #:attr spec (FieldViewFld #'fld (attribute qty.num))]
    [pattern (#:access qty:tqty? fld:id get:expr set:expr)
             #:attr spec (AccessViewFld #'fld (attribute qty.num)
                                        #'get #'set)])

  (define-splicing-syntax-class vflds
    #:description "view fields specification"
    #:attributes (spec-lst) ;; (listof ViewFld)
    [pattern (~seq #:fields fld:id ...)
             #:attr spec-lst (map
                              (lambda (id) (FieldViewFld id #f))
                              (syntax->list #'(fld ...)))]
    [pattern (~seq fld:vfld ...)
             #:attr spec-lst (attribute fld.spec)])
  
  (define-syntax-class vspec
    #:description "view implementation specification"
    #:attributes (fspec-lst) ;; (listof ViewFld)
    [pattern (flds:vflds)
             #:attr fspec-lst (attribute flds.spec-lst)])
  
  (define-syntax-class* vspec-with-copy
    #:description "view implementation specification"
    #:attributes (fspec-lst copy) ;; (listof ViewFld) and syntax
    [pattern (flds:vflds (~optional (~seq #:copy copy:expr)))
             #:attr fspec-lst (attribute flds.spec-lst)]))

(define-for-syntax (make-define-view def-gen-id def-stx-id 
                                     def-pat-id def-fun-id stx)
  (define (generate view-id fld-spec-lst trav? opt-stx-lst)
    (define view-name (syntax-e view-id))
    (define fld-ids (map ViewFld-id fld-spec-lst))

    (with-syntax ([view view-id]
                  [(fld ...) fld-ids])
      (define (make-accessor-sigs fld-id)
        (define fld-name (syntax-e fld-id))
        (define getter-sig
          #`(#,(format-id stx "~a-~a" view-name fld-name) view))
        (define setter-sig
          #`(#,(format-id stx "set-~a-~a" view-name fld-name) view #,fld-id))
        (list getter-sig setter-sig))
    
      (define method-sig-lst
        (cons
         #`(#,(format-id stx "~a-copy" view-name) view fld ...)
         (apply append (map make-accessor-sigs fld-ids))))
    
      (with-syntax ([(method ...) method-sig-lst]
                    [def-gen def-gen-id]
                    [def-stx def-stx-id]
                    [(def-trav ...)
                     (if trav?
                         (make-trav-accessors def-fun-id view-id fld-spec-lst)
                         null)]
                    [def-pat (make-view-pattern view-id fld-ids def-pat-id)]
                    [def-equ (make-view-equal view-id fld-ids def-fun-id)]
                    [view-info (format-id stx "view:~a" view-name)]
                    [(gen-opt ...) opt-stx-lst]
                    [(entry ...) (map ViewFld->syntax fld-spec-lst)])
        #'(begin
            (def-stx view-info
              (list entry ...))
            (def-gen view
              gen-opt ...
              method ...)
            def-trav ...
            def-pat
            def-equ))))
  
  (syntax-parse stx
    [(_ view:id flds:vspec
        (~optional (~and #:support-traversals support-traversals))
        (~optional (~seq #:generics-options (opt ...))))
     (generate #'view 
               (attribute flds.fspec-lst)
               (and (attribute support-traversals) #t)
               (if (attribute opt)
                   (syntax->list #'(opt ...))
                   null))]))

(define-syntax* (define-view stx)
  (make-define-view #'define-generics #'define-syntax 
                    #'define-match-expander #'define stx))

(define-syntax* (define-view* stx)
  (make-define-view #'define-generics* #'define-syntax* 
                    #'define-match-expander* #'define* stx))

;;; 
;;; view implementation
;;; 

;; Generates syntax for specifying the implementation of the methods
;; of the specified view for the specified concrete struct type. The
;; resulting list of syntax objects is intended to be spliced into a
;; (struct ...) declaration. E.g., (generate-view-methods #'C (list
;; #'V '() #f #hasheq())) -> (list #'#:methods #'gen:V (...)).
(define-for-syntax* (generate-view-methods conc-id view-spec conc-qty-h)
  (match-define (list view-id fld-override-lst copy-lambda-stx) view-spec)
  (define view-name (syntax-e view-id))
  (define conc-name (syntax-e conc-id))
  (define fld-info-lst ;; (listof ViewFld)
    ;; Note that this call requires transformer context.
    (syntax-local-value (format-id view-id "view:~a" view-name)))
  
  (unless (null? fld-override-lst)
    ;;(write `(before override ,fld-info-lst)) (newline)
    (define h (make-hasheq)) ;; field name -> override info
    (for ([fld fld-override-lst])
      (hash-set! h (syntax-e (ViewFld-id fld)) fld))
    (set! fld-info-lst 
          (for/list ([fld fld-info-lst])
            (define n (syntax-e (ViewFld-id fld)))
            (define e (hash-ref h n #f))
            (if e 
                (begin0 e
                  (hash-remove! h n))
                fld)))
    (unless (hash-empty? h)
      (error 'generate-view-methods
             "overrides for non-existent fields of view ~a of ~a: ~a"
             view-name conc-name (hash-keys h)))
    ;;(write `(after override ,fld-info-lst)) (newline)
    (void))

  (for ([fld fld-info-lst])
    (when (FieldViewFld? fld)
      (define name (syntax-e (ViewFld-id fld)))
      (define c-qty (hash-ref conc-qty-h name #f))
      (when c-qty
        (define v-qty (ViewFld-qty fld))
        (when (and v-qty (not (= c-qty v-qty)))
          (error 'generate-view-methods
                 "quantities differ for field `~a`: ~a"
                 name
                 (format "~a (view) vs. ~a (concrete)" v-qty c-qty))))))
  
  (define fld-id-lst (map ViewFld-id fld-info-lst))
  
  (define (mk-setter-id fld-name)
    (format-id view-id "set-~a-~a" view-name fld-name))
  
  (with-syntax ([view view-id]
                [conc conc-id])
    ;; Returns (list/c syntax? syntax?) for getter and setter definitions.
    (define (make-accessor-impls fld-info)
      (match-define (ViewFld fld-id _) fld-info)
      (define-values (get-stx set-stx)
        (if (AccessViewFld? fld-info)
            (values (AccessViewFld-get fld-info)
                    (AccessViewFld-set fld-info))
            (values #f #f)))
      
      (define fld-name (syntax-e fld-id))
      
      (define (mk-default-get)
        (with-syntax ([c-getter-id
                       (format-id conc-id "~a-~a" conc-name fld-name)])
          #'(lambda (view) (c-getter-id view))))
      
      (define getter-impl
        (with-syntax ([v-getter-id
                       (format-id view-id "~a-~a" view-name fld-name)]
                      [v-getter-expr
                       (or get-stx (mk-default-get))])
          #'(define v-getter-id v-getter-expr)))
      
      (define setter-id (mk-setter-id fld-name))
      (define setter-impl
        (with-syntax 
            ([v-setter-id setter-id]
             [v-setter-expr
              (or set-stx
                  (with-syntax ([fld fld-id])
                    #'(lambda (view fld)
                        (struct-copy conc view [fld fld]))))])
          #'(define v-setter-id v-setter-expr)))
      
      (list getter-impl setter-impl))
    
    (define copy-impl
      (with-syntax ([v-copy (format-id view-id "~a-copy" view-name)])
        (cond
         [copy-lambda-stx
          (with-syntax ([copy-impl copy-lambda-stx])
            #'(define v-copy copy-impl))]
         [else
          (with-syntax ([(fld ...) fld-id-lst])
            ;; `set-info-lst` will have `#:access` fields, while
            ;; `copy-info-lst` will have `#:field` fields. Can just
            ;; `struct-copy` the latter, whereas the former must be
            ;; set as specified.
            (define-values (set-info-lst copy-info-lst)
              (partition AccessViewFld? fld-info-lst))
            #`(define (v-copy view fld ...)
                #,@(if (null? copy-info-lst)
                       null
                       (with-syntax ([(c-fld ...) 
                                      (map ViewFld-id copy-info-lst)])
                         (list 
                          #'(set! view
                                  (struct-copy conc view 
                                               [c-fld c-fld] ...)))))
                #,@(for/list ([info set-info-lst])
                     (define fld-id (ViewFld-id info))
                     (with-syntax ([s-fld fld-id]
                                   [set (mk-setter-id (syntax-e fld-id))])
                       #'(set! view (set view s-fld))))
                view))])))
    
    (define method-impl-lst
      (cons
       copy-impl
       (apply append (map make-accessor-impls fld-info-lst))))
    
    (list
     (quote-syntax #:methods)
     (format-id view-id "gen:~a" view-name)
     #`(#,@method-impl-lst))))
