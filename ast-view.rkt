#lang racket/base

#|

Definition and implementation of "views".

E.g.,

  (define-view* Def (#:fields annos id))

and then, within a macro, `generate-view-methods` may be used to
generate syntax for an implementation of the relevant generic methods
for a given view.

E.g.,

  (generate-view-methods #'DefVar (list #'Def '() #f) ....)

  =>

  (list
    #'#:property #'prop:Def
    #'#((lambda (obj) #t) ;; Def?
        (lambda (obj annos id) ;; Def-copy 
          (struct-copy DefVar obj [annos annos] [id id]))
        (lambda (obj) (DefVar-annos obj)) ;; Def-annos
        (lambda (obj annos) ;; set-Def-annos
          (struct-copy DefVar obj [annos annos]))
        (lambda (obj) (DefVar-id obj)) ;; Def-id
        (lambda (obj id) ;; set-Def-id
          (struct-copy DefVar obj [id id]))))

|#

(require racket/match racket/unsafe/ops "util.rkt"
         (for-syntax racket/base racket/dict racket/list racket/match
                     racket/pretty racket/syntax
                     syntax/id-table syntax/parse syntax/strip-context
                     "util/module.rkt"))

;; A version of `struct-copy` that does not take field accessor
;; lexical context from `field-id`s, but instead uses `type-id`.
(define-syntax (struct-copy/type-ctx stx)
  (syntax-parse stx
    [(_ type-id obj [field-id e] ...)
     (define ctx #'type-id)
     (with-syntax ([(cn ...)
                    (map (lambda (x) (replace-context ctx x))
                         (syntax->list #'(field-id ...)))])
       #'(struct-copy type-id obj [cn e] ...))]))

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
             "no quantity specified for ~a's field ~a"
             view-name (syntax-e (ViewFld-id fld)))))

  (define term-fld-lst
    (for/list ([fld fld-spec-lst]
               #:when (term-ViewFld? fld))
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
                                    #:when (term-ViewFld? fld))
                           v-id)]
                        [(def-nt-v ...)
                         (for/list ([fld fld-spec-lst]
                                    [v-id v-id-lst]
                                    #:unless (term-ViewFld? fld))
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
  ;; flds : (listof ViewFld), others : (listof identifier),
  ;; partial : boolean
  (struct ViewInfo (flds others partial) #:transparent)
  
  (define-syntax-class* term-qty
    #:attributes (qty)
    (pattern #:none #:attr qty 'none)
    (pattern #:maybe #:attr qty 'maybe)
    (pattern #:just #:attr qty 'just)
    (pattern #:many #:attr qty 'many))

  (define-splicing-syntax-class tqty?
    #:attributes (qty)
    (pattern (~optional tq:term-qty)
             #:attr qty (attribute tq.qty)))

  ;; The `qty` field value is optional, and may be #f.
  (define-datatype (ViewFld id qty)
    ([FieldViewFld] [AccessViewFld get set]) #:transparent)

  (define (term-ViewFld? fld)
    (not (eq? (ViewFld-qty fld) 'none)))
  
  (define (ViewFld->syntax fld)
    (match fld
      [(FieldViewFld id qty)
       #`(FieldViewFld #'#,id '#,qty)]
      [(AccessViewFld id qty get set)
       #`(AccessViewFld #'#,id '#,qty #'#,get #'#,set)]))
  
  (define-syntax-class vfld
    #:description "view field specification"
    #:attributes (spec)
    [pattern (#:field tq:tqty? fld:id) 
             #:attr spec (FieldViewFld #'fld (attribute tq.qty))]
    [pattern (#:access tq:tqty? fld:id get:expr set:expr)
             #:attr spec (AccessViewFld #'fld (attribute tq.qty)
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
  
  (define-syntax-class view-vspec
    #:description "default view implementation specification"
    #:attributes (fspec-lst) ;; (listof ViewFld)
    [pattern (flds:vflds)
             #:attr fspec-lst (attribute flds.spec-lst)])
  
  (define-syntax-class* conc-vspec
    #:description "overridden view implementation specification"
    #:attributes (fspec-lst copy predicate) ;; (listof ViewFld) and syntax
    [pattern (flds:vflds
              (~seq (~or
                     (~optional (~seq #:copy copy:expr))
                     (~optional (~seq #:predicate predicate:expr))) ...))
             #:attr fspec-lst (attribute flds.spec-lst)]))

(define-for-syntax (make-define-view def-vals-id def-stx-id 
                                     def-pat-id def-fun-id stx)
  (define (generate view-id fld-spec-lst other-id-lst partial? trav?)
    (define view-name (syntax-e view-id))
    (define fld-ids (map ViewFld-id fld-spec-lst))
    (define/with-syntax view view-id)
    (define/with-syntax (fld ...) fld-ids)
    (define/with-syntax def-fun def-fun-id)
    (define/with-syntax def-vals def-vals-id)
      
    (define (make-accessor-sigs fld-id)
      (define fld-name (syntax-e fld-id))
      (list
       #`(#,(format-id stx "~a-~a" view-name fld-name) view)
       #`(#,(format-id stx "set-~a-~a" view-name fld-name) view #,fld-id)))
    
    (define method-sig-lst
      (cons
       #`(#,(format-id stx "~a-copy" view-name) view fld ...)
       (append-map make-accessor-sigs fld-ids)))

    (define generics-stx-lst
      (let ()
        (define/with-syntax prop:view
          (format-id view-id "prop:~a" view-name))
        (define/with-syntax prop:view?
          (format-id view-id
                     (if partial? "prop:~a?" "~a?")
                     view-name))
        (define/with-syntax prop-value:view
          (format-id view-id "prop-value:~a" view-name))
        `(,#'(def-vals (prop:view prop:view? prop-value:view)
               (make-struct-type-property 'view))
            ,@(if partial?
                  (list
                   (with-syntax ([view?
                                  (format-id view-id "~a?" view-name)])
                     #'(def-fun (view? obj)
                         (and (prop:view? obj)
                              ((unsafe-vector*-ref (prop-value:view obj) 0)
                               obj)))))
                  null)
            ,@(for/list ([sig method-sig-lst]
                         [i (in-naturals (if partial? 1 0))])
                (syntax-parse sig
                  [(f:id obj:id arg:id ...)
                   (define/with-syntax ix i)
                   #'(def-fun (f obj arg ...)
                       ((unsafe-vector*-ref (prop-value:view obj) ix)
                        obj arg ...))])))))
      
    (with-syntax ([(generics ...) generics-stx-lst]
                  [def-stx def-stx-id]
                  [(def-trav ...)
                   (if trav?
                       (make-trav-accessors def-fun-id view-id fld-spec-lst)
                       null)]
                  [def-pat (make-view-pattern view-id fld-ids def-pat-id)]
                  [def-equ (make-view-equal view-id fld-ids def-fun-id)]
                  [view-info (format-id stx "view:~a" view-name)]
                  [partial-lit partial?]
                  [(other ...) other-id-lst]
                  [(entry ...) (map ViewFld->syntax fld-spec-lst)])
      #'(begin
          (def-stx view-info
            (ViewInfo (list entry ...)
                      (list #'other ...)
                      partial-lit))
          generics ...
          def-trav ...
          def-pat
          def-equ)))
  
  (syntax-parse stx
    [(_ view:id flds:view-vspec
        (~seq (~or
               (~optional (~seq #:also (other-view:id ...)))
               (~optional (~and #:partial partial))
               (~optional (~and #:traversable traversable))
               ) ...))
     (generate #'view 
               (attribute flds.fspec-lst)
               (if (attribute other-view)
                   (syntax->list #'(other-view ...))
                   null)
               (and (attribute partial) #t)
               (and (attribute traversable) #t))]))

(define-syntax* (define-view stx)
  (make-define-view #'define-values #'define-syntax 
                    #'define-match-expander #'define stx))

(define-syntax* (define-view* stx)
  (make-define-view #'define-values* #'define-syntax* 
                    #'define-match-expander* #'define* stx))

;;; 
;;; implied views
;;;

;; Adds implied views to `view-spec-lst`, removing duplicates.
;; Requires transformer context.
(define-for-syntax* (extend-with-implied-views view-spec-lst)
  (define (get-others-for view-id)
    (define view-info ;; ViewInfo
      (syntax-local-value (format-id view-id "view:~a" (syntax-e view-id))))
    (ViewInfo-others view-info))

  (define tbl (make-free-id-table))

  ;; Explicitly specified views.
  (for ((view-spec view-spec-lst))
    (match-define (list view-id _ _ _) view-spec)
    (free-id-table-set! tbl view-id view-spec))
  
  ;; Implied views.
  (for ((view-spec view-spec-lst))
    (define id (car view-spec))
    (define others (get-others-for id))
    (for ((other-id others))
      ;; Introduce it as if the user had specified it.
      (let ((other-id (syntax-local-introduce other-id)))
        (unless (dict-has-key? tbl other-id)
          ;; Naturally, any overrides must be specified explicitly; here
          ;; we assume no overrides.
          (free-id-table-set! tbl other-id (list other-id null #f #f))))))

  (dict-values tbl))

;;; 
;;; view implementation
;;; 

;; Generates syntax for specifying the implementation of the methods
;; of the specified view for the specified concrete struct type. The
;; resulting list of syntax objects is intended to be spliced into a
;; (struct ....) declaration. E.g., (generate-view-methods #'C (list
;; #'V '() #f) ....) -> (list #'#:property #'prop:V
;; #'(vector-immutable ....)).
(define-for-syntax* (generate-view-methods conc-id
                                           view-spec
                                           conc-info-h)
  ;;(writeln (list conc-id view-spec))
  (match-define (list view-id fld-override-lst
                      copy-lambda-stx pred-lambda-stx)
    view-spec)
  (define view-name (syntax-e view-id))
  (define conc-name (syntax-e conc-id))
  (define view-info ;; ViewInfo
    ;; Note that this call requires transformer context.
    (syntax-local-value (format-id view-id "view:~a" view-name)))
  (define fld-info-lst (ViewInfo-flds view-info)) ;; (listof ViewFld)
  
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
      (define c-info (hash-ref conc-info-h name #f))
      (when c-info
        (define c-qty (first c-info))
        (define v-qty (ViewFld-qty fld))
        (when (and v-qty (not (eq? c-qty v-qty)))
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
        (define ix (second (hash-ref conc-info-h fld-name)))
        #`(lambda (view) (unsafe-struct*-ref view #,ix)))
      
      (define getter-impl
        (or get-stx (mk-default-get)))
      
      (define setter-id (mk-setter-id fld-name))
      (define setter-impl
        (or set-stx
            (with-syntax ([fld fld-id])
              #'(lambda (view fld)
                  (struct-copy/type-ctx conc view [fld fld])))))
      
      (list getter-impl setter-impl))
    
    (define copy-impl
      (or
       copy-lambda-stx
       (with-syntax ([(fld ...) fld-id-lst])
         ;; `set-info-lst` will have `#:access` fields, while
         ;; `copy-info-lst` will have `#:field` fields. Can just
         ;; `struct-copy` the latter, whereas the former must be
         ;; set as specified.
         (define-values (set-info-lst copy-info-lst)
           (partition AccessViewFld? fld-info-lst))
         #`(lambda (view fld ...)
             #,@(if (null? copy-info-lst)
                    null
                    (with-syntax ([(c-fld ...) 
                                   (map ViewFld-id copy-info-lst)])
                      (list 
                       #'(set! view
                               (struct-copy/type-ctx
                                conc view 
                                [c-fld c-fld] ...)))))
             #,@(for/list ([info set-info-lst])
                  (define fld-id (ViewFld-id info))
                  (with-syntax ([s-fld fld-id]
                                [set (mk-setter-id (syntax-e fld-id))])
                    #'(set! view (set view s-fld))))
             view))))

    (define partial? (ViewInfo-partial view-info))

    (define partial-impl
      (cond
        [pred-lambda-stx
         (unless partial?
           (error 'generate-view-methods
                  "predicate overridden for non-#:partial view ~a: ~a"
                  view-name pred-lambda-stx))
         pred-lambda-stx]
        [else
         #'(lambda (x) #t)]))
    
    (define (maybe-cons p? x y)
      (if p? (cons x y) y))

    (define method-impl-lst
      (maybe-cons
       partial?
       partial-impl
       (cons
        copy-impl
        (append-map make-accessor-impls fld-info-lst))))
    
    (list
     (quote-syntax #:property)
     (format-id view-id "prop:~a" view-name)
     #`(vector-immutable #,@method-impl-lst))))
