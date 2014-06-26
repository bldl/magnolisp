#lang racket/base

#|

Definition and implementation of "views".

E.g.,

  (define-view* Def (#:fields annos id))

and then, within a macro, generate-view-methods may be used to
generate syntax for an implementation of the relevant generic methods
for a given view.

E.g.,

  (generate-view-methods #'DefVar #'Def)

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

(require "util.rkt" racket/generic racket/match
         (for-syntax racket/base racket/list
                     racket/pretty racket/syntax syntax/parse))

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
;;; view definition
;;; 

(begin-for-syntax
  (define-syntax-class vfld
    #:description "view field specification"
    #:attributes (spec)
    [pattern (#:field fld:id) 
             #:attr spec (list #'fld #f #f)]
    [pattern (#:access fld:id get:expr set:expr)
             #:attr spec (list #'fld #'get #'set)])

  (define-splicing-syntax-class vflds
    #:description "view fields specification"
    #:attributes (spec-lst)
    [pattern (~seq #:fields fld:id ...)
             #:attr spec-lst (map
                              (lambda (id) (list id #f #f))
                              (syntax->list #'(fld ...)))]
    [pattern (~seq fld:vfld ...)
             #:attr spec-lst (attribute fld.spec)])
  
  (define-syntax-class vspec
    #:description "view implementation specification"
    #:attributes (fspec-lst)
    [pattern (flds:vflds)
             #:attr fspec-lst (attribute flds.spec-lst)])
  
  (define-syntax-class vspec-with-copy
    #:description "view implementation specification"
    #:attributes (fspec-lst copy)
    [pattern (flds:vflds (~optional (~seq #:copy copy:expr)))
             #:attr fspec-lst (attribute flds.spec-lst)])
  
  (provide vspec-with-copy))

(define-for-syntax (make-define-view def-gen-id def-stx-id 
                                     def-pat-id def-fun-id stx)
  (define (generate view-id fld-spec-lst opt-stx-lst)
    (define view-name (syntax-e view-id))
    (define fld-ids (map car fld-spec-lst))

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
                    [def-pat (make-view-pattern view-id fld-ids def-pat-id)]
                    [def-equ (make-view-equal view-id fld-ids def-fun-id)]
                    [view-info (format-id stx "view:~a" view-name)]
                    [(gen-opt ...) opt-stx-lst]
                    [(entry ...) 
                     (for/list ([fld fld-spec-lst])
                       (define-values (id-stx get-stx set-stx)
                         (apply values fld))
                       (if (not get-stx)
                           #`(list #'#,id-stx #f #f)
                           #`(list #'#,id-stx #'#,get-stx #'#,set-stx)))])
        #'(begin
            (def-stx view-info
              (list entry ...))
            (def-gen view
              gen-opt ...
              method ...)
            def-pat
            def-equ))))
  
  (syntax-parse stx
    [(_ view:id flds:vspec
        (~optional (~seq #:generics-options (opt ...))))
     (generate #'view 
               (attribute flds.fspec-lst)
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
;; (struct ...) declaration. E.g., (generate-view-methods #'C #'V)
;; -> (list #'#:methods #'gen:V (...)).
(define-for-syntax* (generate-view-methods conc-id view-id [singleton? #f])
  (define view-name (syntax-e view-id))
  (define conc-name (syntax-e conc-id))
  (define fld-info-lst
    ;; Note that this call requires transformer context.
    (syntax-local-value (format-id view-id "view:~a" view-name)))
  (define fld-id-lst (map car fld-info-lst))

  (define (mk-setter-id fld-name)
    (format-id view-id "set-~a-~a" view-name fld-name))
  
  (with-syntax ([view view-id]
                [conc conc-id])
    (define (make-accessor-impls fld-info)
      (define-values (fld-id get-stx set-stx)
        (apply values fld-info))
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
                  (with-syntax
                      ([fld fld-id])
                    (if singleton?
                        #'(lambda (view fld)
                            (error 'v-setter-id 
                                   "cannot copy a singleton (~a)" 'conc))
                        #'(lambda (view fld)
                            (struct-copy conc view [fld fld])))))])
          #'(define v-setter-id v-setter-expr)))
      
      (list getter-impl setter-impl))
    
    (define copy-impl
      (with-syntax ([(fld ...) fld-id-lst]
                    [v-copy (format-id view-id "~a-copy" view-name)])
        (if singleton?
            #'(define (v-copy view fld ...)
                (error 'v-copy "cannot copy a singleton (~a)" 'conc))
            (let ()
              (define-values (set-info-lst copy-info-lst)
                (partition (lambda (x) (caddr x)) fld-info-lst))
              #`(define (v-copy view fld ...)
                  #,@(if (null? copy-info-lst)
                         null
                         (with-syntax ([(c-fld ...) 
                                        (map car copy-info-lst)])
                          (list 
                           #'(set! view
                                   (struct-copy conc view 
                                                [c-fld c-fld] ...)))))
                  #,@(for/list ([info set-info-lst])
                       (define fld-id (car info))
                       (with-syntax ([s-fld fld-id]
                                     [set (mk-setter-id (syntax-e fld-id))])
                         #'(set! view (set view s-fld))))
                  view)))))
    
    (define method-impl-lst
      (cons
       copy-impl
       (apply append (map make-accessor-impls fld-info-lst))))
    
    (list
     (quote-syntax #:methods)
     (format-id view-id "gen:~a" view-name)
     #`(#,@method-impl-lst))))
