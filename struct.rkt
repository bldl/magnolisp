#lang racket/base

#|

An API for declaring foreign structure types in Magnolisp. Simulation
is implemented in terms of Racket structs. Any C++ implementation must
be given separately.

Constructors are named with a `make-` prefix. Magnolisp type names in
turn get a `t:` prefix. A `match` expander gets defined, also for
expression positions.

|#

(require "util.rkt"
         "core.rkt" 
         (only-in "surface.rkt"
                  [define mgl.define] declare begin-racket
                  foreign type
                  -> auto for-all)
         racket/match
         (for-syntax racket/base racket/syntax
                     syntax/parse))

;; Returns syntax for a match pattern transformer, for things of the
;; specified predicate, and its specified field accessors. Pattern
;; matching is positional, so the order of the fields in `get-id-lst`
;; matters.
(define-for-syntax (make-match-pat-lam pred-id get-id-lst)
  (with-syntax*
    ([pred? pred-id]
     [(get ...) get-id-lst]
     [(pat ...) (generate-temporaries get-id-lst)]
     [(fld-pat ...) #'((app get pat) ...)])
    #'(lambda (stx)
        (syntax-case stx ()
          [(_ pat ...)
           #'(? pred? fld-pat ...)]))))

(define-for-syntax (make-match-expr-lam ctor-id t-id)
  (with-syntax ([ctor-n ctor-id]
                [t-n t-id])
    #'(lambda (stx)
        (syntax-parse stx
          [_:id #'t-n]
          [(_ . args) #'(ctor-n . args)]))))
  
(define-syntax* (define-foreign-struct stx)
  (define-splicing-syntax-class opts
    (pattern
     (~seq
      (~or
       (~optional (~seq #:inspector _:expr))
       (~optional (~seq #:guard _:expr))
       (~seq #:property _:expr _:expr)
       (~optional #:transparent)
       (~seq #:methods _:id _)
       ) ...)))

  (define-syntax-class fld
    #:datum-literals (::)
    (pattern
     (~or n:id [n:id :: given-t:expr])
     #:with t (or (attribute given-t) #'(auto))))

  (define-syntax-class maybe-typed-id
    #:datum-literals (::)
    (pattern
     (~or n:id [n:id :: given-t:expr])
     #:attr t (attribute given-t)))

  (define (->ctor id)
    (format-id id "make-~a" (syntax-e id)))
  (define (->pred id)
    (format-id id "~a?" (syntax-e id)))
  (define (->get id fld-id)
    (format-id id "~a-~a" (syntax-e id) (syntax-e fld-id)))

  (syntax-parse stx
    [(_ st:maybe-typed-id (f:fld ...) rkt-os:opts)
     (define fld-id-lst (syntax->list #'(f.n ...)))

     (define mgl-id #'st.n)
     (define/with-syntax mgl-n mgl-id)
     (define mgl-sym (syntax-e mgl-id))
     (define mgl-t-id (or (attribute st.t)
                          (format-id stx "t:~a" mgl-sym)))
     (define static-type? (not (attribute st.t)))
     (define mgl-ctor-id (->ctor mgl-id))
     (define mgl-pred-id (->pred mgl-id))
     (define mgl-get-id-lst
       (map
        (lambda (fld-id) (->get mgl-id fld-id))
        fld-id-lst))
     (define/with-syntax mgl-t-n mgl-t-id)

     (define rkt-id (generate-temporary mgl-sym))
     (define rkt-sym (syntax-e rkt-id))
     (define/with-syntax rkt-n rkt-id)

     (define/with-syntax
       (define-getter ...)
       (map
        (lambda (n-id t-stx)
          (define/with-syntax t t-stx)
          #`(mgl.define
             #,(->get mgl-id n-id)
             #:: (foreign [type (-> mgl-t-n t)])
             #,(->get rkt-id n-id)))
        fld-id-lst
        (syntax->list #'(f.t ...))))

     (define/with-syntax
       (define-type ...)
       (if static-type?
           (list #'(mgl.define
                    #:type mgl-t-n
                    #:: ([foreign mgl-n])))
           null))
     
     #`(begin
         (begin-racket
           (struct rkt-n (f.n ...)
             #:constructor-name #,(->ctor rkt-id)
             #:reflection-name 'mgl-n
             . rkt-os))
         define-type ...
         (mgl.define
          #,mgl-ctor-id
          #:: (foreign [type (-> f.t ... mgl-t-n)])
          #,(->ctor rkt-id))
         (mgl.define
          #,mgl-pred-id
          #:: (foreign [type (for-all T (-> T Bool))])
          #,(->pred rkt-id))
         define-getter ...
         (define-match-expander mgl-n
           #,(make-match-pat-lam mgl-pred-id mgl-get-id-lst)
           #,(make-match-expr-lam mgl-ctor-id mgl-t-id)))]))
