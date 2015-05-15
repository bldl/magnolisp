#lang racket/base

#|

This is a basic Stratego-inspired term rewriting library for Racket.
This module defines generic and commonly used functionality. The
auxiliary modules "strategy-*.rkt" define additional
data-type-specific operations.

|#

(require "util.rkt" racket/generic
         (for-syntax racket/base syntax/parse))

;;; 
;;; List element access operations.
;;; 

;; Like `for-each`, except that does not accept multiple list
;; arguments.
(define* (list-visit-all s lst)
  (for-each s lst))

;; Like `map`, except that: does not accept multiple list arguments;
;; and if `s` returns #f, then stops mapping and returns #f. Returns
;; unmodified `in-lst` if `s` returns each element unmodified.
(define* (list-rewrite-all s in-lst)
  (define changed? #f)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        (if changed?
            (reverse res-lst)
            in-lst)
        (let* ((x (car lst))
               (res (s x)))
          (and res
               (let ()
                 (unless (eq? x res)
                   (set! changed? #t))
                 (next (cons res res-lst) (cdr lst))))))))

;; Like `map`, except that: does not accept multiple list arguments;
;; does not change elements for which `s` returns #f; and if `s`
;; returns #f for all elements, then returns #f. Returns unmodified
;; `lst` if `s` does not change any elements (i.e., `eq?`uivalence
;; holds).
(define* (list-rewrite-some s lst)
  (define changed? #f)
  (define some? #f)
  (define res (map (lambda (x)
                     (define y (s x))
                     (if y
                         (begin
                           (unless (eq? x y)
                             (set! changed? #t))
                           (set! some? #t)
                           y)
                         x))
                   lst))
  (and some? (if changed? res lst)))

;; Like `map`, but stops transforming elements in `lst` as soon as `s`
;; has produced a true value for an element. Does not change elements
;; for which `s` returns #f. If `s` returns #f for all elements, the
;; overall result will also be #f. Returns unmodified `lst` if `s`
;; does not change any elements.
(define* (list-rewrite-one s in-lst)
  (let next ((res-lst '())
             (lst in-lst))
    (if (null? lst)
        #f
        (let* ((x (car lst))
               (xs (cdr lst))
               (res (s x)))
          (if res
              (if (eq? x res)
                  in-lst
                  (append (reverse res-lst) (cons res xs)))
              (next (cons x res-lst) xs))))))

;;; 
;;; Subterm access operations.
;;; 

(define-generics* strategic
  (term-visit-all s strategic)
  (term-rewrite-all s strategic)
  (term-fields strategic)
  (set-term-fields strategic lst))

(define* (term-rewrite-some s strategic)
  (define o-lst (term-fields strategic))
  (define changed? #f)
  (define some? #f)
  (define n-lst 
    (for/list ([fv o-lst])
      (let ([nv (if (list? fv)
                    (list-rewrite-some s fv)
                    (s fv))])
        (if nv
            (begin
              (unless (eq? fv nv)
                (set! changed? #t))
              (set! some? #t)
              nv)
            fv))))
  (and some?
       (if changed?
           (set-term-fields strategic n-lst)
           strategic)))

(define* (term-rewrite-one s strategic)
  (define o-lst (term-fields strategic))
  (define changed? #f)
  (define one? #f)
  (define n-lst 
    (for/list ([fv o-lst])
      (if one? 
          fv
          (let ()
            (define nv (if (list? fv)
                           (list-rewrite-one s fv)
                           (s fv)))
            (if nv
                (begin
                  (unless (eq? fv nv)
                    (set! changed? #t))
                  (set! one? #t)
                  nv)
                fv)))))
  (and one?
       (if changed?
           (set-term-fields strategic n-lst)
           strategic)))

;;; 
;;; Default sub-object access operations.
;;; 

(define ((make-derived-visit-all get) s obj)
  (list-visit-all s (get obj)))

(define ((make-derived-rewrite list-rw get set) s obj)
  (define o-lst (get obj))
  (define n-lst (list-rw s o-lst))
  (and n-lst (if (eq? o-lst n-lst) obj (set obj n-lst))))

(define* (make-strategic-data-accessors
          get set
          #:visit-all [vall #f]
          #:rewrite-all [rall #f]
          #:rewrite-some [rsome #f]
          #:rewrite-one [rone #f])
  (hasheq 'fields get
          'set-fields set
          'visit-all (or vall (make-derived-visit-all get))
          'rewrite-all (or rall (make-derived-rewrite
                                 list-rewrite-all get set))
          'rewrite-some (or rsome (make-derived-rewrite
                                   list-rewrite-some get set))
          'rewrite-one (or rone (make-derived-rewrite
                                 list-rewrite-one get set))))

(define* strategic-term-accessors
  (make-strategic-data-accessors
   term-fields set-term-fields
   #:visit-all term-visit-all
   #:rewrite-all term-rewrite-all
   #:rewrite-some term-rewrite-some
   #:rewrite-one term-rewrite-one))

(define* current-strategic-data-accessors
  (make-parameter strategic-term-accessors))

(define-syntax-rule* (with-strategic-data-accessors acc e ...)
  (parameterize ([current-strategic-data-accessors acc])
    e ...))

(define (default-accessor name)
  (hash-ref (current-strategic-data-accessors) name))

;;; 
;;; Strategy definition forms.
;;; 

;; For defining strategies with overridable accessors (given as
;; keyword arguments).
(define-syntax (define-strategy*/accessor stx)
  (syntax-parse stx
    [(_ (n:id s:id kw:id ...) b:expr ...)
     (with-syntax ([(kw-spec ...)
                    (apply
                     append
                     (for/list ([id (syntax->list #'(kw ...))])
                       (list (string->keyword (symbol->string (syntax-e id)))
                             #`[#,id (default-accessor '#,id)])))])
       #'(define* (n s kw-spec ...)
           b ...))]))

;; For defining generic strategies that apply `s` on the
;; sub-components of the object, using the object accessor `f`, which
;; may be supplied as a keyword argument (otherwise the default is
;; used).
(define-syntax-rule (define-abstract-data-strategy* n f)
  (define-strategy*/accessor (n s f)
    (lambda (ast)
      (f s ast))))

;; For defining data type specific strategies, using an accessor as
;; given by `f-expr`.
(define-syntax-rule*
  (define-specific-data-strategy* n f-expr)
  (define* (n s)
    (let ([f f-expr])
      (lambda (ast)
        (f s ast)))))

;;; 
;;; Strategies.
;;; 

(define-abstract-data-strategy* all-visitor visit-all)
(define-abstract-data-strategy* all-rewriter rewrite-all)
(define-abstract-data-strategy* some-rewriter rewrite-some)
(define-abstract-data-strategy* one-rewriter rewrite-one)

(define-syntax-rule* (rec-lambda loop (arg ...) e ...)
  (lambda (arg ...)
    (let loop ([arg arg] ...)
      e ...)))

;; Tries a rewrite, restoring original term on failure.
(define* (try-rewriter s)
  (lambda (ast)
    (define r (s ast))
    (or r ast)))

;; Tries a rewrite, but restores original term on success.
(define* (where-rewriter s)
  (lambda (ast)
    (and (s ast) ast)))

;; Applies a rewrite rule `s` on `ast` for as long as it succeeds.
(define* (rewrite-repeat s ast)
  (let loop ([ast ast])
    (define r (s ast))
    (if r (loop r) ast)))

(define* ((repeat-rewriter s) ast)
  (rewrite-repeat s ast))

(define-syntax* and-rewrite
  ;; Note that (and e ...) defines left-to-right evaluation order, and
  ;; also that (and) == #t.
  (syntax-rules ()
    [(_ ast) ast]
    [(_ ast s . rest)
     (let ([ast (s ast)])
       (and ast (and-rewrite ast . rest)))]))

(define-syntax-rule* (or-rewrite ast s ...)
  ;; Note that (or e ...) defines left-to-right evaluation order, and
  ;; also that (or) == #f.
  (or (s ast) ...))

(define-strategy*/accessor (topdown-visitor s visit-all)
  (rec-lambda loop (ast)
    (s ast)
    (visit-all loop ast)
    (void)))

(define-strategy*/accessor (bottomup-visitor s visit-all)
  (rec-lambda loop (ast)
    (visit-all loop ast)
    (s ast)
    (void)))

(define-strategy*/accessor (topdown-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (define r (s ast))
    (and r (rewrite-all loop r))))

(define-strategy*/accessor (bottomup-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (define r (rewrite-all loop ast))
    (and r (s r))))

(define-strategy*/accessor (downup-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (and-rewrite ast s (fix rewrite-all loop) s)))

(define-strategy*/accessor (alltd-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (or-rewrite ast s (fix rewrite-all loop))))

(define-strategy*/accessor (oncetd-rewriter s rewrite-one)
  (rec-lambda loop (ast)
    (or-rewrite ast s (fix rewrite-one loop))))

;; Traverses top-down, applying `s` on each node for as many time as
;; it succeeds, keeping the latest successful rewrite result.
(define-strategy*/accessor (outermost-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (let ((ast (rewrite-repeat s ast)))
      (rewrite-all loop ast))))
    
(define-strategy*/accessor (innermost-rewriter s rewrite-all)
  (rec-lambda loop (ast)
    (let ((ast (rewrite-all loop ast)))
      (rewrite-repeat s ast))))

;; DEPRECATED
(define* (fail-rw x) #f)
(define* (id-rw x) x)

;; DEPRECATED
(provide (rename-out
          [repeat-rewriter repeat]
          [try-rewriter try]
          [where-rewriter where]
          [all-visitor all-visit]
          [all-rewriter all]
          [some-rewriter some]
          [one-rewriter one]
          [topdown-visitor topdown-visit]
          [bottomup-visitor bottomup-visit]
          [topdown-rewriter topdown]
          [bottomup-rewriter bottomup]
          [outermost-rewriter outermost]
          [innermost-rewriter innermost]
          ))
