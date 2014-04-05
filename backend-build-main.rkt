#lang racket

#|

Routines for parsing and collecting 'build annotations, and generating
code for them.

|#

(require "ast-magnolisp.rkt" "backend-build-writer.rkt"
         "backend-util.rkt" "app-util.rkt"
         "util.rkt" "util/order.rkt"
         data/order data/splay-tree)

;;; 
;;; parsing
;;; 

(define (opt-name? s)
  (regexp-match? #rx"^[a-z][a-z0-9-]*$" s))

(define (allowed-symbol? sym)
  (opt-name? (symbol->string sym)))

(define (opt-value-number? v)
  (any-pred-holds exact-integer? hexnum? v))

(define (opt-value-atom/c v)
  (any-pred-holds boolean? opt-value-number? string? symbol? v))

(define (opt-value-atom-pred v)
  (ormap
   (lambda (p?)
     (and (p? v) p?))
   (list boolean? opt-value-number? string? symbol?)))

(define (opt-value-number->number v)
  (if (hexnum? v) (hexnum-num v) v))

(define (opt-value-number-comparator x y)
  (define x-n (opt-value-number->number x))
  (define y-n (opt-value-number->number y))
  (cond
   ((= x-n y-n) '=)
   ((< x-n y-n) '<)
   (else '>)))

(define opt-value-number-order
  (order 'number-order
         opt-value-number?
         opt-value-number-comparator))

(define (order-for-opt-value-atom v)
  (cond
   ((boolean? v) boolean-order)
   ((opt-value-number? v) opt-value-number-order)
   ((string? v) string-order)
   ((symbol? v) symbol-order)
   (else (assert #f))))

(define opt-value-set/c
  (cons/c predicate/c ordered-dict?))

(define ir-opt-value/c
  (or/c opt-value-atom/c opt-value-set/c))

(define opt-value/c
  (or/c opt-value-atom/c list?))

;; The returned dictionary has Lispy, all lowercase strings as keys,
;; i.e., opt-name? holds for the keys. The ir-opt-value/c contract
;; holds for the values, which may be either atoms or sets of values.
;; Set elements are also ordered, and must all be of the same value
;; type.
(define-with-contract*
  (-> (listof (list/c Id? syntax?))
      ordered-dict?)
  (parse-analyze-build-annos/ir build-lst)
  
  (define h (make-splay-tree string-order
                             #:key-contract opt-name?
                             #:value-contract ir-opt-value/c))

  (define (to-name id-ast n-stx)
    (define s (symbol->string (syntax-e n-stx)))
    (unless (opt-name? s)
      (raise-syntax-error
       #f
       (format "illegal build option name for definition ~a"
               (Id-name id-ast))
       n-stx))
    s)

  (define (to-value id-ast opt-stx v-stx)
    (define v
      (syntax-case v-stx ()
        ((#:hex h-v)
         (exact-integer? (syntax-e #'h-v))
         (hexnum (syntax->datum #'h-v)))
        (_
         (syntax->datum v-stx))))
    (define p? (opt-value-atom-pred v))
    (unless p?
      (raise-language-error
       #f
       "illegal value for build option"
       opt-stx
       v-stx
       #:continued
       (format "(for declaration ~a)" (Id-name id-ast))))
    (when (and (symbol? v) (not (allowed-symbol? v)))
      (raise-language-error
       #f
       "too exotic a symbol for build option"
       opt-stx
       v-stx
       #:continued
       (format "(for declaration ~a)" (Id-name id-ast))))
    (values p? v))
  
  (define (set-value-opt! id-ast opt-stx n-stx v-stx)
    (define n (to-name id-ast n-stx))
    (define-values (p? v) (to-value id-ast opt-stx v-stx))
    (if (dict-has-key? h n)
        (let ()
          (define x-v (dict-ref h n))
          (unless (equal? x-v v)
            (error 'parse-analyze-build-annos
                   "conflicting redefinition of build option ~a for definition ~a (previously: ~s): ~s"
                   n (Id-name id-ast) x-v v-stx)))
        (dict-set! h n v)))

  (define (set-set-opt! id-ast opt-stx n-stx v-stx-lst)
    (assert (not (null? v-stx-lst)))
    (define n (to-name id-ast n-stx))
    (define-values (type-p? v-h)
      (if (dict-has-key? h n)
          (let ((p (dict-ref h n)))
            (define-values (type-p? v-h) (values (car p) (cdr p)))
            (unless (ordered-dict? v-h)
              (error 'parse-analyze-build-annos
                     "conflicting use of operator += with build option ~a for definition ~a (previously defined as non-set ~s): ~s"
                     n (Id-name id-ast) v-h opt-stx))
            (values type-p? v-h))
          (values #f #f)))
    (for ((v-stx v-stx-lst))
      (define-values (p? v) (to-value id-ast opt-stx v-stx))
      (if type-p?
          (unless (type-p? v)
            (raise-syntax-error
             #f
             (format "type mismatch for definition ~a build option ~a value (expected ~a)" (Id-name id-ast) n (object-name type-p?))
             opt-stx v-stx))
          (set!-values
           (type-p? v-h)
           (values p? (make-splay-tree (order-for-opt-value-atom v)))))
      (dict-set! v-h v #t))
    (dict-set! h n (cons type-p? v-h)))

  (define (parse-opt! id-ast build-stx opt-stx)
    (syntax-case opt-stx ()
      (n
       (identifier? #'n)
       (set-value-opt! id-ast opt-stx #'n #'#t))
      ((n v)
       (identifier? #'n)
       (set-value-opt! id-ast opt-stx #'n #'v))
      ((p n v more-v ...)
       (and (eq? '+= (syntax-e #'p)) (identifier? #'n))
       (set-set-opt! id-ast opt-stx #'n
                     (syntax->list #'(v more-v ...))))
      (_
       (raise-language-error
        'build
        "illegal (build ...) annotation sub-form"
        build-stx
        opt-stx
        #:continued
        (format "(for declaration ~a)" (Id-name id-ast))))))

  (define (parse-build! id-ast build-stx)
    (syntax-case build-stx ()
      ((_ . opt)
       (for-each
        (fix parse-opt! id-ast build-stx)
        (syntax->list #'opt)))
      (_
       (raise-language-error
        'build
        "illegal (build ...) annotation form"
        build-stx
        #:continued
        (format "(for declaration ~a)" (Id-name id-ast))))))
  
  (for ((build build-lst))
    (define-values (id-ast build-stx) (apply values build))
    (parse-build! id-ast build-stx))
    
  h)

;; Any returned lists are sorted. The (list/c string? opt-value/c)
;; part of the signature could be more accurately given as (list/c
;; opt-name? (or/c opt-value-atom/c (listof opt-value-atom/c))).
(define-with-contract*
  (-> (listof (list/c Id? syntax?))
      (listof (list/c string? opt-value/c)))
  (parse-analyze-build-annos build-lst)

  (define (mk-lst v)
    (define h (cdr v))
    (for/list (((k v) (in-dict h)))
      k))
  
  (define h (parse-analyze-build-annos/ir build-lst))
  (for/list (((n v) (in-dict h)))
    (list n (if (pair? v) (mk-lst v) v))))

;;; 
;;; collection
;;;

(define-with-contract*
  (-> hash? (listof (list/c Id? syntax?)))
  (defs-collect-build-annos defs)
  (define lst null)

  (define (add! id build-stx)
    (set! lst (cons (list id build-stx) lst)))
  
  (for (((id def) (in-dict defs)))
    (assert (Def? def))
    (define b (ast-anno-maybe def 'build))
    (when b (add! (Def-id def) b)))
  
  lst)

;;; 
;;; code generation
;;;

(define-with-contract*
  (-> symbol? list? path-string? output-port? boolean? void?)
  (generate-build-file kind attrs path-stem out banner?)

  (define-values (writer sfx pfx) (get-writer-etc kind))
  (define path (path-add-suffix path-stem sfx))
  (define filename (path-basename-as-string path))
  
  (write-generated-output
   path out
   (thunk
    (when banner?
      (display-banner pfx filename))
    (writer path attrs)))

  (void))
