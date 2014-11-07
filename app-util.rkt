#lang racket/base

#|

Application-specific utilities shared by both the #lang and the
compiler.

|#

(require "racket-5-compat.rkt" "util.rkt"
         racket/contract racket/dict racket/function
         racket/list racket/pretty
         syntax/id-table syntax/stx)

;;; 
;;; gensym
;;; 

;; Produces interned, deterministically-numbered symbols. They are
;; only unique in a context as indicated by `r`.
(define* (next-gensym r sym)
  (define num (hash-ref r sym 0))
  (define n-sym
    (if (= num 0)
        sym
        (string->symbol
         (string-append (symbol->string sym) "_"
                        (number->string num)))))
  (values (hash-set r sym (+ num 1)) n-sym))

;;; 
;;; identifier tables
;;; 

(define* (id-table? x)
  (or (free-id-table? x)
      (bound-id-table? x)))

(define* (immutable-id-table? x)
  (or (immutable-free-id-table? x)
      (immutable-bound-id-table? x)))

;;; 
;;; debugging utilities
;;; 

(define* (show-matches-in-id-table id-stx d)
  (writeln
   (cons
    `(looking-for ,(syntax-e id-stx))
    (dict-map
     d
     (lambda (k v)
       (cond
        ((bound-identifier=? k id-stx 0) `(bound ,(syntax-e k)))
        ((free-identifier=? k id-stx 0 0) `(free ,(syntax-e k)))
        (else #f)))))))

;; E.g.
;; (let* ((x 1)
;;        (x-id #'x))
;;   (let ((x 1))
;;     (syntax->datum/free-id #`(x x #,x-id y))))
;; See also: identifier-binding-symbol
(define* (syntax->datum/free-id stx)
  (define n 0)
  (define h (make-free-id-table #:phase 0))
  (define (f x)
    (cond
     ((null? x)
      null)
     ((pair? x)
      (cons (f (car x)) (f (cdr x))))
     ((syntax? x)
      (define uw (syntax-e x))
      (cond
       ((pair? uw)
        (f uw))
       ((symbol? uw)
        (define cur-n (dict-ref h x #f))
        (unless cur-n
          (set!-values (n cur-n) (values (+ n 1) n))
          (dict-set! h x cur-n))
        (string->symbol (format "~a.~a" (syntax-e x) cur-n)))
       (else
        ;; Composite types other than pairs (vectors, boxes, etc.) not
        ;; supported at this time.
        (syntax->datum x))))
     (else
      (error 'syntax->datum/free-id "unsupported: ~s" x))))
  (f stx))

(define (mpi->datum/split mpi)
  (define (mpi->datum mpi [root? #f])
    (define-values (m b) (module-path-index-split mpi))
    (cond
     ((and root? (not (or m b))) '(self))
     ((not m) null)
     ((not b) (list m))
     ((module-path-index? b) (append (mpi->datum b) (list m)))
     ((resolved-module-path? b) (list b m))
     (else (raise-result-error
            'module-path-index-split "documented result" 0 m b))))
  (mpi->datum mpi #t))

(define (mpi->datum/resolve mpi)
  (resolved-module-path-name (module-path-index-resolve mpi)))

(define* (id->datum/detailed id-stx 
                             #:conv-mpi mpi->datum
                             #:phase [ph (syntax-local-phase-level)])
  (define b (identifier-binding id-stx ph))
  (define name (syntax-e id-stx))
  (cond
   ((not b) `[,name : #f])
   ((eq? b 'lexical) `[,name : lexical])
   ((list? b)
    (define mpi (first b))
    (define sym (second b))
    `[,name : ,(mpi->datum mpi)
            ,@(if (eq? sym name) '() `(RENAMED FROM ,sym))])
   (else
    (raise-result-error 'identifier-binding
     "documented identifier-binding result" b))))

(define* (id->datum/detailed/sym id-stx #:conv-mpi mpi->datum)
  (define b (identifier-binding id-stx))
  (define name (syntax-e id-stx))
  (define v
    (cond
     ((not b) #f)
     ((eq? b 'lexical) "lexical")
     ((list? b)
      (define mpi (first b))
      (define sym (second b))
      (format "~a~a"
              (if (eq? sym name) "" (format "was:~a " sym))
              (mpi->datum mpi)))
     (else
      (raise-result-error 'identifier-binding
                          "documented identifier-binding result" b))))
  (if (not v)
      name
      (string->symbol (format "~s«~a»" name v))))

;; Tries to make it easy to see at a glance which IDs have bindings
;; and which do not, and what kind they are.
(define* (id->datum/phase id #:conv-mpi dummy)
  (define (pick-glyph)
    (define b- (identifier-binding id -1))
    (define b0 (identifier-binding id 0))
    (define b+ (identifier-binding id 1))
    (cond
     ((not (or b- b0 b+)) "ø")
     ((eq? b0 'lexical) 
      (assert (eq? b- 'lexical)) 
      (assert (eq? b+ 'lexical)) 
      "$")
     (else
      (format "~a~a~a"
              (if b- (begin (assert (list? b-)) "¥") "")
              (if b0 (begin (assert (list? b0)) "£") "")
              (if b+ (begin (assert (list? b+)) "€") "")))))
  (string->symbol
   (format "~a~a" (syntax-e id) (pick-glyph))))

(define* (id->datum/plain id #:conv-mpi dummy)
  (syntax-e id))
  
(define* (syntax->datum/binding stx
                                #:conv-id [id->datum id->datum/detailed]
                                #:conv-mpi [mpi->datum mpi->datum/resolve]
                                #:pred [p? (lambda (x) #t)])
  (define (f x)
    (cond
     ((null? x)
      null)
     ((pair? x)
      (cons (f (car x)) (f (cdr x))))
     ((syntax? x)
      (define uw (syntax-e x))
      (cond
       ((pair? uw)
        (f uw))
       ((symbol? uw)
        (if (p? uw) (id->datum x #:conv-mpi mpi->datum) uw))
       (else
        ;; Composite types other than pairs (vectors, boxes, etc.) not
        ;; supported at this time.
        (syntax->datum x))))
     (else
      (error 'syntax->datum/binding "unsupported: ~s" x))))
  (f stx))

(define (sym-add-sfx sfx sym)
  (string->symbol (string-append (symbol->string sym) sfx)))

(define (sym-add-pfx pfx sym)
  (string->symbol (string-append pfx (symbol->string sym))))

(define* (stx->datum/source f stx)
  (define src 
    (or (let-and p (syntax-source stx)
          (if (path? p)
              (let ()
                (define-values (d f d?) (split-path p))
                f)
              p))
        'ø))
  (define uw (syntax-e stx))
  (cond
   [(pair? uw)
    (cons (string->symbol (format "~a" src)) (f uw))]
   [(symbol? uw)
    (string->symbol (format "~a:~a" src uw))]
   [else
    (syntax->datum stx)]))

(define* (stx->datum/any-loc? f stx)
  (define has? (syntax-source stx))
  (define sigil (if has? '¤ 'ø))
  (define uw (syntax-e stx))
  (cond
   [(pair? uw)
    (cons sigil (f uw))]
   [(symbol? uw)
    (sym-add-pfx (symbol->string sigil) uw)]
   [else
    (syntax->datum stx)]))

;; `stx->datum` is invoked as (stx->datum f stx), where `f` can be
;; used to convert any sub-forms.
(define* (syntax->datum/loc stx
                            #:stx->datum [stx->datum stx->datum/any-loc?]
                            #:pred [p? (lambda (x) #t)])
  (define (f x)
    (cond
     ((null? x)
      null)
     ((pair? x)
      (cons (f (car x)) (f (cdr x))))
     ((syntax? x)
      (cond
       [(p? x) (stx->datum f x)]
       [else
        (define uw (syntax-e x))
        (cond
         [(pair? uw)
          (f uw)]
         [(symbol? uw)
          (f uw)]
         [else
          ;; Composite types other than pairs (vectors, boxes, etc.) not
          ;; supported at this time.
          (syntax->datum x)])]))
     (else
      (error 'syntax->datum/loc "unsupported: ~s" x))))
  (f stx))

(define* (print-with-select-syntax-properties props stx)
  (define (props-for stx)
    (define lst
      (for/list ((prop props))
        (define v (syntax-property stx prop))
        (and v `(,prop = ,v))))
    (filter identity lst))

  (define (print-for stx)
    (define vs (props-for stx))
    (unless (null? vs)
      (writeln `(PROPS ,@vs PRESENT IN ,stx))))
  
  (define (f stx)
    (define lst (syntax->list stx))
    (cond
     (lst (print-for stx)
          (for-each f lst))
     (else (print-for stx))))

  (pretty-print (syntax->datum stx))
  (f stx))

;;; 
;;; error reporting
;;; 

(define-syntax-rule* (unsupported v ...)
  (error "unsupported" v ...))

(define-with-contract*
  (-> syntax? (or/c symbol? #f))
  (form-get-name stx)
  (define x (syntax-e stx))
  (cond
   ((symbol? x) x)
   ((pair? x)
    (define y (syntax-e (car x)))
    (and (symbol? y) y))
   (else #f)))

;; Unlike with exn:fail:syntax, 'exprs' need not contain syntax
;; objects. It is a (listof any/c).
(concrete-struct* exn:fail:language exn:fail (exprs) #:transparent)

(define field/c
  (let ([option/c (or/c 'value 'multi 'maybe)])
    (or/c string? (cons/c string? (listof option/c)))))

(define-with-contract*
  (->* ((or/c symbol? #f) string?)
       (any/c any/c (listof any/c)
              #:continued (or/c string? (listof string?))
              #:fields (listof (list/c field/c any/c)))
       any)
  ;; The 'name' symbol here has different semantics compared to
  ;; raise-syntax-error, as it is only used as the fallback value if
  ;; neither 'expr' nor 'sub-expr' have no symbol to extract.
  (raise-language-error name message
                        [expr #f]
                        [sub-expr #f]
                        [extra-exprs null]
                        #:continued [continued null]
                        #:fields [more-fields null])
  (define (get-sym x)
    (and (syntax? x)
         (if (identifier? x)
             (syntax-e x)
             (and (stx-pair? x)
                  (let ((y (car (syntax-e x))))
                    (and (identifier? y)
                         (syntax-e y)))))))
  (define n (or (get-sym expr) (get-sym sub-expr) name '?))
  (define exprs (append
                 (if sub-expr (list sub-expr) null)
                 (if expr (list expr) null)
                 extra-exprs))
  (define (mk s e)
    (if (syntax? e)
        (list s (syntax->datum e)
              (string-append s " (syntax)") e
              (string-append s " (origin)")
              (let ((orig (syntax-property e 'origin)))
                (and orig (not (null? orig))
                     (map (lambda (id)
                            (if (identifier? id)
                                (syntax-e id) id)) orig))))
        (list s e)))
  (define fields
    (apply append (mk "at" sub-expr) (mk "in" expr) more-fields))
  (apply raise-misc-error n message fields
         #:continued continued
         #:constructor (lambda (s cs)
                         (exn:fail:language s cs exprs))))

