#lang racket

#|
|#

(require "racket-5-compat.rkt" "util.rkt"
         syntax/id-table syntax/stx)

(define* (next-gensym r sym)
  (define num (hash-ref r sym 0))
  (define n-sym
    (if (= num 0)
        sym
        (string->symbol
         (string-append (symbol->string sym) "_"
                        (number->string num)))))
  (values (hash-set r sym (+ num 1)) n-sym))

(define-syntax-rule* (unsupported v ...)
  (error "unsupported" v ...))

(define* (id-table? x)
  (or (free-id-table? x)
      (bound-id-table? x)))

(define* (immutable-id-table? x)
  (or (immutable-free-id-table? x)
      (immutable-bound-id-table? x)))

(define* (global-id? id)
  (and (identifier? id)
       (not (eq? (identifier-binding id 0) 'lexical))))

;; First argument should be a known global identifier to compare
;; against. Matching is done at phase level 0.
(define-with-contract*
  (-> identifier? any/c boolean?)
  (matches-global-id? g-id x)
  (and (global-id? x)
       (global-id? g-id)
       (or (free-identifier=? g-id x 0 0)
           (eq? (syntax-e g-id) (syntax-e x)))))

(define-with-contract*
  (-> identifier? identifier? boolean?)
  (def-identifier=? id-1 id-2)
  (free-identifier=?
   (or (syntax-property id-1 'def-id) id-1)
   (or (syntax-property id-2 'def-id) id-2)))

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

(define (id->datum id-stx)
  (define b (identifier-binding id-stx))
  (define name (syntax-e id-stx))
  (define (mpi->datum mpi)
    (define-values (m b) (module-path-index-split mpi))
    (list m b))
  (cond
   ((not b) `[,name : #f])
   ((eq? b 'lexical) `[,name : lexical])
   ((list? b) `[,name : ,@(mpi->datum (first b)) ,(second b)])
   (else
    (raise-result-error 'identifier-binding
     "documented identifier-binding result" b))))

;; Tries to make it easy to see at a glance which IDs have bindings
;; and which do not, and what kind they are.
(define* (id->datum/phase id)
  (define (pick-glyph)
    (define b-0 (identifier-binding id 0))
    (define b-1 (identifier-binding id 1))
    (cond
     ((not (or b-0 b-1)) "ø")
     ((eq? b-0 'lexical) (assert (eq? b-1 'lexical)) "$")
     (else
      (format "~a~a"
              (if b-0 (begin (assert (list? b-0)) "£") "")
              (if b-1 (begin (assert (list? b-1)) "€") "")))))
  (string->symbol
   (format "~a~a" (syntax-e id) (pick-glyph))))

(define* (syntax->datum/binding stx
                                #:conv [id->datum id->datum]
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
        (if (p? uw) (id->datum x) uw))
       (else
        ;; Composite types other than pairs (vectors, boxes, etc.) not
        ;; supported at this time.
        (syntax->datum x))))
     (else
      (error 'syntax->datum/binding "unsupported: ~s" x))))
  (f stx))

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

;;; 
;;; C++ identifiers
;;; 

(define* (string-cxx-id? s)
  (not (or (regexp-match? #rx"^[^a-zA-Z_]" s)
           (regexp-match? #rx"[^a-zA-Z0-9_]" s)
           (= (string-length s) 0))))

(define (translate-id-string s)
  (when-let r (regexp-match #rx"^(.*)[?]$" s)
    (set! s (string-append "is_" (second r))))
  (when-let r (regexp-match #rx"^(.*)[=]$" s)
    (set! s (string-append (second r) "_equal")))
  (set! s (regexp-replace* #rx"->" s "_to_"))
  s)  

(define* (string->maybe-cxx-id s)
  (set! s (translate-id-string s))
  (set! s (regexp-replace #rx"[!?=]+$" s ""))
  (set! s (string-underscorify s))
  (and (string-cxx-id? s) s))

(define* (string->exported-cxx-id o-s)
  (define s (string->maybe-cxx-id o-s))
  (unless s
    (error
     'string->exported-cxx-id
     "illegal name for a C++ export: ~s" o-s))
  s)

(define* (string->internal-cxx-id s #:default [default #f])
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (translate-id-string s))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  (if (and default (= (string-length s) 0))
      default s))

;;; 
;;; debugging utilities
;;; 

(define* (pretty-print-id-table d)
  (pretty-print (dict-map d cons)))

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

(define* (print-with-select-syntax-properties props stx)
  (define (props-for stx)
    (define lst
      (for/list ((prop props))
        (define v (syntax-property stx prop))
        (and v (cons prop v))))
    (filter identity lst))

  (define (print-for stx)
    (define vs (props-for stx))
    (unless (null? vs)
      (writeln `(PROPS ,vs ,stx))))
  
  (define (f stx)
    (define lst (syntax->list stx))
    (cond
     (lst (print-for stx)
          (for-each f lst))
     (else (print-for stx))))

  (pretty-print (syntax->datum stx))
  (f stx))

