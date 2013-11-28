#lang racket

#|
|#

(require "util.rkt" syntax/id-table syntax/stx unstable/error)

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
