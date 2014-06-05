#lang racket/base

#|

Assumptions for AST node types:

- each type must be defined as #:transparent
  (this is done by the macros here)

- the first field of each node is for annotations,
  and named 'annos', and declared as 'no-term'

- no inheritance is used in defining the types

|#

(require "ast-serialize.rkt" "ast-view.rkt" "strategy.rkt"
         "util.rkt" "util/struct.rkt"
         racket/generic unstable/struct
         (for-syntax racket/base racket/function racket/list
                     ;;racket/pretty 
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
(define-for-syntax (make-ast-write n-stx fld-id-lst)
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

;; Note that ordering is delicate here. Any identifiers must be
;; defined before their values are accessed, regardless of phase
;; level. Forward references to module-level variables (without
;; access) are fine.

(define-for-syntax (make-all-visit-term nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  #`(define (all-visit-term s ast)
      #,@(map
          (lambda (f-stx)
            (syntax-case f-stx (no-term just-term list-of-term)
              ((no-term fn-pat)
               #'(begin))
              ((just-term fn-pat)
               (let* ((fn-sym (syntax-e #'fn-pat))
                      (get-stx (format-id nn-stx "~a-~a"
                                          nn-sym fn-sym)))
                 #`(s (#,get-stx ast))))
              ((list-of-term fn-pat)
               (let* ((fn-sym (syntax-e #'fn-pat))
                      (get-stx (format-id nn-stx "~a-~a"
                                          nn-sym fn-sym)))
                 #`(for-each s (#,get-stx ast))))))
          f-stx-lst)
      (void)))

(define-for-syntax (get-relevant-fields f-stx-lst)
  (filter
   identity
   (map
    (lambda (f-stx)
      (syntax-case f-stx (no-term just-term list-of-term)
        ((no-term fn-pat)
         #f)
        ((just-term fn-pat)
         (list 'just #'fn-pat (generate-temporary (syntax-e #'fn-pat))))
        ((list-of-term fn-pat)
         (list 'list #'fn-pat (generate-temporary (syntax-e #'fn-pat))))))
    f-stx-lst)))

;; E.g. output
;; (define (all-rw-term s ast)
;;   (let-and var (s (Define-var ast))
;;            body (all-rw-list s (Define-body ast))
;;            (struct-copy Define ast (var var) (body body))))
(define-for-syntax (make-all-rw-term nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  (define r-f-lst (get-relevant-fields f-stx-lst))
  #`(define (all-rw-term s ast)
      (let-and
       #,@(apply
           append
           (map
            (lambda (fld)
              (let* ((kind (first fld))
                     (fn-stx (second fld))
                     (tmp-stx (third fld))
                     (fn-sym (syntax-e fn-stx))
                     (get-stx (format-id nn-stx "~a-~a"
                                         nn-sym fn-sym)))
                (list
                 tmp-stx
                 (cond
                  ((eq? kind 'just)
                   #`(s (#,get-stx ast)))
                  ((eq? kind 'list)
                   #`(all-rw-list s (#,get-stx ast)))))))
            r-f-lst))
       (struct-copy
        #,nn-stx ast
        #,@(map
            (lambda (fld)
              (let ((fn-stx (second fld))
                    (tmp-stx (third fld)))
                #`[#,fn-stx #,tmp-stx]))
            r-f-lst)))))

(define-for-syntax (make-some-rw-term nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  (define r-f-lst (get-relevant-fields f-stx-lst))

  (define bind-lst
    (for/list ([fld r-f-lst])
      (define kind (first fld))
      (define fn-stx (second fld))
      (define fn-sym (syntax-e fn-stx))
      (with-syntax ([tmp (third fld)]
                    [get (format-id nn-stx "~a-~a" nn-sym fn-sym)])
        #`[tmp (let* ([v (get ast)]
                      [r #,(case kind
                             [(just) #'(s v)]
                             [(list) #'(some-rw-list s v)])])
              (if r (begin (set! any? #t) r) v))])))
  
  #`(define (some-rw-term s ast)
      (define any? #f)
      (let (#,@bind-lst)
        (and any?
             (struct-copy 
              #,nn-stx ast
              #,@(map
                  (lambda (fld)
                    (let ((fn-stx (second fld))
                          (tmp-stx (third fld)))
                      #`[#,fn-stx #,tmp-stx]))
                  r-f-lst))))))

(define-for-syntax (make-strategic nn-stx f-stx-lst)
  (list (make-all-visit-term nn-stx f-stx-lst)
        (make-all-rw-term nn-stx f-stx-lst)
        (make-some-rw-term nn-stx f-stx-lst)))

;;; 
;;; gen:syntactifiable
;;; 

(define-for-syntax (make-syntactifiable/singleton singleton-id)
  (list
   (with-syntax ((the-name singleton-id))
     #'(define (syntactifiable-mkstx x)
         #'the-name))))

(define-for-syntax (make-syntactifiable conc-id fld-id-lst)
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
(define-for-syntax (make-extra-accessors conc-id fld-id-lst provide?)
  (define conc-name (syntax-e conc-id))
  (with-syntax ([conc conc-id]
                [def (if provide? #'define* #'define)])
    (define copy-impl
      (with-syntax ([(fld ...) fld-id-lst]
                    [c-copy (format-id conc-id "~a-copy" conc-name)])
         #'(def (c-copy obj fld ...)
             (struct-copy conc obj [fld fld] ...))))
    
    (define (make-setter-impl fld-id)
      (define fld-name (syntax-e fld-id))
      (with-syntax ([c-setter-id
                     (format-id conc-id "set-~a-~a" conc-name fld-name)]
                    [fld fld-id])
        #'(def (c-setter-id obj fld)
            (struct-copy conc obj [fld fld]))))
    
    (cons copy-impl (map make-setter-impl fld-id-lst))))

;;; 
;;; equal? implementation
;;; 

;; For singletons, these are unnecessary, as mere eq? comparison will
;; do, and the default implementation provides that.
(define-for-syntax (make-equal+hash n-stx fld-id-lst)
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

;;; 
;;; concrete AST node definition
;;; 

(define-for-syntax (make-define-ast stx provide?)
  (define-syntax-class ft
    (pattern (~or (~datum no-term)
                  (~datum just-term)
                  (~datum list-of-term))))
  (syntax-parse stx
    [(_ name:id (view:id ...) ((t:ft fld:id) ...)
        (~optional (~seq #:singleton (arg:expr ...)))
        (~optional (~seq #:custom-write writer:expr))
        (~optional (~seq #:struct-options (opt ...+))))
     (define singleton? (attribute arg))
     (define singleton-id
       (and singleton? (format-id stx "the-~a" (syntax-e #'name))))
     (define conc-id #'name)
     (define fld-id-lst (syntax->list #'(fld ...)))
     (define struct-def
       (quasisyntax
        (#,(if provide?
               (if singleton? #'singleton-struct* #'concrete-struct*)
               (if singleton? #'singleton-struct #'struct))
         name
         #,@(if singleton?
                (with-syntax ((the-name singleton-id))
                  (list #'(the-name arg ...)))
                null)
         (fld ...)
         #:methods gen:custom-write
         [(define write-proc #,(if (attribute writer)
                                   #'writer
                                   (make-ast-write conc-id fld-id-lst)))]
         #,@(if singleton?
                null
                (list 
                 #'#:methods #'gen:equal+hash
                 (with-syntax ([(m ...) 
                                (make-equal+hash conc-id fld-id-lst)])
                   #'[m ...])))
         #:methods gen:syntactifiable
         (#,@(if singleton?
                 (make-syntactifiable/singleton singleton-id)
                 (make-syntactifiable conc-id fld-id-lst)))
         #:methods gen:strategic (#,@(make-strategic
                                      conc-id
                                      (syntax->list #'((t fld) ...))))
         #,@(apply append
                   (for/list ([view-id (syntax->list #'(view ...))])
                     (generate-view-methods conc-id view-id singleton?)))
         #:transparent
         #,@(if (attribute opt)
                (syntax->list #'(opt ...))
                null))))
     ;;(pretty-print (syntax->datum struct-def))
     (if singleton?
         struct-def
         #`(begin
             #,struct-def
             #,@(make-extra-accessors conc-id fld-id-lst provide?)))]))

(define-syntax* (define-ast stx)
  (make-define-ast stx #f))

(define-syntax* (define-ast* stx)
  (make-define-ast stx #t))

;;; 
;;; testing
;;; 

(module* test #f
  (require racket rackunit)

  (define-view Ast #:fields (annos))

  (define-ast Singleton (Ast) ((no-term annos)) #:singleton (#hasheq()))
  (define-ast Empty (Ast) ((no-term annos)))
  (define-ast Some (Ast) ((no-term annos) (just-term thing)))
  (define-ast Object (Ast) ((no-term annos) (just-term one) 
                            (list-of-term many)))

  (define empty (Empty #hasheq()))
  (define object (Object #hasheq() the-Singleton (list the-Singleton empty)))

  (check-equal? the-Singleton the-Singleton)
  (check-equal? empty empty)
  (check-equal? empty (Empty (hasheq 'x 5)))
  (check-not-equal? (Some #hasheq() empty) (Some #hasheq() the-Singleton))
  (check-true (Ast=? (Empty (hasheq 'x 5)) (Some (hasheq 'x 5) empty)))
  (check-false (Ast=? (Empty (hasheq 'x 5)) (Some (hasheq 'x 7) empty)))
  (check-true (match empty [(Ast (? hash?)) #t] [_ #f]))
  (check-true (match empty [(Ast (? hash? h)) (hash-empty? h)] [_ #f]))

  (define some-Singleton->Empty
    (some
     (lambda (ast)
       (match ast
         [(? Singleton?) empty]
         [else ast]))))
  
  (check-false (some-Singleton->Empty empty))
  (check-not-false (some-Singleton->Empty object))
  
  (for ([dat (list the-Singleton
                   `(,the-Singleton)
                   `(1 ,the-Singleton 3)
                   (Empty #hasheq())
                   object
                   (box object)
                   (hasheq 'empty empty 'singleton the-Singleton)
                   (Empty (hasheq 'stx #'(Empty #hasheq())))
                   (Empty (hasheq 'origin (list #'foo #'bar)))
                   )])
    ;;(writeln `(ORIGINAL VALUE ,dat))
    (define stx (syntactifiable-mkstx dat))
    ;;(writeln stx)
    ;;(writeln `(MARSHALLED SYNTAX ,(syntax->datum stx)))
    (define val (eval-syntax stx))
    ;;(writeln `(UNMARSHALED VALUE ,val))
    (when (Ast? val)
      (define annos (Ast-annos val))
      (unless (hash-empty? annos)
        ;;(writeln `(UNMARSHALED ANNOS ,annos))
        (void))))

  (void))
