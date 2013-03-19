#lang racket

#|

For more compact printing, we do not make annotations transparent.

|#

(require "strategy.rkt" "util.rkt")
(require (for-syntax racket/syntax))
(require racket/generic)

;;; 
;;; location info
;;; 

(define-struct* loc (source line column position span) #:transparent)

(define* (stx-loc stx)
  (loc
   (syntax-source stx)
   (syntax-line stx)
   (syntax-column stx)
   (syntax-position stx)
   (syntax-span stx)))

;;; 
;;; syntax-derived annotations
;;; 

(define* (stx-annos stx)
  (let ((h (for/hasheq ((k (syntax-property-symbol-keys stx)))
                       (values k (syntax-property stx k)))))
    (set! h (hash-set h 'loc (stx-loc stx)))
    (set! h (hash-set h 'stx stx))
    h))

;;; 
;;; "all" strategies
;;; 

;; Note that ordering is delicate here. Any identifiers referenced at
;; macro expansion time must have been defined by then, but for
;; runtime code forward references to module-level variables are
;; fine.

(define (all-identity f ast) ast)

(define (Module-all f ast)
  (let-and body (map-while f (Module-body ast))
           (struct-copy Module ast (body body))))

(define (Call-all f ast)
  (let-and proc (f (Call-proc ast))
           (struct-copy Call ast (proc proc))))

(define (Define-all f ast)
  (let-and var (f (Define-var ast))
           body (map-while f (Define-body ast))
           (struct-copy Define ast (var var) (body body))))

;;; 
;;; abstract node
;;; 

(define-struct* Ast (annos))

(define-generics strategic
  (for-each-subterm s strategic))

(define-for-syntax (make-for-each-subterm nn-stx f-stx-lst)
  (define nn-sym (syntax-e nn-stx))
  #`(define (for-each-subterm s ast)
      #,@(map
          (lambda (f-stx)
            (syntax-case f-stx (no-term just-term list-of-term)
              ((no-term fn-pat)
               (void))
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

(define-syntax (define-ast* stx)
  (syntax-case stx ()
    ((_ name ((t field) ...) all-op)
     #`(begin
         (define-struct* name Ast (field ...)
           #:property prop:subterm-all all-op
           #:methods gen:strategic (#,(make-for-each-subterm
                                       #'name
                                       (syntax->list #'((t field) ...))))
           #:transparent)
         (define* #,(format-id stx "new-~a" (syntax-e #'name))
           (lambda (stx . args)
             (apply name (stx-annos stx) args)))))))

;;; 
;;; concrete nodes
;;; 

(define-ast* Var ((no-term name)) all-identity)
(define-ast* Module ((list-of-term body)) Module-all)
(define-ast* Pass () all-identity)
(define-ast* Call ((just-term proc)) Call-all)
(define-ast* Define ((just-term var) (no-term kind)
                     (list-of-term body)) Define-all)

(define* (Var-from-stx id-stx)
  (new-Var id-stx (syntax-e id-stx)))

(define* (Var-rename ast n)
  (struct-copy Var ast (name n)))

(begin
  (for-each-subterm writeln (Var #f 'x))
  (for-each-subterm writeln (Call #f (Var #f 'p)))
  (for-each-subterm writeln (Define #f (Var #f 'a) 4
                              (list (Var #f 'b)
                                    (Pass #f)
                                    (Var #f 'c)
                                    (Call #f (Var #f 'p))))))
