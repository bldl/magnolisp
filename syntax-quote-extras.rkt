#lang racket/base

#|
|#

(require "syntax-quote.rkt" "util.rkt"
         (for-template racket/base))

(define-syntax* keep?->
  (syntax-rules ()
    [(_ p? ...)
     (lambda (stx)
       (or (p? stx) ...))]))

(define-syntax* keep->
  (syntax-rules ()
    [(_ f ...)
     (lambda (stx dat)
       (set! dat (f stx dat)) ...
       dat)]))

(define* keep-default?
  (keep?-> keep-position? keep-paren-shape?))

(define* keep-default
  (keep-> keep-position keep-paren-shape))

(define* (make-keep-listed-properties? ps)
  (lambda (stx)
    (ormap (lambda (n)
             (and (syntax-property stx n) #t)) ps)))

(define* (default-val->stx v)
  #`(quote #,v))

(define* (make-keep-listed-properties ps val->stx)
  (lambda (stx dat)
    (for/fold ([dat dat]) ([n ps])
      ;;(writeln `(STX ,stx DAT ,dat N ,n))
      (define v (syntax-property stx n))
      (if v
          #`(syntax-property #,dat '#,n #,(val->stx v))
          dat))))

(define* (syntax-preserve/loc+none stx)
  (syntax-preserve keep-position? keep-position stx))

(define* (syntax-preserve/loc+listed ps stx)
  (define keep-props? (make-keep-listed-properties? ps))
  (define keep-props (make-keep-listed-properties ps default-val->stx))
  (define keep? (keep?-> keep-position? keep-props?))
  (define keep (keep-> keep-position keep-props))
  (syntax-preserve keep? keep stx))  

(module* test #f
  (require racket rackunit)
  
  ;; Removes location info and properties;
  ;; recursive, and may ruin bindings.
  (define (strip-stx stx)
    (define d (syntax->datum stx))
    (datum->syntax stx d #f #f))

  (define (keep-properties stx)
    (syntax-preserve keep-default? keep-default stx))

  (define (keep-noloc+parens stx)
    (define sub-keep? (make-keep-listed-properties? '(paren-shape)))
    (define sub-keep (make-keep-listed-properties '(paren-shape)
                                                  default-val->stx))
    (syntax-preserve (keep?-> keep-syntax? sub-keep?)
                     (keep-> keep-syntax sub-keep) stx))
  
  (define (keep-stripped stx)
    (syntax-preserve keep-default? keep-default (strip-stx stx)))

  (define (keep-loc+parens stx)
    (syntax-preserve/loc+listed '(paren-shape) stx))

  (define (keep-none stx)
    (syntax-preserve keep-syntax? keep-syntax stx))
  
  (define x 1)
  
  (for* ([stx (list #'1
                    #'x
                    #'[cons 1 2]
                    #''(1 2 3)
                    #'car
                    #''sym
                    #'#&car
                    #''[1]
                    #'#(1 2 3)
                    #'#s(Obj 1)
                    #'#hasheqv((a . [cons 1 2]) (b . #s(Obj 1)))
                    #'#hasheq()
                    #'#hash(("foo" . car) ("bar" . cdr))
                    #'#hasheq((x . 1) (y . 2)))]
         [f (list keep-properties
                  keep-noloc+parens
                  keep-stripped
                  syntax-preserve/loc+none
                  keep-loc+parens
                  keep-none
                  )])
    (define f-name (object-name f))
    (define dat (eval-syntax stx))
    (define nq-stx (f stx))
    ;;(writeln `(,f-name : TRANSFORMATION ,(syntax->datum stx) -> ,(syntax->datum nq-stx) : PRESERVED AS ,nq-stx : VALUES ,dat))
    (define n-stx (eval-syntax nq-stx))
    (define n-dat (eval-syntax n-stx))
    ;;(writeln `(,f-name : TRANSFORMATION ,(syntax->datum stx) -> ,(syntax->datum nq-stx) : PRESERVED AS ,nq-stx : VALUES ,dat -> ,n-dat EQUAL? ,(equal? dat n-dat)))
    (check-equal? dat n-dat)
    (when (eq? f-name 'keep-stripped)
      (check-false (syntax-position n-stx)))
    (void)))
