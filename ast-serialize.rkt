#lang racket/base

#|

A generic method 'syntactifiable-mkstx' for AST marshaling as syntax.
It is assumed that each AST node type implements the generic interface
'gen:syntactifiable', and hence also 'syntactifiable-mkstx'. Common
Racket datatypes are also supported by the generic method. This
includes syntax objects, but in a somewhat lossy way. Mutability and
weakness properties of collection types may also not be preserved.

|#

(require racket/generic
         "util.rkt" "util/syntax-quote-main.rkt" "util/syntax-quote-extras.rkt"
         (for-template racket/base))
                     
(define (quotable? x)
  (any-pred-holds
   symbol? number? boolean? string? char? keyword? bytes? regexp?
   x))

(define (hash-maker-id-for x)
  (cond
   [(hash-eq? x) #'make-immutable-hasheq]
   [(hash-eqv? x) #'make-immutable-hasheqv]
   [(hash-equal? x) #'make-immutable-hash]
   [else
    (error
     'hash-maker-id-for
     "expected (or/c hash-eq? hash-eqv? hash-equal?): ~s" x)]))

;; Custom version so that we can avoid #<path:....> representation for
;; paths, for example. Then again, there really isn't any one correct
;; choice for marshaling paths. See "quotable" in the Racket
;; Reference.
(define (keep-position/syntactifiable stx dat)
  (define loc (vector-immutable
               (syntax-source stx)
               (syntax-line stx)
               (syntax-column stx)
               (syntax-position stx)
               (syntax-span stx)))
  #`(datum->syntax (quote-syntax #,(datum->syntax stx 'ctx))
                   #,dat #,(syntactifiable-mkstx loc)))

(define kept-properties '(origin paren-shape))

(define keep?/syntactifiable
  (keep?-> keep-position?
           (make-keep-listed-properties? kept-properties)))
           
(define keep/syntactifiable
  (keep-> keep-position/syntactifiable
          (make-keep-listed-properties kept-properties syntactifiable-mkstx)))

(define (syntax-preserve/syntactifiable stx)
  (syntax-preserve keep?/syntactifiable keep/syntactifiable stx))

(define-generics* syntactifiable
  (syntactifiable-mkstx syntactifiable)
  #:defaults
  (
   [syntax?
    (define (syntactifiable-mkstx x)
      (syntax-preserve/syntactifiable x))]
   [null?
    (define (syntactifiable-mkstx x)
      #'null)]
   [pair?
    (define/generic f syntactifiable-mkstx)
    (define (syntactifiable-mkstx x)
      (if (list? x)
          #`(list #,@(map f x))
          #`(cons #,(f (car x)) #,(f (cdr x)))))]
   [box?
    (define/generic f syntactifiable-mkstx)
    (define (syntactifiable-mkstx x)
      #`(box-immutable #,(f (unbox x))))]
   [vector?
    (define/generic f syntactifiable-mkstx)
    (define (syntactifiable-mkstx x)
      #`(vector-immutable
         #,@(for/list ([v (in-vector x)])
              (f v))))]
   [prefab-struct-key
    (define/generic f syntactifiable-mkstx)
    (define (syntactifiable-mkstx x)
      (define lst (cdr (vector->list (struct->vector x))))
      (define key (prefab-struct-key x))
      #`(make-prefab-struct '#,key #,@(map f lst)))]
   [hash?
    (define/generic f syntactifiable-mkstx)
    (define (syntactifiable-mkstx x)
      (define make (hash-maker-id-for x))
      #`(#,make (list #,@(for/list ([(k v) x])
                           #`(cons #,(f k) #,(f v))))))]
   [path?
    (define (syntactifiable-mkstx x)
      #`(bytes->path #,(path->bytes x)))]
   [quotable?
    (define (syntactifiable-mkstx x)
      #`(quote #,x))]
   ))

(module* test #f
  (require racket rackunit)

  (for ([dat (list #f 1 'x "x" #'x '() '(1 2 3)
                   #&7 #(1 2 3)
                   #s(Obj 1) #s(Obj car)
                   #hasheq() #'#hasheq()
                   #hasheq((foo . 1))
                   #hasheq((foo . car))
                   #hasheqv((1 . 2))
                   #hasheqv((1 . 2) (3 . car))
                   #hash(("foo" . car) ("bar" . cdr))
                   '(#'(1 2 3))
                   (list 'no-check #'x)
                   (list 'no-check "obj" (hasheq 'stx #'(m) 'origin (list #'x #'y)))
                   )])
    (define stx (syntactifiable-mkstx dat))
    ;;(writeln (syntax->datum stx))
    (define val (eval-syntax stx))
    (unless (or (syntax? dat) (and (pair? dat) (eq? (car dat) 'no-check)))
      (check-equal? dat val)))
  
  (void))
