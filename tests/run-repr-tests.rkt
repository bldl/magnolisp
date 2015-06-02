#lang racket

#|
|#

(module* test #f
  (require magnolisp/ast-repr magnolisp/ast-serialize magnolisp/ast-view
           magnolisp/strategy magnolisp/strategy-term
           rackunit)

  (define-view Ast (#:fields annos))

  (define-ast Singleton (Ast) ((#:none annos)) #:singleton (#hasheq()))
  (define-ast Empty (Ast) ([#:none annos]))
  (define-ast Some (Ast) ((#:none annos) (#:just thing)))
  (define-ast Object (Ast) ((#:none annos) (#:just one) 
                            (#:many many)))

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

  (check-equal? (term-fields the-Singleton) '())
  (check-equal? (term-fields empty) '())
  (check-eqv? 1 (length (term-fields (Some (hasheq 'x 5) empty))))
  (check-eqv? 2 (length (term-fields object)))
  
  (define rw-Singleton->Empty
     (lambda (ast)
       (match ast
         [(? Singleton?) empty]
         [else #f])))
  
  (define some-Singleton->Empty
    (some rw-Singleton->Empty))
  
  (define one-Singleton->Empty
    (one rw-Singleton->Empty))

  (define (count-Singleton ast)
    (define c 0)
    ((topdown-visit (lambda (ast)
                      (when (Singleton? ast)
                        (set! c (add1 c))))) 
     ast)
    c)
  
  (check-false (some-Singleton->Empty empty))
  (check-not-false (some-Singleton->Empty object))
  (check-false (one-Singleton->Empty empty))
  (check-not-false (one-Singleton->Empty object))

  (let ((ast (one-Singleton->Empty 
              (Object #hasheq() 
                      empty 
                      (list empty the-Singleton the-Singleton)))))
    (check-eqv? 1 (count-Singleton ast)))
  
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
  
  (define-ast Tree () ([#:many lst]))
  (define-ast Atom () ([#:none v]))
  (let ((t (Tree (list (Atom 1) (Atom 2))))
        (x 0))
    (define (s ast) (when (Atom? ast) (set! x (+ x (Atom-v ast)))))
    (define (br ast) (break))
    ((topdown-visit s) t)
    (check-eqv? x 3)
    (set! t (Tree (list (Tree (list (Atom 1) (Atom 2))) (Atom 3))))
    (define (s2 ast) (when (Atom? ast) (set! x (+ x (Atom-v ast))) (break)))
    ((topdown-visit-break s2) t)
    (check-eqv? x 9)
    (define (s3 ast) (cond ((Atom? ast) 
                            (set! x (+ x (Atom-v ast))))
                           ((Tree? ast)
                            (break))))
    ((topdown-visit-break s3) t)
    (check-eqv? x 9))
  (let ()
    (define (sum t)
      (define x 0)
      (define (f ast)
        (when (Atom? ast)
          (set! x (+ x (Atom-v ast)))
          (break)))
      ((topdown-visit-break f) t)
      x)
    (define t (Tree (list (Tree (list (Atom 1) (Atom 2))) (Atom 3))))
    (check-eqv? (sum t) 6)
    (define (inc ast)
      (match ast
        ((Atom x) (break (Atom (add1 x))))
        (_ ast)))
    (set! t ((topdown-break inc) t))
    (check-eqv? (sum t) 9))
  (let ()
    (define (collect-nums t)
      (define lst '())
      ((bottomup-visit
        (lambda (ast)
          (when (Atom? ast)
            (set! lst (cons (Atom-v ast) lst))))) t)
      (reverse lst))
    (define t (Tree (list (Tree (list (Atom 1) (Atom 2))) (Atom 3))))
    ;; note: not in reverse order as probably are in Stratego
    (check-equal? '(1 2 3) (collect-nums t)))

  (let ((t (Tree (list (Tree (list (Atom 1) (Atom 2))) (Atom 3)))))
    (let ((lst (term-fields t)))
      (check-equal? t (set-term-fields t lst))
      (check-equal? t (set-term-fields (Tree null) lst))))
  (let ((t (Tree (list (Atom 1) (Atom 2)))))
    (define (inc ast)
      (match ast
        ((Atom x) (Atom (add1 x)))
        (_ ast)))
    (define lst (term-fields t))
    (define inc-lst (for/list ((f lst))
                      (map inc f)))
    (check-equal? (set-term-fields t inc-lst)
                  (Tree (list (Atom 2) (Atom 3)))))
  
  (let ((t1 (Tree (list (Atom 1) (Atom 2))))
        (t2 (Tree (list (Atom 1) (Tree null))))
        (t3 (Tree (list (Tree null) (Tree null)))))
    (define (rw-Tree ast)
      (and (Tree? ast) (Atom 555)))
    (define (rw-Atom ast)
      (and (Atom? ast) (Tree null)))
    (check-not-false ((all rw-Atom) t1))
    (check-false ((all rw-Atom) t2))
    (check-false ((all rw-Atom) t3))
    (check-false ((all rw-Tree) t1))
    (check-false ((all rw-Tree) t2))
    (check-not-false ((all rw-Tree) t3))
    (check-not-false ((some rw-Atom) t1))
    (check-not-false ((some rw-Atom) t2))
    (check-false ((some rw-Atom) t3))
    (check-false ((some rw-Tree) t1))
    (check-not-false ((some rw-Tree) t2))
    (check-not-false ((some rw-Tree) t3))
    (check-not-false ((one rw-Atom) t1))
    (check-not-false ((one rw-Atom) t2))
    (check-false ((one rw-Atom) t3))
    (check-false ((one rw-Tree) t1))
    (check-not-false ((one rw-Tree) t2))
    (check-not-false ((one rw-Tree) t3))
    (check-not-false ((one rw-Atom) ((one rw-Atom) t1)))
    (check-false ((one rw-Atom) ((one rw-Atom) t2)))
    (check-false ((some rw-Atom) ((some rw-Atom) t1)))
    (void))

  (void))
