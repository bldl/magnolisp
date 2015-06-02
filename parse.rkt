#lang racket/base

#|

|#

(require "ast-magnolisp.rkt" "app-util.rkt" "util.rkt"
         racket/contract racket/dict racket/function racket/list
         racket/match racket/pretty
         syntax/id-table syntax/kerncase syntax/parse syntax/stx
         (for-template racket/base "core.rkt"))

;;; 
;;; debugging utilities
;;; 

(define (print-stx-with-bindings stx)
  (define lst (syntax->list stx))
  (cond
   (lst (for-each print-stx-with-bindings lst))
   ((identifier? stx) (writeln (list stx (identifier-binding stx 0))))
   (else (writeln stx))))

(define (find-id-stx find-stx stx (msg #f))
  (define (f stx)
    (define lst (syntax->list stx))
    (cond
     (lst
      (for-each f lst))
     ((identifier? stx)
      (when (free-identifier=? find-stx stx)
        (writeln (list (or msg (format "~a" (syntax->datum stx)))
                       stx (identifier-binding stx 0)))))))
  (f stx))

(define (stx-binding-info stx)
  (define p (syntax-e stx))
  (and (pair? p)
       (let ((id (car p)))
         (and (identifier? id)
              (identifier-binding id 0)))))

;;; 
;;; annotation parsing
;;; 

(define (parse-type-expr t-e)
  (let loop ([stx t-e])
    (kernel-syntax-case*/phase stx 0 (#%magnolisp)
      [id
       (identifier? #'id)
       (syntaxed stx NameT #'id)]
      [(if _ (#%plain-app #%magnolisp (quote f) p ... r) _)
       (eq? 'fn (syntax-e #'f))
       (let ()
         (define p-stx-lst (syntax->list #'(p ...)))
         (define p-ast-lst (map loop p-stx-lst))
         (define r-ast (loop #'r))
         (syntaxed stx FunT p-ast-lst r-ast))]
      [(if _ (#%plain-app #%magnolisp (quote q)
                          (let-values (((tp) _) ...)
                            sub-t)) _)
       (or (eq? 'exists (syntax-e #'q))
           (eq? 'for-all (syntax-e #'q)))
       (let ()
         (define for-all? (eq? 'for-all (syntax-e #'q)))
         (define tp-id-lst (syntax->list #'(tp ...)))
         (define tp-ast-lst (for/list ((id tp-id-lst))
                              (syntaxed id NameT id)))
         (define sub-ast (loop #'sub-t))
         (syntaxed stx (if for-all? ForAllT ExistsT)
                   tp-ast-lst sub-ast))]
      [(if _ (#%plain-app #%magnolisp (quote f) t p ...) _)
       (and (eq? 'parameterized (syntax-e #'f))
            (identifier? #'t))
       (let ()
         (define t-ast (loop #'t))
         (define p-stx-lst (syntax->list #'(p ...)))
         (define p-ast-lst (map loop p-stx-lst))
         (syntaxed stx ParamT t-ast p-ast-lst))]
      [(if _ (#%plain-app #%magnolisp (quote f)) _)
       (eq? 'auto (syntax-e #'f))
       (syntaxed stx AnyT)]
      [_
       (raise-language-error
        #f "illegal type expression"
        t-e stx)])))
  
(define (parse-anno-value anno-stx kind dat-stx)
  (case kind
    [(type) 
     (syntaxed anno-stx TypeAnno (parse-type-expr dat-stx))]
    [else 
     ;;(writeln dat-stx)
     (kernel-syntax-case/phase dat-stx 0
       [(quote dat)
        (syntaxed anno-stx GenericAnno kind (syntax->datum #'dat))]
       [(quote-syntax stx)
        (syntaxed anno-stx GenericAnno kind #'stx)]
       [_
        (raise-language-error
         #f "unsupported data in generic annotation"
         anno-stx dat-stx)])]))
    
(define (parse-anno-expr let-stx anno-stx)
  (kernel-syntax-case*/phase anno-stx 0 (#%magnolisp)
    [(if _ (#%plain-app #%magnolisp (quote a) (quote k) dat) _)
     (and (eq? 'anno (syntax-e #'a)) (identifier? #'k))
     (let ((kind (syntax-e #'k)))
       (parse-anno-value anno-stx (syntax-e #'k) #'dat))]
    [_
     (raise-language-error
      #f "illegal syntax in annotation expression context"
      let-stx anno-stx)]))

;;; 
;;; parsing
;;; 

;; Returns top-level defs in module.
(define-with-contract*
  (-> syntax? immutable-id-table?)
  (parse-defs-from-module modbeg-stx)

  (define defs-in-mod (make-immutable-free-id-table))

  (define (get-def-in-mod id)
    (dict-ref defs-in-mod id #f))

  (define (set-def-in-mod! id def)
    (set! defs-in-mod (dict-set defs-in-mod id def)))
  
  (define (redefinition id old-def new-stx)
    (error 'parse-defs-from-module
           "redefinition of ~a: ~a and ~a"
           id (ast-anno-must old-def 'stx) new-stx))

  (define (check-redefinition id new-stx)
    (when-let old-def (get-def-in-mod id)
              (redefinition id old-def new-stx)))

  ;; Returns initial annotations for definition 'stx' (binding
  ;; 'id-stx'). Returns AST node annotations as a (hash/c symbol?
  ;; any/c).
  (define (mk-annos stx id-stx)
    (hasheq 'stx stx))

  (define (make-DefVar stx id-stx e-stx #:top? [top? #f])
    (check-redefinition id-stx stx)
    (define ast (parse-expr e-stx))
    (define ann-h (mk-annos stx id-stx))
    (define def (DefVar ann-h id-stx the-AnyT ast))
    (when top?
      (set-def-in-mod! id-stx def))
    def)

  (define (make-Param stx id-stx)
    (check-redefinition id-stx stx)
    (define ann-h (mk-annos stx id-stx))
    (define def (Param ann-h id-stx the-AnyT))
    def)
  
  (define (make-Literal stx datum-stx)
    (syntaxed stx Literal (syntax->datum datum-stx)))
  
  ;; Parses all the different kinds of Let expressions (`let`,
  ;; `letrec`, etc.). Any number of definition clauses, any number of
  ;; variables each.
  (define (parse-let-expr stx binds-stx exprs-stx)
    (define kind (syntax-e (car (syntax-e stx))))
    (define body-stx-lst (syntax->list exprs-stx))
    (define body-ast-lst (map parse-expr body-stx-lst))
    (define bind-stx-lst (syntax->list binds-stx))

    (define (wrap-in-let dv ast-lst)
      (define as (hasheq 'stx stx 'let-kind kind))
      (list (LetExpr as dv ast-lst)))

    (define (wrap-in-let* dv-lst ast-lst)
      (for/fold ([ast-lst ast-lst]) ([dv (reverse dv-lst)])
        (wrap-in-let dv ast-lst)))
    
    (define ast-lst
      (for/fold ([ast-lst body-ast-lst]) ([i-e (reverse bind-stx-lst)])
        ;;(pretty-print `(BINDING ,(syntax->datum i-e)))
        (kernel-syntax-case*/phase i-e 0 (values)
          [((id ...) (#%plain-app values v ...))
           (let ()
             (define id-lst (syntax->list #'(id ...)))
             (assert (andmap identifier? id-lst))
             (define v-lst (syntax->list #'(v ...)))
             (unless (= (length v-lst) (length id-lst))
               (raise-language-error
                #f
                (format "expected ~a values" (length id-lst))
                stx))
             (define dv-lst
               (map
                (lambda (id-stx v-stx)
                  (make-DefVar i-e id-stx v-stx))
                id-lst v-lst))
             (wrap-in-let* dv-lst ast-lst))]
          
          [(() stat)
           (cons (parse-expr #'stat) ast-lst)]
          
          [((id) v)
           (identifier? #'id)
           (let ()
             (define dv (make-DefVar i-e #'id #'v))
             (wrap-in-let dv ast-lst))]
          
          [_
           (raise-language-error
            #f 
            (format "illegal syntax for a `~a` binding form" kind)
            stx i-e
            #:fields `(("binding"
                        ,(or (stx-binding-info i-e) 'unbound))))])))
    
    (if (= (length ast-lst) 1)
        (first ast-lst)
        (syntaxed stx SeqExpr ast-lst)))
    
  (define (parse-module-begin stx)
    (kernel-syntax-case/phase stx 0
      ((#%module-begin . bs)
       (for-each
        parse-module-level
        (syntax->list #'bs)))

      (_ (raise-language-error
          #f "illegal syntax for #%module-begin" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  (define (for-non-cxx-target stx)
    (define v (syntax-property stx 'for-target))
    (and v (not (eq? v 'cxx))))
  
  (define (parse-module-level stx)
    (kernel-syntax-case*/phase stx 0 (#%magnolisp values)
      (_
       (for-non-cxx-target stx)
       (void))

      ((begin-for-syntax . _)
       (void))

      ;; This form binds nothing in Racket, but for C++ it does have
      ;; the effect of binding something, in the same way as the form
      ;; (define-values (id) e) would.
      ((define-values ()
         (begin
           (if _ (#%plain-app #%magnolisp (quote d) id e) _)
           (#%plain-app values)))
       (and (eq? 'declare (syntax-e #'d)) (identifier? #'id))
       (make-DefVar stx #'id #'e #:top? #t))
      
      ;; We support (values v ...) in an expression position only
      ;; directly in binding form value expressions.
      ((define-values (id ...) (#%plain-app values v ...))
       (let ()
         (define id-lst (syntax->list #'(id ...)))
         (assert (andmap identifier? id-lst))
         (define v-lst (syntax->list #'(v ...)))
         (unless (= (length v-lst) (length id-lst))
           (raise-language-error
            #f
            (format "expected ~a values" (length id-lst))
            stx (third (syntax->list stx))))
         (for-each
          (lambda (id-stx e-stx)
            (make-DefVar stx id-stx e-stx #:top? #t))
          id-lst v-lst)))

      ;; Must come after the ((define-values (id ...) (#%plain-app
      ;; values v ...)) pattern.
      ((define-values (id) e)
       (identifier? #'id)
       (make-DefVar stx #'id #'e #:top? #t))
      
      ((define-syntaxes . _)
       (void))

      ((#%provide . _)
       (void))
      
      ((#%require . _)
       (void))
      
      ((module . _)
       (void))

      ((module* . _)
       (void))

      ;; Any module-level Racket expression is ignored. (These are as
      ;; for the Racket 'expr' non-terminal, and mostly in the same
      ;; order, too.) Note that we should only have (begin expr ...+)
      ;; forms, and not anything containing non-expressions, since
      ;; those should have gotten spliced into the module body.
      
      ((#%expression e)
       (void))
      
      (id
       (identifier? #'id)
       (void))

      ((#%plain-lambda . _)
       (void))

      ((case-lambda . _)
       (void))

      ((if c t e)
       (void))

      ((begin . _)
       (void))

      ((begin0 . _)
       (void))
      
      ((let-values . _)
       (void))
      
      ((letrec-values . _)
       (void))
      
      ;; Not Racket core language, but may appear here.
      ((letrec-syntaxes+values . _)
       (void))
      
      ((set! id expr)
       (identifier? #'id)
       (void))
      
      ((quote _)
       (void))
      
      ((quote-syntax _)
       (void))

      ((with-continuation-mark . _)
       (void))

      ((#%plain-app . _)
       (void))

      ((#%top . id) ;; should not appear in a module body
       (identifier? #'id)
       (void))

      ((#%variable-reference id)
       (identifier? #'id)
       (void))

      ((#%variable-reference (#%top . id))
       (identifier? #'id)
       (void))

      ((#%variable-reference)
       (void))

      (_
       (raise-language-error
        #f "illegal syntax at module level" stx
        #:fields `(("binding"
                    ,(or (stx-binding-info stx) 'unbound)))))))
    
  (define (parse-expr stx)
    (kernel-syntax-case*/phase stx 0 (call/ec values void #%magnolisp)

      (_
       (for-non-cxx-target stx)
       (syntaxed stx RacketExpr))
      
      ((begin . bs)
       (syntaxed stx SeqExpr
                 (map parse-expr
                      (syntax->list #'bs))))
      
      ((begin0 e . bs)
       (let ()
         (define e-ast (parse-expr #'e))
         (define b-ast-lst (map parse-expr (syntax->list #'bs)))
         (cond
          ((null? b-ast-lst)
           e-ast)
          (else
           (syntaxed stx Begin0 (cons e-ast b-ast-lst))))))
      
      ;; These do appear as well.
      ((#%expression e)
       (parse-expr #'e))

      ((#%plain-lambda (par ...) . exprs)
       (let ()
         (define par-id-lst (syntax->list #'(par ...)))
         (define e-stx-lst (syntax->list #'exprs))
         (define par-ast-lst
           (map (lambda (id)
                  ;; Annotations would probably have to be propagated
                  ;; from any binding whose value this lambda is, but
                  ;; that must wait until later.
                  (make-Param stx id))
                par-id-lst))
         (define e-ast-lst (map parse-expr e-stx-lst))
         (define e-ast
           (case (length e-ast-lst)
             ((0) (assert #f))
             ((1) (first e-ast-lst))
             (else (annoless SeqExpr e-ast-lst))))
         (syntaxed stx Lambda par-ast-lst e-ast)))

      ((if _ t e)
       (syntax-property stx 'if-target)
       (let ()
         (define name (syntax-property stx 'if-target))
         (define ast
           (if (eq? name 'cxx)
               (parse-expr #'t)
               (parse-expr #'e)))
         ast))
      
      ((if _ (#%plain-app #%magnolisp (quote k)) _)
       (eq? 'foreign-type (syntax-e #'k))
       (syntaxed stx ForeignTypeExpr))
      
      ((if c t e)
       (syntaxed stx IfExpr
                 (parse-expr #'c)
                 (parse-expr #'t)
                 (parse-expr #'e)))

      ((#%plain-app void . exprs)
       (let ()
         (define e-lst (map parse-expr (syntax->list #'exprs)))
         (if (null? e-lst)
             (syntaxed stx VoidStat)
             (syntaxed stx SeqExpr 
                       (append e-lst (list (annoless VoidStat)))))))
      
      ((#%plain-app values e)
       (parse-expr #'e))

      ((#%plain-app values)
       (syntaxed stx SeqExpr '()))

      ((#%plain-app call/ec
        (#%plain-lambda (k) . bs))
       (syntax-property stx 'local-ec)
       (syntaxed stx LetLocalEc
                 (Var (hasheq 'stx #'k 'type the-KontT) #'k) 
                 (map parse-expr (syntax->list #'bs))))
      
      ((#%plain-app k e)
       (and (syntax-property stx 'local-ec) (identifier? #'k))
       (syntaxed stx AppLocalEc 
                 (Var (hasheq 'stx #'k 'type the-KontT) #'k) 
                 (parse-expr #'e)))
      
      ((#%plain-app p-expr . a-expr)
       (let ()
         (define f-ast (parse-expr #'p-expr))
         (unless (Var? f-ast)
           ;; No first-class functions in Magnolisp.
           (raise-syntax-error #f "expected identifier"
                               stx #'p-expr))
         (ApplyExpr
          (hasheq 'stx stx 'uid (gensym 'apply))
          f-ast
          (map
           parse-expr
           (syntax->list #'a-expr)))))
      
      ((let-values ([() (begin a (#%plain-app values))] ...) e)
       (syntax-property stx 'annotate)
       (let ()
         (define a-stx-lst (syntax->list #'(a ...)))
         (define a-ast-lst (for/list ((a-stx a-stx-lst))
                             (parse-anno-expr stx a-stx)))
         (define e-ast (parse-expr #'e))
         (syntaxed stx AnnoExpr a-ast-lst e-ast)))

      ((let-values binds . exprs)
       (parse-let-expr stx #'binds #'exprs))
      
      ((letrec-values binds . exprs)
       (parse-let-expr stx #'binds #'exprs))
      
      ((letrec-syntaxes+values _ binds . exprs)
       (parse-let-expr stx #'binds #'exprs))

      ((set! id expr)
       (identifier? #'id)
       (syntaxed stx AssignStat (parse-expr #'id) (parse-expr #'expr)))

      ((quote lit)
       (make-Literal stx #'lit))
      
      ((#%top . id) ;; module-level variable (can prevent shadowing)
       (identifier? #'id)
       (syntaxed stx Var #'id))
      
      (id
       (identifier? #'id)
       (syntaxed stx Var #'id))
      
      (_ (raise-language-error
          #f "illegal syntax in expression context" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  ;;(print-with-select-syntax-properties '(in-racket local-ec) modbeg-stx)
  (parse-module-begin modbeg-stx)
  defs-in-mod)
