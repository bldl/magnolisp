#lang racket/base

#|

|#

(require "annos-util.rkt" "ast-magnolisp.rkt" "app-util.rkt"
         "util.rkt" "util/case.rkt"
         racket/contract racket/dict racket/function racket/list
         racket/match racket/pretty
         syntax/id-table syntax/kerncase syntax/parse syntax/stx
         (for-template racket/base
                       (only-in "surface.rkt" #%magnolisp)))

;;; 
;;; debugging utilities
;;; 

(define (print-stx-with-bindings stx)
  (define lst (syntax->list stx))
  (cond
   (lst (for-each print-stx-with-bindings lst))
   ((identifier? stx) (writeln (list stx (identifier-binding stx 0))))
   (else (writeln stx))))

(define (stx-print-if-type-annoed stx)
  (define annos (syntax-get-annos stx))
  (when annos
    (define type (hash-ref annos 'type #f))
    (when type
      (writeln (list 'TYPED stx type)))))

(define (stx-print-all-type-annoed stx)
  (stx-print-if-type-annoed stx)
  (define lst (syntax->list stx))
  (when lst
   (for-each stx-print-all-type-annoed lst)))

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
;;; Magnolisp type parsing
;;; 

(define (parse-type anno-stx)
  (define (parse-name name-stx)
    (syntax-parse name-stx
      #:context anno-stx
      (name:id
       (syntaxed name-stx NameT #'name))))
  
  (syntax-parse anno-stx
    ((_ ((~datum fn) p-type ... r-type))
     (syntaxed (cdr (syntax-e anno-stx))
               FunT (map parse-name (syntax->list #'(p-type ...)))
               (parse-name #'r-type)))
    ((_ name:id)
     (parse-name #'name))))

;;; 
;;; parsing
;;; 

;; Returns top-level defs in module.
(define-with-contract*
  (-> syntax? immutable-id-table? immutable-id-table?)
  (parse-defs-from-module modbeg-stx annos)

  (define defs-in-mod (make-immutable-free-id-table #:phase 0))

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

  ;; Looks up annotations (from annotation table) for declaration
  ;; 'stx' (binding 'id-stx'). Returns AST node annotations as a
  ;; (hash/c symbol? any/c).
  (define (mk-annos stx id-stx)
    (define ann-h (dict-ref annos id-stx #hasheq()))
    ;;(writeln `(raw annos for ,(syntax-e id-stx) are ,@(apply append (for/list (((k v) ann-h)) `(,k = ,v)))))
    (set! ann-h (hash-set ann-h 'stx stx))
    (set! ann-h (hash-remove ann-h 'type))
    ;;(writeln `(parsed annos for ,(syntax-e id-stx) are ,@(apply append (for/list (((k v) ann-h)) `(,k = ,v)))))
    ann-h)

  (define (lookup-type id-stx)
    (define (f)
      (define anno-h (dict-ref annos id-stx #f))
      (and anno-h
           (let ((type-stx (hash-ref anno-h 'type #f)))
             (and type-stx
                  (parse-type type-stx)))))
    (or (f) the-AnyT))

  (define (make-DefVar stx id-stx e-stx #:top? [top? #f])
    (check-redefinition id-stx stx)
    (define ast (parse-expr e-stx))
    (define ann-h (mk-annos stx id-stx))
    (define t (lookup-type id-stx))
    (define def (DefVar ann-h id-stx t ast))
    (when top?
      (set-def-in-mod! id-stx def))
    def)

  (define (make-ForeignTypeDecl stx id-stx)
    (check-redefinition id-stx stx)
    (define ann-h (mk-annos stx id-stx))
    (define def (ForeignTypeDecl ann-h id-stx the-Unresolved))
    (set-def-in-mod! id-stx def)
    def)
  
  (define (make-Param stx id-stx)
    (check-redefinition id-stx stx)
    (define ann-h (mk-annos stx id-stx))
    (define def (Param ann-h id-stx the-AnyT))
    def)
  
  (define (make-Literal stx datum-stx)
    (syntaxed stx Literal datum-stx))
  
  (define (make-LetStat stx kind-stx binds-stx exprs-stx)
    (define kind (syntax-e kind-stx))
    (define e-stx-lst (syntax->list exprs-stx))
    (define e-ast-lst (map parse-stat e-stx-lst))
    (define i-e-lst (syntax->list binds-stx))

    (define (wrap-in-let dv ast-lst)
      (define as (hasheq 'stx stx 'let-kind kind))
      (list (LetStat as dv ast-lst)))

    (define (wrap-in-let* dv-lst ast-lst)
      (for/fold ([ast-lst ast-lst]) ([dv (reverse dv-lst)])
        (wrap-in-let dv ast-lst)))
    
    (define ast-lst
      (for/fold ([ast-lst e-ast-lst]) ([i-e (reverse i-e-lst)])
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
           (cons (parse-stat #'stat) ast-lst)]
          
          [((id) v)
           (identifier? #'id)
           (let ()
             (define dv (make-DefVar i-e #'id #'v))
             (wrap-in-let dv ast-lst))]
          
          [_
           (raise-language-error
            #f "illegal syntax for a let binding form"
            stx i-e
            #:fields `(("binding"
                        ,(or (stx-binding-info i-e) 'unbound))))])))
    
    (if (= (length ast-lst) 1)
        (first ast-lst)
        (syntaxed stx BlockStat ast-lst)))
    
  (define (parse-define-value ctx stx id-stx e-stx)
    ;;(writeln (list 'parse-define-value e-stx (syntax->datum e-stx)))
    ;;(writeln (identifier-binding #'#%magnolisp 0))
    (kernel-syntax-case*/phase e-stx 0 (#%magnolisp)
      ((#%plain-app #%magnolisp (quote k))
       (eq? 'foreign-type (syntax-e #'k))
       (make-ForeignTypeDecl stx id-stx))
      
      (_
       (make-DefVar stx id-stx e-stx #:top? #t))))

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

  (define (parse-module-level stx)
    (kernel-syntax-case*/phase stx 0 (values)
      (_
       (syntax-property stx 'in-racket)
       (void))

      ((begin-for-syntax . _)
       (void))

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
            (parse-define-value 'module-level stx id-stx e-stx))
          id-lst v-lst)))

      ;; Must come after the ((define-values (id ...) (#%plain-app
      ;; values v ...)) pattern.
      ((define-values (id) e)
       (identifier? #'id)
       (parse-define-value 'module-level stx #'id #'e))

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
  
  (define (parse-stat stx)
    (kernel-syntax-case*/phase stx 0 (values)
      (_
       (syntax-property stx 'in-racket)
       (syntaxed stx BlockStat null))
      
      ((begin . bs)
       (syntaxed stx BlockStat
                 (map parse-stat
                      (syntax->list #'bs))))
      
      ((#%expression e)
       (parse-stat #'e))
      
      ((if c t e)
       (syntaxed stx IfStat
                 (parse-expr #'c)
                 (parse-stat #'t)
                 (parse-stat #'e)))

      ((#%plain-app f . e)
       (and (identifier? #'f)
            (or (free-identifier=? #'f #'void)
                (free-identifier=? #'f #'values)))
       (begin
         (unless (null? (syntax-e #'e))
           (raise-syntax-error
            #f
            (format "arguments not allowed for ~a in a statement position"
                    (syntax-e #'f))
            stx #'e))
         (syntaxed stx BlockStat null)))

      ((#%plain-app k e)
       (and (syntax-property stx 'local-ec) (identifier? #'k))
       (syntaxed stx AppLocalEc (self-syntaxed Label #'k) (parse-expr #'e)))
      
      ((let-values binds . exprs)
       (make-LetStat stx (car (syntax-e stx)) #'binds #'exprs))

      ((letrec-values binds . exprs)
       (make-LetStat stx (car (syntax-e stx)) #'binds #'exprs))

      ((letrec-syntaxes+values _ binds . exprs)
       (make-LetStat stx (car (syntax-e stx)) #'binds #'exprs))

      ((set! id expr)
       (identifier? #'id)
       (syntaxed stx Assign (parse-expr #'id) (parse-expr #'expr)))

      (_ (raise-language-error
          #f "illegal syntax in statement context" stx
          #:fields `(("binding"
                      ,(or (stx-binding-info stx) 'unbound)))))))

  ;; All the arguments are syntax objects.
  (define (parse-let-expr stx kind binds e)
    (kernel-syntax-case/phase binds 0
     (()
      (parse-expr e))
     (([(n) v])
      (identifier? #'n)
      (let ()
        (define d-ast (make-DefVar stx #'n #'v))
        (define e-ast (parse-expr e))
        (define as (hasheq 'stx stx 'let-kind (syntax-e kind)))
        (LetExpr as d-ast e-ast)))
     (_
      (raise-language-error
       #f "illegal let expression" stx))))
  
  ;; Inserts bindings into 'defs-in-mod' as a side effect. Returns an
  ;; Ast object for non top-level things.
  (define (parse-expr stx)
    ;;(stx-print-if-type-annoed stx)
    ;;(writeln (list ctx stx))
    
    (kernel-syntax-case*/phase stx 0 (call/ec)

      (_
       (syntax-property stx 'in-racket)
       (syntaxed stx RacketExpr))
      
      ;; These do appear as well.
      ((#%expression e)
       (parse-expr #'e))

      ((#%plain-lambda formals . exprs)
       (let ()
         (define par-id-lst (syntax->list #'formals))
         (define e-stx-lst (syntax->list #'exprs))
         (when (> (length e-stx-lst) 1)
           (raise-syntax-error
            #f "function body must be a single expression" stx))
         (define par-ast-lst
           (map (lambda (id)
                  ;; Annotations would probably have to be propagated
                  ;; from any binding whose value this lambda is, but
                  ;; that must wait until later.
                  (make-Param stx id))
                par-id-lst))
         (define e-stx (first e-stx-lst))
         (define e-ast (parse-expr e-stx))
         (syntaxed stx Lambda par-ast-lst e-ast)))
      
      ((if c t e)
       (syntaxed stx IfExpr
                 (parse-expr #'c)
                 (parse-expr #'t)
                 (parse-expr #'e)))

      ((#%plain-app call/ec
        (#%plain-lambda (k) b ...))
       (syntax-property stx 'local-ec)
       (syntaxed stx
        LetLocalEc (self-syntaxed Label #'k)
        (map
         parse-stat
         (syntax->list #'(b ...)))))
      
      ((#%plain-app p-expr . a-expr)
       (let ()
         (define f-ast (parse-expr #'p-expr))
         (unless (Var? f-ast)
           ;; No first-class functions in Magnolisp.
           (raise-syntax-error #f "expected identifier"
                               stx #'p-expr))
         (syntaxed stx Apply
          f-ast
          (map
           parse-expr
           (syntax->list #'a-expr)))))

      ((let-values binds e)
       (parse-let-expr stx (car (syntax-e stx)) #'binds #'e))
      
      ((letrec-values binds e)
       (parse-let-expr stx (car (syntax-e stx)) #'binds #'e))
      
      ((letrec-syntaxes+values _ binds e)
       (parse-let-expr stx (car (syntax-e stx)) #'binds #'e))

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
