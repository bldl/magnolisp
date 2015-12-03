#lang racket/base

#|

C++ back end.

|#

(require racket/contract/base
         racket/dict
         racket/function
         racket/list
         racket/match
         racket/pretty
         racket/set
         "app-util.rkt"
         "ir-id-coll.rkt"
         "ir-ast.rkt"
         "backend-cxx-ast.rkt"
         "backend-cxx-print.rkt"
         "backend-util.rkt"
         "ir-transform.rkt"
         "strategy-stratego.rkt"
         "strategy-term.rkt"
         "strategy.rkt"
         "util.rkt" "util/debug.rkt"
         "util/field.rkt" "util/system.rkt")

;;; 
;;; reformatting
;;; 

;; Not quite perfect as does not appear to insert line breaks.
;; http://astyle.sourceforge.net/
;; (astyle "void main() { return; }")
(define-with-contract*
  (-> string? string?) (astyle s)
  (exe-filter s '("/usr/bin/astyle" "--mode=c"
                  "--lineend=linux" "--style=otbs"
                  "--quiet")))

;; This one does line breaking.
;; Needs a config file, so try
;; uncrustify --update-config > ~/.uncrustify.cfg
;; http://uncrustify.sourceforge.net/
;; (uncrustify "void main() { return; }")
(define-with-contract*
  (-> string? string?) (uncrustify s)
  (exe-filter s '("/usr/bin/uncrustify" "-l" "cpp" "-q")))

;; This one is one of the tools in Clang. Does line breaking.
;; http://clang.llvm.org/docs/ClangFormat.html
;; Can be customized by creating a configuration file.
;; To get started with creating a configuration, can do
;; clang-format -dump-config > ~/.clang-format
;; (clang-format "void main() { int x = 1; return; }")
(define-with-contract*
  (-> string? string?) (clang-format s)
  (exe-filter s (list (find-executable-path "clang-format"))))

;;; 
;;; C++ identifiers
;;; 

;; does not support qualified names or operator names
(define (string-cxx-id? s)
  (not (or (regexp-match? #rx"^[^a-zA-Z_]" s)
           (regexp-match? #rx"[^a-zA-Z0-9_]" s)
           (= (string-length s) 0))))

(define (translate-id-string s)
  (when-let r (regexp-match #rx"^(.*)[?]$" s)
    (set! s (string-append "is_" (second r))))
  (when-let r (regexp-match #rx"^(.*)[=]$" s)
    (set! s (string-append (second r) "_equal")))
  (set! s (regexp-replace* #rx"^->" s "to_"))
  (set! s (regexp-replace* #rx"->" s "_to_"))
  s)  

(define (string->maybe-cxx-id s)
  (set! s (translate-id-string s))
  (set! s (regexp-replace #rx"[!?=]+$" s ""))
  (set! s (string-underscorify s))
  (and (string-cxx-id? s) s))

(define (string->exported-cxx-id o-s)
  (define s (string->maybe-cxx-id o-s))
  (unless s
    (error
     'string->exported-cxx-id
     "illegal name for a C++ export: ~s" o-s))
  s)

(define (string->internal-cxx-id s #:default [default "_"])
  (set! s (string-underscorify s))
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (translate-id-string s))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  (if (string=? "" s) default s))

;;; 
;;; C++ renaming
;;; 

;; Renames Ids to legal C++ symbols. Tracks renamings using a map.
;; Does fairly "stable" renaming by limiting the context of locals to
;; the function body.
(define (defs-cxx-rename ast-lst)
  ;;(pretty-print ast-lst) (exit)
  
  ;; We use (next-gensym r sym) with this to do numbering. All the
  ;; global names are in this table by Id-name. When visiting function
  ;; bodies we update functionally.
  (define r #hasheq())
  
  ;; Id-bind to renamed symbol mappings are stored here.
  (define id->sym (make-hasheq))

  ;; More than one binding may get the same name, but each binding is
  ;; unique. As a form of consistency checking, we do not allow two
  ;; registrations for the same `id`.
  (define (record-cxx-name! id n-sym)
    (define bind (Id-bind id))
    (when (hash-has-key? id->sym bind)
      (raise-assertion-error
       'record-cxx-name!
       "C++ name already recorded for ~s: ~s"
       id (hash->list id->sym)))
    (hash-set! id->sym bind n-sym))

  ;; Returns #f if `id` has no chosen name registration.
  (define (lookup-cxx-name id)
    (hash-ref id->sym (Id-bind id) #f))
  
  ;; Decides name for an ID binding. For a name that has no allowed
  ;; characters, `stem` is used instead as the base name. Returns
  ;; (values r sym).
  (define (decide-name-for-id r id stem)
    (define orig-s (Id->string id))
    (define cand-s (string->internal-cxx-id orig-s #:default stem))
    (define-values (n-r n-sym) (next-gensym r (string->symbol cand-s)))
    (record-cxx-name! id n-sym)
    (values n-r n-sym))

  ;; Gets decided name for an ID reference.
  (define (get-decision-for-id id)
    (define sym (lookup-cxx-name id))
    (unless sym
      (raise-assertion-error
       'defs-cxx-rename
       "expected C++ name to have been decided for ~s" id))
    sym)

  (define (get-or-decide-for-id r id stem)
    (define sym (lookup-cxx-name id))
    (if sym
        (values r sym)
        (decide-name-for-id r id stem)))

  (define (CxxDefun-specified-ext ast) ;; (or/c #f #t identifier?)
    (define a (Ast-annos ast))
    (or (get-export-name a)
        (get-foreign-name a)))
  
  (define (fun-register-requested! ast)
    (when (CxxDefun? ast)
      (define specified-ext (CxxDefun-specified-ext ast))
      (when (identifier? specified-ext)
        (define id (Def-id ast))
        (define id-s (Id->string id))
        (define cxx-sym
          (let ()
            (define sym (syntax-e specified-ext))
            sym))
        (let ((uniq-cxx-sym #f))
          (set!-values (r uniq-cxx-sym) (next-gensym r cxx-sym)))
        (record-cxx-name! id cxx-sym))))

  (define (fun-register-derived! ast)
    (when (CxxDefun? ast)
      (define specified-ext (CxxDefun-specified-ext ast))
      (unless (identifier? specified-ext)
        (define id (Def-id ast))
        (define id-s (Id->string id))
        (define cxx-sym
          (match specified-ext
            [#t
             (string->symbol (string->exported-cxx-id id-s))]
            [#f
             (define a (Ast-annos ast))
             (define local-s
               (if-let owner-id (hash-ref a 'owner-id #f)
                 (string-append (Id->string owner-id) "_" id-s)
                 id-s))
             (define cand-s
               (string->internal-cxx-id local-s #:default "f"))
             (string->symbol cand-s)]))
        (set!-values (r cxx-sym) (next-gensym r cxx-sym))
        (record-cxx-name! id cxx-sym))))
        
  ;; We must collect all top-level IDs (and decide on their C++ name)
  ;; before renaming any locals. We first record explicitly requested
  ;; names, and only after that assign derived names to ensure that we
  ;; do not derive a requested name.
  (for-each fun-register-requested! ast-lst)
  (for-each fun-register-derived! ast-lst)

  ;; Returns (values r ast).
  (define (rw r ast)
    (match ast
      ((Var a id)
       (define sym (get-decision-for-id id))
       (values r (Var a sym)))
      ((CxxDefun a id m t ps b)
       (define n-sym (get-decision-for-id id))
       (define-values (r-dummy n-ast)
         (rw-all r (CxxDefun a n-sym m t ps b)))
       (values r n-ast))
      ((Param a id t)
       (define-values (n-r n-sym) (decide-name-for-id r id "a"))
       (values n-r (Param a n-sym t)))
      ((DefVar a id t v)
       (define-values (r-1 n-v) (rw r v))
       (define-values (r-2 n-sym) (decide-name-for-id r-1 id "v"))
       (values r-2 (DefVar a n-sym t n-v)))
      ((DeclVar a id t)
       (define-values (r-2 n-sym) (decide-name-for-id r id "v"))
       (values r-2 (DeclVar a n-sym t)))
      ((LabelDef a id)
       (define-values (n-r n-sym) (get-or-decide-for-id r id "l"))
       (values n-r (LabelDef a n-sym)))
      ((Goto a id)
       (define-values (n-r n-sym) (get-or-decide-for-id r id "l"))
       (values n-r (Goto a n-sym)))
      (_
       (rw-all r ast))))

  ;; Rewrites subterms of 'ast', updating 'r' in the process.
  (define (rw-all r ast)
    (let ((ast
           (term-rewrite-all
            (lambda (ast)
              (let-values (((sub-r ast) (rw r ast)))
                (set! r sub-r)
                ast))
            ast)))
      (values r ast)))

  (define (rw-drop r ast)
    (let-values (((r ast) (rw r ast)))
      ast))

  (set! ast-lst (map (fix rw-drop r) ast-lst))
  ;;(writeln ast-lst)
  ast-lst)

;;; 
;;; partitioning
;;; 

;; One of:
;; 'public-prototypes
;; 'private-types
;; 'private-prototypes
;; 'private-implementations
(define cxx-kind (make-parameter #f))

(define (cxx->partition ast)
  ;;(writeln ast)
  (match ast
    ((CxxDefun a id m t ps b)
     (define foreign? (and (get-foreign-name a) #t))
     (define export? (and (get-export-name a) #t))
     (define proto? (memq (cxx-kind) '(public-prototypes private-prototypes)))
     ;; "static" for locals, "MGL_API_" for exports, "MGL_" for
     ;; non-exports, "FUNC" for function definitions, and "PROTO" for
     ;; function prototypes (Lua-inspired naming).
     (define modif
       (string-append
        (if export? "MGL_API_" "MGL_")
        (if proto? "PROTO" "FUNC")))
     (set! m (cons modif m))
     (cond
      (foreign? #f)
      ((eq? (cxx-kind) 'private-implementations)
       (CxxDefun a id m t ps b))
      ((and proto?
            (or (and export? (eq? (cxx-kind) 'public-prototypes))
                (and (not export?) (eq? (cxx-kind) 'private-prototypes))))
       (Proto a id m t ps))
      (else #f)))
    (else
     (unsupported ast))))

(define (defs->partition kind def-lst)
  (filter
   values
   (parameterize ((cxx-kind kind))
     (map cxx->partition def-lst))))

;;; 
;;; C++ translation
;;; 

(define (CxxDefun-rm-SeqExpr def)
  (define (to-expr ast)
    (match ast
      [(or (? Var?) (? Literal?) (? VoidExpr?))
       ast]
      [(or (? ApplyExpr?) (? IfExpr?))
       (term-rewrite-all to-expr ast)]
      [(SeqExpr a es)
       (define t (Expr-type ast))
       (define void-t? (Void-type? t))
       (define id (fresh-Id 'lifted))
       (unless void-t?
         (set! es (list-map-last
                   (lambda (e) (annoless AssignStat
                                    (annoless Var id) e)) es)))
       (define ss (map to-stat es))
       (LiftStatExpr (hasheq 'type t) id ss)]
      [(ExprStat _ e)
       (to-expr e)]
      [(LiftStatExpr a id ss)
       (LiftStatExpr a id (map to-stat ss))]
      [_
       (raise-argument-error
        'to-expr "supported Expr? or Stat? or Def?" ast)]))
  
  (define (to-stat ast)
    (match ast
      [(or (? Var?) (? Literal?) (? VoidExpr?))
       ;; Discarding result due to statement context.
       (annoless SeqStat '())]
      [(? ApplyExpr?)
       (annoless ExprStat (term-rewrite-all to-expr ast))]
      [(or (? AssignStat?) (? ReturnStat?))
       (term-rewrite-all to-expr ast)]
      [(AssignExpr a lv rv)
       (AssignStat a (to-expr lv) (to-expr rv))]
      [(DefVar a id t b)
       (DefVar a id t (to-expr b))]
      [(? SeqStat?)
       (term-rewrite-all to-stat ast)]
      [(SeqExpr a es)
       ;; Due to the statement context, the result of the expression
       ;; can be discarded.
       (SeqStat a (map to-stat es))]
      [(IfExpr a c t e)
       ;; Discarding result due to statement context.
       (IfStat a c (to-stat t) (to-stat e))]
      [(ExprStat _ e)
       (to-stat e)]
      [(LiftStatExpr a id ss)
       (define dv (annoless DeclVar id (Expr-type ast)))
       (SeqStat a (cons dv (map to-stat ss)))]
      [(or (? Goto?) (? DeclVar?) (? NoBody?) (? LabelDef?))
       ast]
      [_
       ;;(writeln `(result-discarded = ,(get-result-discarded ast)))
       (raise-argument-error
        'to-stat "supported Expr? or Stat? or Def?" ast)]))
  
  (let ((s (CxxDefun-s def)))
    (set-CxxDefun-s def (to-stat s))))

;; Performs initial translation from IR to C++. If `exprify?`, prefers
;; to keep expressions as expressions, although that results in
;; odd-looking C++.
(define (defs->cxx exprify? a-def-lst)
  (define (fun->cxx ast) ;; (-> Defun? CxxDefun?)
    (define ds null) ;; (listof DeclVar?), in reverse appearance order

    (define (add-decl! d)
      (set! ds (cons d ds)))

    (define (local-def->cxx-expr ast) ;; (-> Def? Expr?)
      (match ast
        [(? DeclVar?)
         (add-decl! ast)
         the-empty-SeqExpr]
        [(DefVar a id t v)
         (add-decl! (DeclVar a id t))
         (annoless AssignExpr
                   (annoless Var id)
                   (expr->cxx v))]
        [_
         (raise-argument-error
          'local-def->cxx "supported local Def?" ast)]))

    (define (local-def->cxx-stat ast) ;; (-> Def? Stat?)
      (match ast
        [(? DeclVar?)
         ast]
        [(DefVar a id t v)
         (DefVar a id t (expr->cxx v))]
        [_
         (raise-argument-error
          'local-def->cxx "supported local Def?" ast)]))

    (define local-def->cxx
      (if exprify?
          local-def->cxx-expr
          local-def->cxx-stat))
    
    ;; Favors expressions in translation where possible (as they are
    ;; closer to the original), and translates into statements
    ;; otherwise.
    (define (expr->cxx ast)
      (match ast
        [(? Var?)
         ast]
        [(? Literal?)
         ast]
        [(? VoidExpr?)
         ast]
        [(ApplyExpr a f es)
         (ApplyExpr a f (map expr->cxx es))]
        [(SeqExpr a ss)
         (SeqExpr a (map expr->cxx ss))]
        [(LetExpr a dv ss)
         (SeqExpr a (cons (local-def->cxx dv) (map expr->cxx ss)))]
        [(IfExpr a c t e)
         (IfExpr a (expr->cxx c) (expr->cxx t) (expr->cxx e))]
        [(AssignExpr a lhs rhs)
         (AssignExpr a (expr->cxx lhs) (expr->cxx rhs))]
        [_
         (raise-argument-error
          'expr->cxx "supported Expr?" ast)]))

    (match ast
      [(Defun a id t ps b)
       (define foreign? (and (get-foreign-name a) #t))
       (define n-b
         (cond
           [foreign?
            the-NoBody]
           [else
            (define n-e (expr->cxx b))
            (define ls
              (if (equal? (FunT-rt t) the-Void-type)
                  (if (ineffective-atom? n-e)
                      null
                      (list (annoless ExprStat n-e)))
                  (list (annoless ReturnStat n-e))))
            (annoless SeqStat (append (reverse ds) ls))]))
       (CxxDefun a id null t ps n-b)]))

  (for/list ((mgl-fun (filter Defun? a-def-lst)))
    (define cxx-fun (fun->cxx mgl-fun))
    (unless exprify?
      (set! cxx-fun (CxxDefun-rm-SeqExpr cxx-fun)))
    cxx-fun))

(define-with-contract
  (-> Def? (set/c symbol? #:cmp 'eq))
  (def-assign-targets def)
  (define targets (mutable-seteq))
  ((topdown-visitor
    (match-lambda
      ((AssignStxp _ (Var _ lv-id) _)
       (set-add! targets (Id-bind lv-id)))
      (_ (void))))
   def)
  targets)

(define (defs-types-to-cxx defs-t def-lst)
  (define (annos-type-param? as)
    (hash-ref as 'type-param #f))

  (define (type->cxx ast)
    (match ast
      [(NameT a id)
       (cond
        [(annos-type-param? a)
         ;; Has no specified C++ name, so leave as is.
         ast]
        [else
         (define def (ast-identifier-lookup defs-t id))
         (unless def
           (raise-language-error/ast
            "reference to unbound type ID"
            ast id))
         (match def
           ((ForeignTypeDecl _ _ cxx-t)
            cxx-t))])]
      [(ParamT a t ats)
       (ParamT a (type->cxx t) (map type->cxx ats))]
      [_
       (raise-argument-error
        'type->cxx "(or/c NameT? ParamT?)" ast)]))
  
  (define (rw-def def)
    (define non-const-bind-set
      (def-assign-targets def))

    (define (var-def-type->cxx id t)
      (define non-const?
        (set-member? non-const-bind-set (Id-bind id)))
      (define cxx-t (type->cxx t))
      (if non-const?
          cxx-t
          (annoless ConstT cxx-t)))
    
    (define rw
      (topdown
       (lambda (ast)
         (match ast
           [(? CxxDefun?)
            (define t (CxxDefun-rtype ast))
            (define rt (type->cxx (FunT-rt t)))
            (struct-copy CxxDefun ast [rtype rt])]
           [(Param a id t)
            (Param a id (annoless RefT (annoless ConstT (type->cxx t))))]
           [(DefVar a id t v)
            (DefVar a id (var-def-type->cxx id t) v)]
           [(DeclVar a id t)
            (DeclVar a id (var-def-type->cxx id t))]
           [(ApplyExpr (and a (app (lambda (a) (hash-ref a 'type<> #f))
                                   (? identity ts))) f args)
            (ApplyExpr (hash-set a 'type<> (map type->cxx ts)) f args)]
           [(Literal a dat)
            (define cxx-t (type->cxx (Expr-type ast)))
            (Literal (hash-set a 'cxx-type cxx-t) dat)]
           [_ ast]))))

    (rw def))
  
  (map rw-def def-lst))

;;; 
;;; simplification
;;; 

(define (ineffective-atomic-stat? ast)
  (matches? ast (ExprStat _ (? ineffective-atom?))))
  
(define (ast-cxx-trim-comprehensively ast)
  ;;(pretty-print ast)

  (define (rw ast)
    (match ast
      [(SeqExpr _ (list e))
       e]
      [(SeqExpr a ss)
       #:when (ormap SeqExpr? ss)
       (let ([ss
              (append-map
               (match-lambda
                 [(SeqExpr _ ss) ss]
                 [s (list s)])
               ss)])
         (rw (SeqExpr a ss)))]
      [(SeqExpr a (list es ... e))
       #:when (ormap ineffective-atom? es)
       (define n-es
         (filter (negate ineffective-atom?) es))
       (rw (SeqExpr a (append n-es (list e))))]
      [(IfExpr a c t e)
       #:when (equal? t e)
       (rw (SeqExpr a (list c t)))]
      [(SeqStat _ (list e))
       e]
      [(SeqStat a ss)
       #:when (ormap (lambda (x) (or (SeqStat? x)
                                (ineffective-atomic-stat? x))) ss)
       (let ([ss
              (append-map
               (match-lambda
                 [(SeqStat _ ss) ss]
                 [(? ineffective-atomic-stat?) null]
                 [s (list s)])
               ss)])
         (rw (SeqStat a ss)))]
      [(IfStat a c t e)
       #:when (equal? t e)
       (rw (SeqStat a (list (annoless ExprStat c) t)))]
      [_ ast]))

  ((bottomup rw) ast))

;;; 
;;; lifting of statements
;;; 

(define (ast-contains? p? ast)
  (let/ec k
    ((topdown-visitor
      (lambda (ast)
        (when (p? ast)
          (k #t)))) ast)
    (k #f)))

(define has-lifts?
  (fix ast-contains? LiftStatExpr?))

(define (prepend-stats s ss)
  (annoless SeqStat (append ss (list s))))

(define (def-rm-LiftStatExpr ast)
  ;; Wraps statement `ast` with the specified lifts, returning a new
  ;; statement. The statements `ss` are assumed to be in evaluation
  ;; order. The declarations `ds` should be of type DeclVar, and
  ;; their order does not matter.
  (define (wrap ds ss ast)
    (cond
     ((and (null? ds) (null? ss))
      ast)
     (else
      (prepend-stats ast (append ds ss)))))
  
  (define (rw-siblings-in-reverse ds ss ast-lst)
    (for/fold ([ds ds] [ss ss] [lst '()]) ([ast (reverse ast-lst)])
      (define-values (n-ds n-ss n-ast) (rw-sibling ds ss ast))
      (values n-ds n-ss (cons n-ast lst))))

  (define (lift-expr ast)
    (define t (Expr-type ast))
      (cond
       ((Void-type? t)
        (define def (rw-stat (annoless ExprStat ast)))
        (define ref the-VoidExpr)
        (values def ref))
       (else
        (define tmp-id (if (Var? ast)
                           (another-Id (Var-id ast))
                           (fresh-Id 'lifted)))
        (define def (rw-stat (annoless DefVar tmp-id t ast)))
        (define ref (Var (hasheq 'type t) tmp-id))
        (values def ref))))
  
  ;; Rewrites expression `ast`, where `ds` is a list of variables to
  ;; declare in the same lift context, and `ss` is a list of
  ;; statements to go into the same lift context after any lifts from
  ;; `ast`. Rewrites the entire expression `ast`, but only up to and
  ;; including any LiftStatExpr nodes. Traverses in reverse evaluation
  ;; order (in practice this means depth-first, mostly right-to-left).
  (define (rw-sibling ds ss ast)
    (cond
     [(LiftStatExpr? ast)
      (match-define (LiftStatExpr a id n-ss) ast)
      (define t (Expr-type ast))
      (cond
       ((Void-type? t)
        (values ds (append n-ss ss) the-VoidExpr))
       (else
        (define decl (annoless DeclVar id t))
        (define ref (Var (hasheq 'type t) id))
        (values (cons decl ds) (append n-ss ss) ref)))]
     [(not (null? ss))
      ;; Since some later subexpressions have been lifted, we must
      ;; lift this one, too.
      (define-values (def ref) (lift-expr ast))
      (values ds (cons def ss) ref)]
     [else
      (match ast
        [(ApplyExpr a f as)
         (define-values (as-ds as-ss as-exprs)
           (rw-siblings-in-reverse '() '() as))
         ;; Arguments evaluate before application, and we need not
         ;; lift the function application even if the argument
         ;; expressions contain lifts.
         (values (append as-ds ds)
                 (append as-ss ss)
                 (ApplyExpr a f as-exprs))]
        [(IfExpr a c t e)
         (define-values (t-ds t-ss t-ast) (rw-expr t))
         (define-values (e-ds e-ss e-ast) (rw-expr e))
         (cond
          [(and (null? t-ss) (null? e-ss))
           ;; There are no statement lifts in the branches, so it is
           ;; enough to do any that appear in `c`.
           (define-values (c-ds c-ss c-ast) (rw-expr c))
           (values (append c-ds t-ds e-ds ds)
                   (append c-ss ss)
                   (IfExpr a c-ast t-ast e-ast))]
          [else
           ;; Either `t` or `e` contain lifts. We must turn the entire
           ;; IfExpr into an IfStat so that we can keep the `t` and
           ;; `e` lifts in their branches. The branches must assign
           ;; the expression result. We must furthermore process any
           ;; lifts in the newly created IfStat (which contains no
           ;; other statements), but said lifts can go into the new
           ;; statement context.
           (define if-typ (Expr-type ast))
           (define if-id (fresh-Id 'lifted))
           (define if-decl (annoless DeclVar if-id if-typ))
           (define if-ref (Var (hasheq 'type if-typ) if-id))
           (define t-ast (wrap-expr-as-Assign if-ref t))
           (define e-ast (wrap-expr-as-Assign if-ref e))
           (define if-ast (IfStat a c t-ast e-ast))
           (values (cons if-decl ds)
                   (cons (rw-stat if-ast) ss)
                   if-ref)])]
        [_
         ;; The expression `ast` does not contain sub-expressions.
         (values ds ss ast)])]))
  
  (define (rw-expr ast)
    ;;(pretty-print `(rw-expr FROM ,ast))
    (rw-sibling '() '() ast))

  (define (wrap-expr-as-Assign lv rv)
    (define t (Expr-type rv))
    (rw-stat
     (if (Void-type? t)
         (annoless ExprStat rv)
         (annoless AssignStat lv rv))))
  
  ;; Processes any R-value expressions of the immediate C++ statement
  ;; `ast`.
  (define (rw-stat ast)
    ;; Where a statement has multiple expressions, we must be sure to
    ;; treat them as siblings, and retain appropriate order, since the
    ;; lifts end up in the same context.
    (match ast
      [(DefVar a id t v)
       #:when (has-lifts? v)
       (define-values (ds ss n-v) (rw-expr v))
       (wrap ds ss (DefVar a id t n-v))]
      [(AssignStat a lv rv)
       #:when (has-lifts? rv)
       (define-values (ds ss n-rv) (rw-expr rv))
       (wrap ds ss (AssignStat a lv n-rv))]
      [(IfStat a c t e)
       #:when (has-lifts? c)
       (define-values (ds ss n-c) (rw-expr c))
       (wrap ds ss (IfStat a n-c t e))]
      [(ReturnStat a v)
       #:when (has-lifts? v)
       (define-values (ds ss n-v) (rw-expr v))
       (wrap ds ss (ReturnStat a n-v))]
      [(ExprStat a v)
       #:when (has-lifts? v)
       (define-values (ds ss n-v) (rw-expr v))
       (wrap ds ss (ExprStat a n-v))]
      [_ ast]))
  
  (define rw-all-stats
    (bottomup
     (lambda (ast)
       (if (or (Stat? ast) (DefVar? ast))
           (rw-stat ast)
           ast))))
  
  (let ()
    (define n-ast
      (rw-all-stats ast))
    ;;(pretty-print `(def-rm-LiftStatExpr FROM ,ast TO ,n-ast))
    n-ast))

;;; 
;;; removal of unreferenced variables
;;; 

(define (rm-unreferenced-var-decl an-ast)
  (define all-uses (make-hasheq)) ;; bind -> count
  (define lv-uses (make-hasheq)) ;; bind -> count

  (define (add-use! h bind)
    (hash-update! h bind (lambda (v) (add1 v)) 0))
  
  ((topdown-visitor
    (lambda (ast)
      (match ast
        [(Var _ id)
         (define bind (Id-bind id))
         (add-use! all-uses bind)]
        [(AssignStxp _ (Var _ id) _)
         (define bind (Id-bind id))
         (add-use! lv-uses bind)]
        [_ ast])))
   an-ast)

  (define lv-only-uses
    (for*/seteq ([(bind lv-k) lv-uses]
                 [all-k (in-value (hash-ref all-uses bind))]
                 #:when (= lv-k all-k))
                bind))
  (define uses
    (set-subtract
     (for/seteq ([(bind v) all-uses]) bind)
     lv-only-uses))

  (define (unused-Id? id)
    (not (set-member? uses (Id-bind id))))

  (define (lv-only-Id? id)
    (set-member? lv-only-uses (Id-bind id)))

  (define (rw ast)
    (match ast
      [(DeclVar _ (? unused-Id? id) _)
       the-NopStat]
      [(DefVar _ (? unused-Id? id) _ e)
       (annoless ExprStat e)]
      [(AssignStat a (Var _ (? lv-only-Id? id)) e)
       (ExprStat a e)]
      [(AssignExpr _ (Var _ (? lv-only-Id? id)) e)
       e]
      [_ ast]))
  
  ((bottomup rw) an-ast))

;;; 
;;; introduce variables for arguments
;;; 

;; For each assigned-to argument, introduces a (non-`const`) variable
;; to which to assign to instead.
(define-with-contract
  (-> CxxDefun? CxxDefun?)
  (fun-ensure-mutable-assigns def)

  (define param-ids
    (for/mutable-setId ((par (CxxDefun-params def)))
      (Param-id par)))

  (unless (set-empty? param-ids)
    (define par-lv-ids (mutable-setId))
    ((topdown-visitor
      (match-lambda
        ((AssignStat _ (Var _ lv-id) _)
         #:when (set-member? param-ids lv-id)
         (set-add! par-lv-ids lv-id))
        (_ (void))))
     def)

    (unless (set-empty? par-lv-ids)
      (define arg-lst
        (for/list ((par (CxxDefun-params def))
                   #:when (set-member? par-lv-ids (Param-id par)))
          (list par (another-Id (Param-id par)))))
      (define arg-h
        (for/mutable-hashId ((arg arg-lst))
          (define par (first arg))
          (values (Param-id par) (second arg))))
      (define body (CxxDefun-s def))
      (match-define (SeqStat a ss) body)
      (define rw
        (alltd
         (match-lambda
           ((Var a id)
            #:when (set-member? par-lv-ids id)
            (Var a (dict-ref arg-h id)))
           (ast #f))))
      (define n-ss
        (append (for/list ((arg arg-lst))
                  (define par (first arg))
                  (define t (Param-t par))
                  (annoless DefVar
                            (second arg)
                            t
                            (Var (hasheq 'type t) (Param-id par))))
                (map rw ss)))
      (define n-ast (SeqStat a n-ss))
      (define n-def (set-CxxDefun-s def n-ast))
      ;;(pretty-print n-def)
      (set! def n-def)))
  
  def)

;;; 
;;; removal of redundant jumps
;;; 

;; Applies `f` to the elements of list `xs` in reverse order, which
;; matters as state `st` is threaded through the list transformation.
(define (map/reverse/state f st xs)
  (for/fold ([st st] [lst '()]) ([x (reverse xs)])
    (define-values (n-st n-x) (f st x))
    (values n-st (cons n-x lst))))

(define (defs-cxx-fun-optimize def-lst)
  ;; Note that a no-op does not change state.
  (define (g st s)
    ;;(writeln `(g on ,s))
    (match s
      [(? LetStat?)
       (raise-assertion-error 
        'defs-cxx-fun-optimize
        "assumed no LetStat")]
      [(SeqCont ss)
       (define-values (n-st n-ss) (map/reverse/state g st ss))
       (values n-st (copy-SeqCont s n-ss))]
      [(IfStat a c t e)
       (define-values (st0 n-t) (g st t))
       (define-values (st1 n-e) (g st e))
       ;; We do account for the special case where both branches of an
       ;; IfStat begin with the same label.
       (values (and st0 st1 (Id-bind=? st0 st1) st0)
               (IfStat a c n-t n-e))]
      [(LabelDef _ id) 
       ;;(writeln `(store ,id))
       (values id s)]
      [(Goto _ (? (lambda (id) (and st (Id-bind=? st id)))))
       ;;(writeln `(delete ,s))
       (values st the-NopStat)]
      [_ 
       (values #f s)]))
  
  (define (stat-rm-goto-next s)
    (define-values (st n-s) (g #f s))
    n-s)
  
  (define (stat-rm-unused-labels s)
    ;; The `targets` set is that of label `bind` values in `s` that
    ;; are Goto targets.
    (define targets (mutable-seteq))
    ((topdown-visitor
      (lambda (ast)
        (when (Goto? ast)
          (define id (Goto-id ast))
          (define bind (Id-bind id))
          (set-add! targets bind))))
     s)
    ;; Replace any unreferenced labels with no-ops.
    ((topdown
      (lambda (ast)
        (match ast
          [(LabelDef _ (? (lambda (id)
                            (define bind (Id-bind id))
                            (not (set-member? targets bind)))))
           the-NopStat]
          [_ ast])))
     s))
  
  (define (defun-optimize ast)
    (set! ast (fun-ensure-mutable-assigns ast))
    (define s (CxxDefun-s ast))
    (set! s (stat-rm-goto-next s))
    (set! s (stat-rm-unused-labels s))
    (define def (set-CxxDefun-s ast s))
    ;;(pretty-print `(BEFORE ,def))
    (set! def (fun-propagate-copies def))
    ;;(pretty-print `(AFTER ,def))
    (set! def (rm-unreferenced-var-decl def))
    (set! def (ast-cxx-trim-comprehensively def))
    ;;(pretty-print `(AFTER ,def))
    def)

  (map (lambda (def)
         (if (CxxDefun? def)
             (defun-optimize def)
             def)) 
       def-lst))

;;; 
;;; sorting of top-level declarations
;;; 

(define (defs-cxx-decl-sort lst)
  (sort lst symbol<? #:key Def-id))

;;; 
;;; pretty-printing preparation
;;; 

;; Updates expression (not declaration) type information, and inserts
;; VoidCast nodes as required to restore type consistency as the
;; interpretation of assignment expressions changes.
(define-with-contract
  (-> Ast? Ast?) 
  (ast-fix-expr-cxx-types an-ast)
  
  (define (voidify t e)
    (if (Void-type? t)
        e
        (VoidCast (hasheq 'type the-Void-type) e)))

  (define (rw ast)
    (match ast
      [(AssignExpr a lv rv)
       (define t (Expr-type rv))
       (assert t)
       (set-Expr-type ast t)]
      [(SeqExpr a (list es ... e))
       (define t (Expr-type e))
       (assert t)
       (set-Expr-type ast t)]
      [(IfExpr a c t e)
       (define t-t (Expr-type t))
       (define e-t (Expr-type e))
       (assert (and t-t e-t))
       (cond
         [(equal? t-t e-t)
          (set-Expr-type ast t-t)]
         [else
          (IfExpr (hash-set a 'type the-Void-type)
                  c
                  (voidify t-t t)
                  (voidify e-t e))])]
      [_ ast]))

  ((bottomup rw) an-ast))

(define (defs-cxx->pp def-lst)
  (define (s->ss ast) ;; (-> Stat? SeqStat?)
    (cond
     [(SeqStat? ast) ast]
     [else (annoless SeqStat (list ast))]))

  (define (rw in-expr? ast)
    (define n-ast
      (match ast
        [(or (? Var?) (? Literal?) (? Type?))
         ast]
        [(IfStat a c t e)
         (IfStat a (rw/no c) (s->ss (rw/no t)) (s->ss (rw/no e)))]
        [(AssignStat a lv rv)
         (let ((lv (rw/yes lv))
               (rv (rw/yes rv)))
           (ExprStat a (AssignExpr (hasheq 'type (Expr-type rv)) lv rv)))]
        [(ExprStat a e)
         (ExprStat a (rw/no e))]
        [(ReturnStat a e)
         (ReturnStat a (rw/no e))]
        [(? Expr?)
         (term-rewrite-all rw/yes ast)]
        [(or (? Stat?) (? Def?))
         (term-rewrite-all rw/no ast)]
        [_
         (error 'defs-cxx->pp "unsupported: ~s" ast)]))
    
    (when (and in-expr?
               (any-pred-holds SeqExpr? IfExpr? AssignExpr? n-ast))
      (set! n-ast (Parens (hasheq 'type (Expr-type n-ast)) n-ast)))
    
    n-ast) ;; end `rw`

  (define rw/no (fix rw #f))
  (define rw/yes (fix rw #t))

  (define (rw-fun ast)
    (match ast
      [(fields CxxDefun [s b])
       (cond
         [(NoBody? b) 
          ast]
         [else
          (let* ((b (ast-fix-expr-cxx-types b))
                 (b (rw/no b))
                 (b (if (SeqStat? b)
                        b
                        (annoless SeqStat (list b)))))
            (set-CxxDefun-s ast b))])]))
  
  (map rw-fun def-lst))
  
;;; 
;;; driver routines
;;; 

(define (get-suffix kind)
  (define tbl `((cc ".cpp")
                (hh ".hpp")))
  (define p (assq kind tbl))
  (unless p
    (raise-argument-error 'get-suffix
                          "either 'cc or 'hh" kind))
  (values (second p)))

(define (pp-exit obj)
  (pretty-print obj)
  (exit))
  
(define-with-contract*
  (-> list? (listof Def?)
      path-string? (or/c #f output-port?) boolean?
      void?)
  (generate-cxx-file spec a-def-lst path-stem out banner?)

  (define parts '(cc hh))
  (define exprify? #t)
  (let ()
    (match-define (cons 'cxx (? list? opt-lst)) spec)
    (for ([opt opt-lst])
      (match opt
        [(list 'parts (and (or 'cc 'hh) lst) ...)
         (set! parts (remove-duplicates lst eq?))]
        ['expr
         (set! exprify? #t)]
        ['stat
         (set! exprify? #f)])))
  
  (define (pp-only obj)
    (pretty-print obj)
    obj)
  
  (define def-lst
    (let ((defs-t (build-tl-defs-table a-def-lst)))
      (let-> lst
        a-def-lst
        (defs->cxx exprify? lst)
        (map def-rm-LiftStatExpr lst)
        (defs-cxx-fun-optimize lst)
        ;;(pp-exit lst)
        (defs-types-to-cxx defs-t lst)
        (defs-cxx-rename lst)
        (defs-cxx-decl-sort lst)
        (defs-cxx->pp lst))))
  
  (for ([part parts])
    (match part
     ['cc
      (define sfx (get-suffix part))
      (define path (path-add-suffix path-stem sfx))
      (define filename (path-basename-as-string path))
      (define basename (path-basename-only-as-string filename))
      (define hh-incl 
        (annoless Include 'user 
                  (path-basename-as-string 
                   (path-add-suffix path-stem (get-suffix 'hh)))))
      (define c-unit
        (append (if (memq 'hh parts) (list hh-incl) null)
                (defs->partition 'private-prototypes def-lst)
                (defs->partition 'private-implementations def-lst)))
      ;;(for-each writeln c-unit) (exit)
      (define s (format-c c-unit))
      (write-generated-output
       path out
       (thunk
        (when banner?
          (display-banner "//" filename))
        (display-generated-notice "//")
        (display s)
        (newline)))]
     ['hh
      (define sfx (get-suffix part))
      (define path (path-add-suffix path-stem sfx))
      (define filename (path-basename-as-string path))
      (define basename (path-basename-only-as-string filename))
      (define config-incl (annoless Include 'user (string-append basename "_config.hpp"))) ;; xxx for now, until we infer required includes
      (define harness-begin (annoless TlVerbatim (string-append "#ifndef " (path-h-ifdefy filename))))
      (define harness-end (annoless TlVerbatim "#endif"))
      (define c-unit
        (append (list harness-begin config-incl)
                (defs->partition 'public-prototypes def-lst)
                (list harness-end)))
      ;;(for-each writeln c-unit) (exit)
      (define s (format-c c-unit))
      (write-generated-output
       path out
       (thunk
        (when banner?
          (display-banner "//" filename))
        (display-generated-notice "//")
        (display s)
        (newline)))]))

  (void))
