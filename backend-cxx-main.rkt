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
         "ast-id-coll.rkt"
         "ast-ir.rkt"
         "backend-cxx-ast.rkt"
         "backend-cxx-print.rkt"
         "backend-util.rkt"
         "compiler-rewrites.rkt"
         "strategy-stratego.rkt"
         "strategy-term.rkt"
         "strategy.rkt"
         "util.rkt"
         "util/system.rkt")

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
(define (cxx-rename ast-lst)
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
       'cxx-rename
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
;;; C++ translation
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

(define (defs->cxx defs-t)
  (define (def->cxx ast)
    (match ast
      [(Defun a id t ps b)
       (define foreign? (and (get-foreign-name a) #t))
       (CxxDefun a id null t
                 (map def->cxx ps)
                 (cond
                  (foreign? the-NoBody)
                  ((equal? (FunT-rt t) the-Void-type) (expr->cxx b))
                  (else (annoless ReturnStat (expr->cxx b)))))]
      [(? Param?)
       ast]
      [(DefVar a id t v)
       (DefVar a id t (expr->cxx v))]
      [_
       (raise-argument-error
        'def->cxx "supported Def?" ast)]))
  
  ;; Favors expressions in translation (as they are closer to the
  ;; original), and statements otherwise. Leaves in some SeqExpr for
  ;; the time being.
  (define (expr->cxx ast)
    (match ast
      [(? Var?)
       ast]
      [(? Literal?)
       ast]
      [(? VoidStat?)
       ast]
      [(ApplyExpr a f es)
       (ApplyExpr a f (map expr->cxx es))]
      [(SeqExpr a ss)
       (SeqExpr a (map expr->cxx ss))]
      [(LetExpr a dv ss)
       (SeqExpr a (cons (def->cxx dv) (map expr->cxx ss)))]
      [(IfExpr a c t e)
       (IfExpr a (expr->cxx c) (expr->cxx t) (expr->cxx e))]
      [(AssignStat a lhs rhs)
       (AssignStat a (expr->cxx lhs) (expr->cxx rhs))]
      [_
       (raise-argument-error
        'expr->cxx "supported ExprLike?" ast)]))

  (define def-lst (filter Defun? (hash-values defs-t)))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map def->cxx def-lst))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map CxxDefun-rm-SeqExpr def-lst))
  ;;(pretty-print def-lst)
  def-lst)

(define (CxxDefun-rm-SeqExpr def)
  (define (can-be-expr? ast)
    (match ast
      [(or (? Var?) (? Literal?) (? ApplyExpr?) (? IfExpr?))
       #t]
      [(or (? AssignStat?) (? Goto?) (? Def?) (? ReturnStat?)
           (? SeqStat?) (? SeqExpr?) (? VoidStat?))
       #f]
      [_
       (raise-argument-error
        'can-be-expr? "supported ExprLike? or Def?" ast)]))
  
  (define (to-expr ast)
    (match ast
      [(or (? Var?) (? Literal?))
       ast]
      [(or (? ApplyExpr?) (? IfExpr?))
       (term-rewrite-all to-expr ast)]
      [(SeqExpr a es)
       (define t (Expr-type ast))
       (define void-t? (equal? t the-Void-type))
       (define id (fresh-Id 'lifted))
       (unless void-t?
         (set! es (list-map-last
                   (lambda (e) (annoless AssignStat
                                    (annoless Var id) e)) es)))
       (define ss (map to-stat es))
       (LiftStatExpr (hasheq 'type t) id ss)]
      [(LiftStatExpr a id ss)
       (LiftStatExpr a id (map to-stat ss))]
      [_
       (raise-argument-error
        'to-expr "supported ExprLike? or Def?" ast)]))
  
  (define (to-stat ast)
    (match ast
      [(or (? Var?) (? Literal?))
       ;; Discarding result due to statement context.
       (annoless SeqStat '())]
      [(? ApplyExpr?)
       (annoless ExprStat (term-rewrite-all to-expr ast))]
      [(or (? AssignStat?) (? ReturnStat?))
       (term-rewrite-all to-expr ast)]
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
      [(VoidStat a)
       ;; Discarding result due to statement context.
       (SeqStat a '())]
      [(LiftStatExpr a id ss)
       (define dv (annoless DeclVar id (Expr-type ast)))
       (SeqStat a (cons dv (map to-stat ss)))]
      [(or (? Goto?) (? DeclVar?) (? NoBody?) (? LabelDef?))
       ast]
      [_
       ;;(writeln `(result-discarded = ,(get-result-discarded ast)))
       (raise-argument-error
        'to-stat "supported ExprLike? or Def?" ast)]))
  
  (let ((s (CxxDefun-s def)))
    (set-CxxDefun-s def (to-stat s))))

(define-with-contract
  (-> Def? (set/c symbol? #:cmp 'eq))
  (def-assign-targets def)
  (define targets (mutable-seteq))
  ((topdown-visitor
    (match-lambda
      ((AssignStat _ (Var _ lv-id) _)
       (set-add! targets (Id-bind lv-id)))
      (_ (void))))
   def)
  targets)

(define (types-to-cxx defs-t def-lst)
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
            (Literal (hash-update a 'type type->cxx) dat)]
           [_ ast]))))

    (rw def))
  
  (map rw-def def-lst))

;;; 
;;; statement unsplicing
;;; 

(define (empty-CxxBlockStat? ast)
  (matches? ast (CxxBlockStat _ (list))))

(define (ast-rm-SeqStat ast)
  (define (pointless-nest? ast)
    (or (SeqStat? ast)
        (empty-CxxBlockStat? ast)))
  
  (define un-nest
    (bottomup
     (lambda (ast)
       (match ast
         [(SeqCont (? (curry ormap pointless-nest?) ss))
          (define n-ss
            (apply append (for/list ((s ss))
                            (if (pointless-nest? s)
                                (SeqCont-ss s)
                                (list s)))))
          (SeqCont-copy ast n-ss)]
         [else ast]))))
  
  (define convert
    (bottomup
     (lambda (ast)
       (match ast
         [(SeqStat a ss)
          (match ss
            [(list s) s]
            [_ (CxxBlockStat a ss)])]
         [_ ast]))))
  
  (convert (un-nest ast)))

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
       ((equal? t the-Void-type)
        (define def (rw-stat (annoless ExprStat ast)))
        (define ref a-VoidExpr)
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
       ((equal? t the-Void-type)
        (values ds (append n-ss ss) a-VoidExpr))
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
     (if (equal? t the-Void-type)
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

(define (cxx-rm-LiftStatExpr def-lst)
  (map def-rm-LiftStatExpr def-lst))

;;; 
;;; copy propagation
;;; 

(define a-noop (annoless SeqStat null))

;; A Φ value type, as in compiler literature. The `set` is a set of
;; value numbers. Each number is a symbol of the form 'vn* for an
;; actual value, or 'nothing to indicate no value assignment.
(struct Phi (set) #:transparent)

;; Sums together value numbers `xs`. Where `xs` are all the same
;; number, returns the number. Otherwise returns Phi(x ...), where `x`
;; are all distinct numbers.
(define (val-num+ . xs)
  (define vs
    (for/fold ((sum (seteq))) ((x xs))
      (if (Phi? x)
          (set-union sum (Phi-set x))
          (set-add sum x))))
  (if (= (set-count vs) 1)
      (set-first vs)
      (Phi vs)))

(define (fun-propagate-copies def)
  ;; (listof (list/c val-num lv rv))
  (define to-examine null)
  
  (define (annos-add-val-num! a lv-id [rv-ast #f])
    (define val-num (gensym 'vn))
    (define n-a (hash-set a 'val-num val-num))
    (when (and rv-ast (any-pred-holds Var? Literal? rv-ast))
      (define item (list val-num lv-id rv-ast))
      ;;(writeln `(potential ,item))
      (set! to-examine (cons item to-examine)))
    n-a)
  
  ;; Assigns a value number for each "assignment", and adds potentials
  ;; for removal to `to-examine`. Note that: a `DeclVar` has no value;
  ;; and a `Param` has some value, but no expression giving it.
  (define assign-val-nums
    (topdown
     (lambda (ast)
       (match ast
         [(AssignStat a lv rv)
          (cond
            [(and (Var? rv) (equal? lv rv))
             ;; Special case of `x := x`, so can remove
             ;; unconditionally.
             a-noop]
            [else
             (define n-a (annos-add-val-num! a (Var-id lv) rv))
             (AssignStat n-a lv rv)])]
         [(DefVar a id t rv)
          (define n-a (annos-add-val-num! a id rv))
          (DefVar n-a id t rv)]
         [(Param a id t)
          (define n-a (annos-add-val-num! a id))
          (Param n-a id t)]
         [_ ast]))))
  
  ;; Rewrites `def` to remove the assignment identified by
  ;; `examine-item`, or fails returning #f. Traverses `def` in
  ;; execution order in order to do data-flow analysis.
  (define (rw-by-item examine-item def)
    ;;(writeln `(examining ,examine-item))

    (match-define (list tgt-num tgt-lv-id tgt-rv-ast) examine-item)

    (define tgt-rv-id ;; true for Vars, not Literals
      (match tgt-rv-ast
        [(Var _ id) id]
        [(? Literal?) #f]))
    (define tgt-rv-bind (and tgt-rv-id (Id-bind tgt-rv-id)))
    (define tgt-rv-num #f) ;; determined later if `tgt-rv-id`

    (define (maybe-set-rv-num! bind->num rv-ast)
      (when (Var? rv-ast)
        (define rv-bind (Id-bind (Var-id rv-ast)))
        (set! tgt-rv-num (hash-ref bind->num rv-bind))))
    
    ;; For each Goto target (indexed by `bind` value), a sum of their
    ;; bind->num assignments. All Gotos to a LabelDef will have been
    ;; seen before we reach the LabelDef, and hence the set of
    ;; assignments will be complete at that point.
    (define label->bind->num (make-hasheq))

    (define (merge h1 h2)
      (define keys (list->mutable-seteq (hash-keys h1)))
      (for ([k (hash-keys h2)])
        (set-add! keys k))
      (for/fold ((h #hasheq())) ((k (in-set keys)))
        (define v1 (hash-ref h1 k 'nothing))
        (define v2 (hash-ref h2 k 'nothing))
        (hash-set h k (val-num+ v1 v2))))
    
    (define (rw bind->num ast)
      ;;(writeln `(rw of ,ast when ,bind->num))
      (match ast
        [(AssignStat a lv rv)
         (define this-num (hash-ref a 'val-num))
         (assert (Var? lv))
         (define lv-id (Var-id lv))
         (define lv-bind (Id-bind lv-id))
         (values (hash-set bind->num lv-bind this-num)
                 (cond
                  [(and (eq? this-num tgt-num)
                        (Id-bind=? tgt-lv-id lv-id))
                   ;;(writeln `(deleting ,this-num : ,lv-id := ,rv))
                   (maybe-set-rv-num! bind->num rv)
                   a-noop]
                  [else
                   (define n-rv (rw-discard bind->num rv)) 
                   (and n-rv (AssignStat a lv n-rv))]))]
        [(DefVar a lv-id t rv)
         (define this-num (hash-ref a 'val-num))
         (define lv-bind (Id-bind lv-id))
         (values (hash-set bind->num lv-bind this-num)
                 (cond
                   [(eq? this-num tgt-num)
                    ;;(writeln `(deleting ,this-num : ,lv-id := ,rv))
                    (maybe-set-rv-num! bind->num rv)
                    (DeclVar a lv-id t)]
                   [else
                    (define n-rv (rw-discard bind->num rv))
                    (and n-rv (DefVar a lv-id t n-rv))]))]
        [(Param a id t)
         (define this-num (hash-ref a 'val-num))
         (assert (not (eq? this-num tgt-num)))
         (define lv-bind (Id-bind id))
         (values (hash-set bind->num lv-bind this-num) ast)]
        [(IfStat a c t e)
         (define n-c (rw-discard bind->num c))
         (cond
           [(not n-c)
            (values bind->num #f)]
           [else
            (define-values (t-st n-t) (rw bind->num t))
            (cond 
              [(not n-t)
               (values bind->num #f)]
              [else
               (define-values (e-st n-e) (rw bind->num e))
               (cond
                 [(not n-e)
                  (values bind->num #f)]
                 [else
                  (values (merge t-st e-st)
                          (IfStat a n-c n-t n-e))])])])]
        [(Var a (? (fix Id-bind=? tgt-lv-id) id))
         (define this-bind (Id-bind id))
         (define this-num (hash-ref bind->num this-bind))
         ;;(writeln `(examining Var id= ,id num= ,this-num rv= ,tgt-rv-ast num= ,(hash-ref bind->num tgt-rv-bind #f)))
         (assert (not (eq? this-num 'nothing))) ;; undefined, cannot use
         (values bind->num
                 (cond
                   ;; The target assignment is in effect here, so we
                   ;; must substitute the R-value variable or literal,
                   ;; but this only works if it still has the same
                   ;; R-value (literals obviously do).
                   [(eq? this-num tgt-num)
                    (cond
                      [tgt-rv-bind ;; Var
                       (assert tgt-rv-num)
                       (define this-rv-num (hash-ref bind->num tgt-rv-bind))
                       (unless this-rv-num
                         (raise-assertion-error
                          'fun-propagate-copies
                          "no value binding for R-value: ~a := ~a (~a)"
                          tgt-lv-id tgt-rv-id this-num))
                       (and (eq? this-rv-num tgt-rv-num) tgt-rv-ast)]
                      [else ;; Literal
                       tgt-rv-ast])]
                   ;; The target assignment might be in effect here,
                   ;; but we don't know if it is, and hence cannot make
                   ;; this optimization.
                   [(and (Phi? this-num)
                         (set-member? (Phi-set this-num) tgt-num))
                    #f]
                   ;; The target assignment is not in effect here, so
                   ;; this use is unaffected.
                   [else
                    ast]))]
        [(Goto _ id)
         (define bind (Id-bind id))
         (define lbl-nums 
           (merge (hash-ref label->bind->num bind #hasheq()) bind->num))
         (hash-set! label->bind->num bind lbl-nums)
         (values bind->num ast)]
        [(LabelDef _ id)
         (define bind (Id-bind id))
         (define lbl-nums 
           (merge 
            ;; any value assignments from Gotos to this label
            (hash-ref label->bind->num bind #hasheq()) 
            ;; the "fall-in" value assignments
            bind->num))
         (values lbl-nums ast)]
        [_
         ;;(writeln `(no special action for ,ast))
         (rw-all bind->num ast)]))

    ;; Rewrites `ast` and returns the modified AST (or #f), discarding
    ;; changes to assignment table.
    (define (rw-discard bind->num ast)
      (define-values (r n-ast) (rw bind->num ast))
      n-ast)
    
    (define (rw-all bind->num ast)
      (term-rewrite-all/stateful rw bind->num ast))

    (rw-discard #hasheq() def)) ;; end rw-by-item
  
  ;; Tries to rewrite `ast` to remove each of the assignments in
  ;; `to-examine`, but preserves `ast` where removal is not possible.
  (define (examine-all ast)
    ;;(pretty-print to-examine)
    (for/fold ([ast ast]) ([item to-examine])
      (define res (rw-by-item item ast))
      ;;(writeln (list 'TRIED item res))
      (or res ast)))
  
  (examine-all (assign-val-nums def)))

;;; 
;;; removal of unreferenced variables
;;; 

(define (rm-unreferenced-var-decl an-ast)
  (define refs (mutable-seteq))
  
  ((topdown-visitor
    (lambda (ast)
      (when (Var? ast)
        (define id (Var-id ast))
        (define bind (Id-bind id))
        (set-add! refs bind))))
   an-ast)
  
  ((topdown
    (lambda (ast)
      (match ast
        [(DeclVar a id t)
         (define bind (Id-bind id))
         (if (set-member? refs bind)
             ast
             a-noop)]
        [_ ast])))
   an-ast))

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

(define (cxx-fun-optimize def-lst)
  ;; Note that a no-op does not change state.
  (define (g st s)
    ;;(writeln `(g on ,s))
    (match s
      [(? LetStat?)
       (raise-assertion-error 
        'cxx-fun-optimize
        "assumed no LetStat")]
      [(SeqCont ss)
       (define-values (n-st n-ss) (map/reverse/state g st ss))
       (values n-st (SeqCont-copy s n-ss))]
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
       (values st a-noop)]
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
           a-noop]
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
    (rm-unreferenced-var-decl def))
  
  (map (lambda (def)
         (if (CxxDefun? def)
             (defun-optimize def)
             def)) 
       def-lst))

;;; 
;;; lifting of local functions
;;; 

(define (defs-lift-locals defs)
  (define n-defs (make-hasheq))
  (define owner-id (make-parameter #f))

  (define rw-body
    (topdown
     (lambda (ast)
       (cond
        [(and (LetExpr? ast) (Defun? (LetExpr-def ast)))
         (match-define (LetExpr a b ss) ast)
         (do-Defun b)
         (SeqExpr a ss)]
        [else
         ast]))))
  
  (define (do-Defun ast)
    (match-define (Defun a id t ps b) ast)
    (define oid (owner-id))
    (unless (Id-bind=? id oid)
      (set! a (hash-set a 'owner-id oid)))
    (set! b (ast-splice-SeqExpr (rw-body b)))
    (hash-set! n-defs (Id-bind id) (Defun a id t ps b)))
  
  (for ([(bind def) defs])
    (cond
     [(Defun? def)
      (when (ast-anno-maybe def 'top)
        (parameterize ((owner-id (Def-id def)))
          (do-Defun def)))]
     [else
      (hash-set! n-defs bind def)]))
  
  n-defs)

;;; 
;;; sorting of top-level declarations
;;; 

(define (cxx-decl-sort lst)
  (sort lst symbol<? #:key Def-id))

;;; 
;;; pretty-printing preparation
;;; 

(define (cxx->pp lst)
  (define (s->ss ast)
    (cond
     ((CxxBlockStat? ast) (CxxBlockStat-ss ast))
     (else (list ast))))
  
  (define f
    (topdown
     (lambda (ast)
       (match ast
         [(? CxxDefun?)
          (define b (CxxDefun-s ast))
          (cond
           ((CxxBlockStat? b)
            ast)
           ((NoBody? b) 
            ast)
           (else
            (struct-copy 
             CxxDefun ast 
             [s (annoless CxxBlockStat (list b))])))]
         [(IfStat a c t e)
          (PpCxxIfStat a c (s->ss t) (s->ss e))]
         [_
          ast]))))
  
  (map f lst))
  
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
  (-> (listof symbol?) hash? path-string? (or/c #f output-port?)
      boolean? void?)
  (generate-cxx-file kinds defs path-stem out banner?)

  (define defs-t
    (defs-lift-locals defs))
  
  (define (pp-only obj)
    (pretty-print obj)
    obj)
  
  (define def-lst
    (thread1->
     defs-t
     defs->cxx
     ;;pp-exit ;;pp-only
     cxx-rm-LiftStatExpr
     ;;pp-exit
     cxx-fun-optimize
     ;;pp-exit
     (curry map ast-rm-SeqStat)
     (curry types-to-cxx defs-t)
     cxx-rename
     cxx-decl-sort
     cxx->pp))
  
  (for ((kind kinds))
    (cond
     ((eq? kind 'cc)
      (define sfx (get-suffix kind))
      (define path (path-add-suffix path-stem sfx))
      (define filename (path-basename-as-string path))
      (define basename (path-basename-only-as-string filename))
      (define hh-incl 
        (annoless Include 'user 
                  (path-basename-as-string 
                   (path-add-suffix path-stem (get-suffix 'hh)))))
      (define c-unit
        (append (if (memq 'hh kinds) (list hh-incl) null)
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
        (newline))))
     ((eq? kind 'hh)
      (define sfx (get-suffix kind))
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
        (newline))))
     (else
      (raise-argument-error
       'generate-cxx-file
       "set of 'cc or 'hh"
       kinds))))
  (void))
