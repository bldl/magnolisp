#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt"
         "backend-util.rkt"
         "compiler-rewrites.rkt" "app-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt"
         racket/syntax syntax/id-table)

;;; 
;;; reformatting
;;; 

(require "util/system.rkt")

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

;;; 
;;; C++ identifiers
;;; 

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
    (define orig-s (ast-identifier->string id))
    (define cand-s (string->internal-cxx-id orig-s #:default stem))
    (define-values (n-r n-sym) (next-gensym r (string->symbol cand-s)))
    (record-cxx-name! id n-sym)
    (values n-r n-sym))

  ;; Gets decided name for an ID reference.
  (define (get-decision-for id)
    (define sym (lookup-cxx-name id))
    (unless sym
      (raise-assertion-error
       'cxx-rename
       "expected C++ name to have been decided for ~s" id))
    sym)
  
  ;; We must collect all top-level IDs (and decide on their C++ name)
  ;; before renaming any locals.
  (for-each
   (lambda (ast)
     (match ast
       [(? CxxDefun?)
        (define a (Ast-annos ast))
        (define id (Def-id ast))
        (define export-name (get-export-name a))
        (define foreign-name (get-foreign-name a))
        (define orig-s (ast-identifier->string id))
        (when-let owner-id (hash-ref a 'owner-id #f)
          (set! orig-s (string-append (ast-identifier->string owner-id)
                                      "_" orig-s)))
        (define (use-or-make name)
          (and name
               (if (identifier? name)
                   (symbol->string (syntax-e name))
                   (string->exported-cxx-id orig-s))))
        (define cand-s
          (or (use-or-make export-name)
              (use-or-make foreign-name)
              (string->internal-cxx-id orig-s #:default "f")))
        (define n-sym (string->symbol cand-s))
        (set!-values (r n-sym) (next-gensym r n-sym))
        (record-cxx-name! id n-sym)]
       [_ (void)]))
   ast-lst)

  ;; Returns (values r ast).
  (define (rw r ast)
    (match ast
      ((Var a id)
       (define sym (get-decision-for id))
       (values r (Var a sym)))
      ((CxxDefun a id m t ps b)
       (define n-sym (get-decision-for id))
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
      ((LabelDecl a id)
       (define-values (r-1 n-sym) (decide-name-for-id r id "l"))
       (values r-1 (LabelDecl a n-sym)))
      ((LabelDef a id)
       (define sym (get-decision-for id))
       (values r (LabelDef a sym)))
      ((Goto a id)
       (define sym (get-decision-for id))
       (values r (Goto a sym)))
      (_
       (rw-all r ast))))

  ;; Rewrites subterms of 'ast', updating 'r' in the process.
  (define (rw-all r ast)
    (let ((ast
           (all-rw-term
            (lambda (ast)
              (let-values (((sub-r ast) (rw r ast)))
                (set! r sub-r)
                ast))
            ast)))
      (values r ast)))

  (define (rw-drop r ast)
    (let-values (((r ast) (rw r ast)))
      ast))

  (define rw-type-refs
    (topdown
     (lambda (ast)
       (match ast
         ((ForeignNameT a id)
          (CxxNameT a (syntax-e id)))
         (_ ast)))))

  (set! ast-lst (map (fix rw-drop r) ast-lst))
  (set! ast-lst (map rw-type-refs ast-lst))
  ;;(writeln ast-lst)
  ast-lst)

(define (cxx-rm-LabelDecl ast-lst)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(SeqCont ss)
          #:when (ormap LabelDecl? ss)
          (SeqCont-copy ast (filter (negate LabelDecl?) ss))]
         [_ ast]))))
  
  (map rw ast-lst))

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
  ;; Tracks surrounding LetLocalEc scopes, by mapping continuation
  ;; bind -> (cons/c label-id var-id).
  (define le-tgt (make-parameter #hasheq()))
  
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
      [(LetLocalEc a (Var _ k) ss)
       (define t (Expr-type ast))
       (define void-t? (equal? t the-Void-type))
       (define lbl-id (fresh-ast-identifier 'b))
       (define rv-id (and (not void-t?)
                          (fresh-ast-identifier 'r)))
       (define tgt (cons lbl-id rv-id))
       (define n-ss
         (parameterize ((le-tgt (hash-set (le-tgt) (Id-bind k) tgt)))
           (map expr->cxx ss)))
       (define es 
         `(,(annoless LabelDecl lbl-id)
           ,@n-ss
           ,(annoless LabelDef lbl-id)))
       (if void-t?
           (SeqStat a es)
           (LiftStatExpr a rv-id es))]
      [(AppLocalEc a (Var _ k) e)
       (define tgt (hash-ref (le-tgt) (Id-bind k) #f))
       (unless tgt
         (raise-language-error/ast
          "local escape out of context"
          ast (AppLocalEc-k ast)))
       (define lbl-id (car tgt))
       (define rv-id (cdr tgt))
       (define n-e (expr->cxx e))
       (define n-ast
         (if rv-id
             (SeqStat a 
                      (list (annoless AssignStat (annoless Var rv-id) n-e)
                            (annoless Goto lbl-id)))
             (annoless Goto lbl-id)))
       ;;(writeln n-ast)
       n-ast]
      [_
       (raise-argument-error
        'expr->cxx "supported ExprLike?" ast)]))

  (define def-lst (filter Defun? (hash-values defs-t)))
  ;;(pretty-print def-lst) (exit)
  (set! def-lst (map def->cxx def-lst))
  ;;(pretty-print def-lst)
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
       (all-rw-term to-expr ast)]
      [(SeqExpr a es)
       (define t (Expr-type ast))
       (define void-t? (equal? t the-Void-type))
       (define id (fresh-ast-identifier 'lifted))
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
       (annoless ExprStat (all-rw-term to-expr ast))]
      [(or (? AssignStat?) (? ReturnStat?))
       (all-rw-term to-expr ast)]
      [(DefVar a id t b)
       (DefVar a id t (to-expr b))]
      [(? SeqStat?)
       (all-rw-term to-stat ast)]
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
      [(or (? Goto?) (? DeclVar?) (? NoBody?) (? Label?))
       ast]
      [_
       ;;(writeln `(result-discarded = ,(get-result-discarded ast)))
       (raise-argument-error
        'to-stat "supported ExprLike? or Def?" ast)]))
  
  (let ((s (CxxDefun-s def)))
    (set-CxxDefun-s def (to-stat s))))

(define (types-to-cxx defs-t def-lst)
  (define (type->cxx ast)
    (match ast
      [(NameT _ id)
       (define def (ast-identifier-lookup defs-t id))
       (unless def
         (raise-language-error/ast
          "reference to unbound type ID"
          ast id))
       (match def
         ((ForeignTypeDecl _ _ cxx-t)
          cxx-t))]
      [_
       (raise-argument-error
        'type->cxx "supported Type?" ast)]))
  
  (define rw-def
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
          (DefVar a id (type->cxx t) v)]
         [(DeclVar a id t)
          (DeclVar a id (type->cxx t))]
         [_ ast]))))
  
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
    ((topdown-visit
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
                           (another-ast-identifier (Var-id ast))
                           (fresh-ast-identifier 'lifted)))
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
           (define if-id (fresh-ast-identifier 'lifted))
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
  
  (define (annos-add-val-num! a lv-id [rv #f])
    (define val-num (gensym 'vn))
    (define n-a (hash-set a 'val-num val-num))
    (when (and rv (Var? rv))
      (define item (list val-num lv-id rv))
      ;;(writeln `(potential ,item))
      (set! to-examine (cons item to-examine)))
    n-a)
  
  ;; Assigns value numbers to each "assignment", and adds potentials
  ;; for removal to `to-examine`. Note that a `DeclVar` has no value.
  (define assign-val-nums
    (topdown
     (lambda (ast)
       (match ast
         [(AssignStat a lv rv)
          (define n-a (annos-add-val-num! a (Var-id lv) rv))
          (AssignStat n-a lv rv)]
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
    
    (define tgt-num (first examine-item))
    (define tgt-id (second examine-item))
    (define tgt-bind (Id-bind tgt-id))
    (define tgt-rv (third examine-item))
    
    ;; For each Goto target (indexed by `bind` value), a sum of their
    ;; bind->val assignments. All Gotos to a LabelDef will have been
    ;; seen before we reach the LabelDef, and hence the set of
    ;; assignments will be complete at that point.
    (define label->bind->val (make-hasheq))
    
    (define (merge h1 h2)
      (define keys (list->mutable-seteq (hash-keys h1)))
      (for ((k (hash-keys h2)))
        (set-add! keys k))
      (for/fold ((h #hasheq())) ((k (in-set keys)))
        (define v1 (hash-ref h1 k 'nothing))
        (define v2 (hash-ref h2 k 'nothing))
        (hash-set h k (val-num+ v1 v2))))
    
    (define (rw bind->val ast)
      ;;(writeln `(rw of ,ast))
      (match ast
        [(AssignStat a lv rv)
         (define this-num (hash-ref a 'val-num))
         (assert (Var? lv))
         (define lv-id (Var-id lv))
         (define lv-bind (Id-bind lv-id))
         (values (hash-set bind->val lv-bind this-num)
                 (cond
                  [(and (eq? this-num tgt-num)
                        (ast-identifier=? tgt-id lv-id))
                   ;;(writeln `(deleting ,ast))
                   a-noop]
                  [else
                   (define n-rv (rw-discard bind->val rv)) 
                   (and n-rv (AssignStat a lv n-rv))]))]
        [(DefVar a id t rv)
         (define this-num (hash-ref a 'val-num))
         (define lv-bind (Id-bind id))
         (values (hash-set bind->val lv-bind this-num)
                 (cond
                  [(eq? this-num tgt-num)
                   (DeclVar a id t)]
                  [else
                   (define n-rv (rw-discard bind->val rv))
                   (and n-rv (DefVar a id t n-rv))]))]
        [(Param a id t)
         (define this-num (hash-ref a 'val-num))
         (assert (not (eq? this-num tgt-num)))
         (define lv-bind (Id-bind id))
         (values (hash-set bind->val lv-bind this-num) ast)]
        [(IfStat a c t e)
         (define n-c (rw-discard bind->val c))
         (define-values (t-st n-t) (rw bind->val t))
         (if (not n-t)
             (values bind->val #f)
             (let ()
               (define-values (e-st n-e) (rw bind->val e))
               (if (not n-e)
                   (values bind->val #f)
                   (values (merge t-st e-st)
                           (IfStat a n-c n-t n-e)))))]
        [(Var a (? (lambda (id) (ast-identifier=? id tgt-id)) id))
         (define this-bind (Id-bind id))
         (define this-num (hash-ref bind->val this-bind))
         (assert (not (eq? this-num 'nothing)))
         ;;(writeln `(Var id= ,id num= ,this-num))
         (values bind->val
                 (cond
                  ;; The target assignment is in effect here.
                  ((eq? this-num tgt-num)
                   tgt-rv)
                  ;; The target assignment might be in effect here, but we
                  ;; don't know, and hence cannot make this optimization.
                  ((Phi? this-num)
                   (define vs (Phi-set this-num))
                   (if (set-member? vs tgt-num)
                       #f
                       ast))
                  ;; The target assignment is not in effect here.
                  (else
                   ast)))]
        [(Goto _ id)
         (define bind (Id-bind id))
         (define lbl-vals 
           (merge (hash-ref label->bind->val bind #hasheq()) bind->val))
         (hash-set! label->bind->val bind lbl-vals)
         (values bind->val ast)]
        [(LabelDef _ id)
         (define bind (Id-bind id))
         (define lbl-vals 
           (merge 
            ;; any value assignments from Gotos to this label
            (hash-ref label->bind->val bind #hasheq()) 
            ;; the "fall-in" value assignments
            bind->val))
         (values lbl-vals ast)]
        [_
         (rw-all bind->val ast)]))
    
    (define (rw-discard bind->val ast)
      (define-values (r n-ast) (rw bind->val ast))
      n-ast)
    
    (define (rw-all bind->val ast)
      (stateful-all-rw-term rw bind->val ast))
    
    (let-values ([(dummy ast) (rw #hasheq() def)])
      ;;(pretty-print ast)
      ast))
  
  ;; Tries to rewrite `ast` to remove each of the assignments in
  ;; `to-examine`, but preserves `ast` where removal is not possible.
  (define (examine-all ast)
    (for/fold ((ast ast)) ((item to-examine))
      (or (rw-by-item item ast) ast)))
  
  (examine-all (assign-val-nums def)))

;;; 
;;; removal of unreferenced variables
;;; 

(define (fun-rm-unreferenced-var-decl def)
  (define refs (mutable-seteq))
  
  ((topdown-visit
    (lambda (ast)
      (when (Var? ast)
        (define id (Var-id ast))
        (define bind (Id-bind id))
        (set-add! refs bind))))
   def)
  
  ((topdown
    (lambda (ast)
      (match ast
        [(DeclVar a id t)
         (define bind (Id-bind id))
         (if (set-member? refs bind)
             ast
             a-noop)]
        [_ ast])))
   def))

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
       (values (and st0 st1 (ast-identifier=? st0 st1) st0)
               (IfStat a c n-t n-e))]
      [(LabelDef a id) 
       ;;(writeln `(store ,id))
       (values id s)]
      [(Goto _ (? (lambda (id) (and st (ast-identifier=? st id)))))
       ;;(writeln `(delete ,s))
       (values st a-noop)]
      [(LabelDecl _ _)
       (values st s)]
      [_ 
       (values #f s)]))
  
  (define (stat-rm-goto-next s)
    (define-values (st n-s) (g #f s))
    n-s)
  
  (define (stat-rm-unused-labels s)
    ;; The `targets` set is that of label `bind` values in `s` that
    ;; are Goto targets.
    (define targets (mutable-seteq))
    ((topdown-visit
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
          [(Label (? (lambda (id)
                          (define bind (Id-bind id))
                          (not (set-member? targets bind)))))
           a-noop]
          [_ ast])))
     s))
  
  (define (defun-optimize ast)
    (define s (CxxDefun-s ast))
    (set! s (stat-rm-goto-next s))
    (set! s (stat-rm-unused-labels s))
    ;;(pretty-print `(,(CxxDefun-id ast) before ,s))
    (set! s (fun-propagate-copies s))
    ;;(pretty-print `(,(CxxDefun-id ast) after ,s))
    (set! s (fun-rm-unreferenced-var-decl s))
    (set-CxxDefun-s ast s))
  
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
    (unless (ast-identifier=? id oid)
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

(require "backend-cxx-print.rkt")

(define (get-suffix kind)
  (define tbl `((cc ".cpp")
                (hh ".hpp")))
  (define p (assq kind tbl))
  (unless p
    (raise-argument-error 'get-suffix
                          "either 'cc or 'hh" kind))
  (values (second p)))

(define-with-contract*
  (-> (listof symbol?) hash? path-string? (or/c #f output-port?)
      boolean? void?)
  (generate-cxx-file kinds defs path-stem out banner?)

  (define defs-t
    (defs-lift-locals defs))
  
  (define (pp-only obj)
    (pretty-print obj)
    obj)
  
  (define (pp-exit obj)
    (pretty-print obj)
    (exit))
  
  (define def-lst
    (thread1->
     defs-t
     defs->cxx
     cxx-rm-LiftStatExpr
     ;;pp-only
     cxx-fun-optimize
     ;;pp-exit
     (curry map ast-rm-SeqStat)
     (curry types-to-cxx defs-t)
     cxx-rename
     cxx-rm-LabelDecl
     cxx-decl-sort
     cxx->pp))
  
  (for ((kind kinds))
    (cond
     ((eq? kind 'cc)
      (define sfx (get-suffix kind))
      (define path (path-add-suffix path-stem sfx))
      (define filename (path-basename-as-string path))
      (define basename (path-basename-only-as-string filename))
      (define hh-incl (annoless Include 'user (path-basename-as-string (path-add-suffix path-stem (get-suffix 'hh)))))
      (define c-unit
        (append (list hh-incl)
                (defs->partition 'private-prototypes def-lst)
                (defs->partition 'private-implementations def-lst)))
      ;;(for-each writeln c-unit) (exit)
      (define s (format-c c-unit))
      ;; xxx uncrustify
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
      ;; xxx uncrustify
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
