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
      ((CxxDeclVar a id t)
       (define-values (r-2 n-sym) (decide-name-for-id r id "v"))
       (values r-2 (CxxDeclVar a n-sym t)))
      ((CxxLabelDecl a id)
       (define-values (r-1 n-sym) (decide-name-for-id r id "l"))
       (values r-1 (CxxLabelDecl a n-sym)))
      ((CxxLabel a id)
       (define sym (get-decision-for id))
       (values r (CxxLabel a sym)))
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

(define (cxx-rm-CxxLabelDecl ast-lst)
  (define rw
    (topdown
     (lambda (ast)
       (match ast
         [(StatCont ss)
          #:when (ormap CxxLabelDecl? ss)
          (StatCont-copy ast (filter (negate CxxLabelDecl?) ss))]
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
  ;; When within a BlockExpr, this is (cons/c label-id var-id).
  (define b-tgt (make-parameter #f))
  
  (define (def->cxx ast)
    (match ast
      [(Defun a id t ps b)
       (define foreign? (and (get-foreign-name a) #t))
       (CxxDefun a id null t
                 (map def->cxx ps)
                 (if foreign?
                     the-NoBody
                     (annoless CxxReturnOne (expr->cxx b))))]
      [(? Param?)
       ast]
      [(DefVar a id t v)
       (DefVar a id t (expr->cxx v))]
      [_
       (raise-argument-error
        'def->cxx "supported Def?" ast)]))
  
  (define (stat->cxx ast)
    (match ast
      [(IfStat a c t e)
       (IfStat a (expr->cxx c) (stat->cxx t) (stat->cxx e))]
      [(BlockStat a ss)
       (BlockStat a (map stat->cxx ss))]
      [(Return a e)
       (define tgt (b-tgt))
       (unless tgt
         (raise-language-error/ast
          "return without surrounding block expression"
          ast))
       (define n-e
         (parameterize ((b-tgt #f))
           (expr->cxx e)))
       (BlockStat a (list (annoless Assign (annoless Var (cdr tgt)) n-e)
                          (annoless Goto (car tgt))))]
      [(LetStat a dv ss)
       (BlockStat a (cons (def->cxx dv) (map stat->cxx ss)))]
      [(Assign a lhs rhs)
       (Assign a (expr->cxx lhs) (expr->cxx rhs))]
      [_
       (raise-argument-error
        'stat->cxx "supported Stat?" ast)]))
  
  (define (expr->cxx ast)
    (match ast
      [(? Var?)
       ast]
      [(Apply a f es)
       (Apply a f (map expr->cxx es))]
      [(IfExpr a c t e)
       (IfExpr a (expr->cxx c) (expr->cxx t) (expr->cxx e))]
      [(BlockExpr a ss)
       (define t (Expr-type ast))
       (define lbl (fresh-ast-identifier 'b))
       (define rval (fresh-ast-identifier 'r))
       (define n-ss
         (parameterize ((b-tgt (cons lbl rval)))
           (map stat->cxx ss)))
       (LiftStatExpr
        a rval
        `(,(annoless CxxLabelDecl lbl)
          ,@n-ss
          ,(annoless CxxLabel lbl)))]
      [(Literal a d)
       (Literal a (syntax->datum d))]
      [_
       (raise-argument-error
        'expr->cxx "supported Expr?" ast)]))
  
  (filter
   values
   (map
    def->cxx
    (filter
     Defun?
     (hash-values defs-t)))))

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
         [(CxxDeclVar a id t)
          (CxxDeclVar a id (type->cxx t))]
         [_ ast]))))
  
  (map rw-def def-lst))

;;; 
;;; statement unsplicing
;;; 

(define (ast-rm-SpliceStat ast)
  (define un-nest
    (bottomup
     (lambda (ast)
       (match ast
         [(StatCont (? (curry ormap SpliceStat?) ss))
          (define n-ss
            (apply append (for/list ((s ss))
                            (if (SpliceStat? s)
                                (SpliceStat-ss s)
                                (list s)))))
          (StatCont-copy ast n-ss)]
         [else ast]))))
  
  (define convert
    (bottomup
     (lambda (ast)
       (match ast
         [(SpliceStat a ss)
          (match ss
            [(list s) s]
            [_ (BlockStat a ss)])]
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
  (annoless SpliceStat (append ss (list s))))

(define (def-rm-LiftStatExpr ast)
  ;; Wraps statement `ast` with the specified lifts, returning a new
  ;; statement. The statements `ss` are assumed to be in evaluation
  ;; order. The declarations `ds` should be of type CxxDeclVar.
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

  (define (lift-expr ast [tmp-id #f])
    (unless tmp-id
      (set! tmp-id (if (Var? ast)
                       (another-ast-identifier (Var-id ast))
                       (fresh-ast-identifier 'lifted))))
    (define t (Expr-type ast))
    (define def (rw-stat (annoless DefVar tmp-id t ast)))
    (define ref (Var (hasheq 'type t) tmp-id))
    (values def ref))
  
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
      (define decl (annoless CxxDeclVar id t))
      (define ref (Var (hasheq 'type t) id))
      (values (cons decl ds) (append n-ss ss) ref)]
     [(not (null? ss))
      ;; Since some later subexpressions have been lifted, we must
      ;; lift this one, too.
      (define-values (def ref) (lift-expr ast))
      (values ds (cons def ss) ref)]
     [else
      (match ast
        [(Apply a f as)
         (define-values (as-ds as-ss as-exprs)
           (rw-siblings-in-reverse '() '() as))
         ;; Arguments evaluate before application, and we need not
         ;; lift the function application even if the argument
         ;; expressions contain lifts.
         (values (append as-ds ds)
                 (append as-ss ss)
                 (Apply a f as-exprs))]
        [(IfExpr a c t e)
         ;; If either `t` or `e` contain lifts, we must lift the
         ;; entire conditional, and make it a statement so that we can
         ;; keep the lifts in their branches. If `c` contains lifts,
         ;; we can just lift those. We must lift the entire `c` if
         ;; either branch had lifts.
         (define-values (t-ds t-ss t-ast) (rw-expr t))
         (define-values (e-ds e-ss e-ast) (rw-expr e))
         (cond
          [(and (null? t-ss) (null? e-ss))
           (define-values (c-ds c-ss c-ast) (rw-expr c))
           (values (append c-ds t-ds e-ds ds)
                   (append c-ss ss)
                   (IfExpr a c-ast t-ast e-ast))]
          [else
           (define-values (c-def c-ref) (lift-expr c))
           (define typ (Expr-type ast))
           (define if-id (fresh-ast-identifier 'lifted))
           (define if-decl (annoless CxxDeclVar if-id typ))
           (define if-ref (Var (hasheq 'type typ) if-id))
           (define t-ast (wrap-expr-as-Assign if-ref t))
           (define e-ast (wrap-expr-as-Assign if-ref e))
           (values (cons if-decl ds)
                   (cons c-def ss)
                   (IfStat a c-ref t-ast e-ast))])]
        [_
         ;; The expression `ast` does not contain sub-expressions.
         (values ds ss ast)])]))
  
  (define (rw-expr ast)
    (rw-sibling '() '() ast))

  (define (wrap-expr-as-Assign lv rv)
    (rw-stat (annoless Assign lv rv)))
  
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
      [(Assign a lv rv)
       #:when (has-lifts? rv)
       (define-values (ds ss n-rv) (rw-expr rv))
       (wrap ds ss (Assign a lv n-rv))]
      [(IfStat a c t e)
       #:when (has-lifts? c)
       (define-values (ds ss n-c) (rw-expr c))
       (wrap ds ss (IfStat a n-c t e))]
      [(CxxReturnOne a v)
       #:when (has-lifts? v)
       (define-values (ds ss n-v) (rw-expr v))
       (wrap ds ss (CxxReturnOne a n-v))]
      [_ ast]))
  
  (define rw-all-stats
    (bottomup
     (lambda (ast)
       (if (or (Stat? ast) (DefVar? ast))
           (rw-stat ast)
           ast))))
  
  (rw-all-stats ast))

(define (cxx-rm-LiftStatExpr def-lst)
  (map def-rm-LiftStatExpr def-lst))

;;; 
;;; removal of redundant jumps
;;; 

(define a-noop (annoless BlockStat null))

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
      [(StatCont ss)
       (define-values (n-st n-ss) (map/reverse/state g st ss))
       (values n-st (StatCont-copy s n-ss))]
      [(IfStat a c t e)
       (define-values (st0 n-t) (g st t))
       (define-values (st1 n-e) (g st e))
       ;; We do account for the special case where both branches of an
       ;; IfStat begin with the same label.
       (values (and st0 st1 (ast-identifier=? st0 st1) st0)
               (IfStat a c n-t n-e))]
      [(CxxLabel a id) 
       ;;(writeln `(store ,id))
       (values id s)]
      [(Goto _ (? (lambda (id) (and st (ast-identifier=? st id)))))
       ;;(writeln `(delete ,s))
       (values st a-noop)]
      [(CxxLabelDecl _ _)
       (values st s)]
      [_ 
       (values #f s)]))
  
  (define (stat-rm-goto-next s)
    (define-values (st n-s) (g #f s))
    n-s)
  
  (define (defun-optimize ast)
    (define b (CxxDefun-s ast))
    (define s (stat-rm-goto-next b))
    (set-CxxDefun-s ast s))
  
  (map (lambda (def)
         (if (CxxDefun? def)
             (defun-optimize def)
             def)) def-lst))

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
        [(and (LetStat? ast) (Defun? (LetStat-def ast)))
         (match-define (LetStat a b ss) ast)
         (do-Defun b)
         (BlockStat a ss)]
        [else
         ast]))))
  
  (define (do-Defun ast)
    (match-define (Defun a id t ps b) ast)
    (define oid (owner-id))
    (unless (ast-identifier=? id oid)
      (set! a (hash-set a 'owner-id oid)))
    (set! b (ast-simplify (rw-body b)))
    (hash-set! n-defs (Id-bind id) (Defun a id t ps b)))
  
  (for ([(bind def) defs])
    (cond
     ((Defun? def)
      (when (ast-anno-maybe def 'top)
        (parameterize ((owner-id (Def-id def)))
          (do-Defun def))))
     (else
      (hash-set! n-defs bind def))))
  
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
     ((BlockStat? ast) (BlockStat-ss ast))
     (else ast)))
  (define f
    (topdown
     (lambda (ast)
       (match ast
         [(? CxxDefun?)
          (define b (CxxDefun-s ast))
          (cond
           ((BlockStat? b)
            ast)
           ((NoBody? b) 
            ast)
           (else
            (struct-copy 
             CxxDefun ast 
             [s (annoless BlockStat (list b))])))]
         [(IfStat a c t e)
          (PpCxxIfStat a c (s->ss t) (s->ss e))]
         [_
          ast]))))
  (for/list ((ast lst))
    (f ast)))
  
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
  ;;(pretty-print (defs-id->ast defs)) (exit)
  (define defs-t
    (defs-lift-locals defs))
  (define def-lst
    (thread1->
     defs-t
     defs->cxx
     cxx-rm-LiftStatExpr
     cxx-fun-optimize
     (curry map ast-rm-SpliceStat)
     (curry types-to-cxx defs-t)
     cxx-rename
     cxx-rm-CxxLabelDecl
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
