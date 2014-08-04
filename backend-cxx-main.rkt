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

(define (string->internal-cxx-id s #:default [default #f])
  (set! s (string-underscorify s))
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (translate-id-string s))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  (if (and default (= (string-length s) 0))
      default s))

;;; 
;;; C++ renaming
;;; 

;; Renames Racket IDs to legal C++ symbols. Tracks renamings using a
;; map. Does fairly "stable" renaming by limiting the context of
;; locals to the function body.
(define (cxx-rename ast-lst)
  ;;(pretty-print ast-lst) (exit)
  
  ;; We use (next-gensym r sym) with this to do numbering. All the
  ;; global names are in this table by Id-name. When visiting function
  ;; bodies we update functionally.
  (define r #hasheq())
  
  ;; Id-bind to renamed symbol mappings are stored here.
  (define id->sym (make-hasheq))

  ;; More than one binding may get the same name, but each binding is
  ;; unique.
  (define (record-cxx-name! id n-sym)
    (define bind (Id-bind id))
    (when (hash-has-key? id->sym bind)
      (raise-assertion-error
       'record-cxx-name!
       "C++ name already recorded for ~s: ~s"
       id (hash->list id->sym)))
    (hash-set! id->sym bind n-sym))

  (define (lookup-cxx-name id)
    (hash-ref id->sym (Id-bind id) #f))
  
  ;; We must collect all top-level IDs (and decide on their C++ name)
  ;; before renaming any locals.
  (for-each
   (lambda (ast)
     (match ast
       ((? CxxDefun?)
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
        (record-cxx-name! id n-sym))
       (_ (void))))
   ast-lst)

  ;; Decides name for an ID binding. Returns (values r sym).
  (define (decide-name-for-id r id stem)
    (define orig-s (ast-identifier->string id))
    (define cand-s
      (string->internal-cxx-id orig-s #:default stem))
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
  
  ;; Returns (values r ast).
  (define (rw r ast)
    (match ast
      ((Var a id)
       (define sym (get-decision-for id))
       (values r (Var a sym)))
      ((CxxDefun a id m t ps bs)
       (define n-sym (get-decision-for id))
       (define-values (r-dummy n-ast)
         (rw-all r (CxxDefun a n-sym m t ps bs)))
       (values r n-ast))
      ((CxxParam a id t)
       (define-values (n-r n-sym) (decide-name-for-id r id "a"))
       (values n-r (CxxParam a n-sym t)))
      ((DefVar a id t v)
       (define-values (r-1 n-v) (rw r v))
       (define-values (r-2 n-sym) (decide-name-for-id r-1 id "v"))
       (values r-2 (DefVar a n-sym t n-v)))
      ((CxxDeclVar a id t)
       (define-values (r-2 n-sym) (decide-name-for-id r id "v"))
       (values r-2 (CxxDeclVar a n-sym t)))
      ((GccLabelDecl a id)
       (define-values (n-r n-sym) (decide-name-for-id r id "l"))
       (values n-r (GccLabelDecl a n-sym)))
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
    ((CxxDefun a id m t ps bs)
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
       (CxxDefun a id m t ps bs))
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
       (CxxDefun a id null (type->cxx (FunT-rt t))
                 (map def->cxx ps)
                 (if foreign?
                     null
                     (list (annoless CxxReturnOne (expr->cxx b)))))]
      [(Param a id t)
       (CxxParam a id (annoless RefT (annoless ConstT (type->cxx t))))]
      [(DefVar a id t v)
       (DefVar a id (type->cxx t) (expr->cxx v))]
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
       (BlockStat a (list (annoless Assign
                                    (annoless Var (cdr tgt))
                                    (expr->cxx e))
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
       (define cxx-t (type->cxx t))
       (define lbl (fresh-ast-identifier 'b))
       (define rval (fresh-ast-identifier 'r))
       (define n-ss
         (parameterize ((b-tgt (cons lbl rval)))
           (map stat->cxx ss)))
       (GccStatExpr
        a
        (append (list
                 (annoless GccLabelDecl lbl)
                 (annoless CxxDeclVar rval cxx-t))
                n-ss
                (list (annoless CxxLabel lbl)))
        (annoless Var rval))]
      [(Literal a d)
       (Literal a (syntax->datum d))]
      [_
       (raise-argument-error
        'expr->cxx "supported Expr?" ast)]))
  
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
  
  (filter
   values
   (map
    def->cxx
    (filter
     Defun?
     (hash-values defs-t)))))

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

(define (cxx-decl-sort lst)
  (sort lst symbol<? #:key Def-id))

(define (cxx->pp lst)
  (define (s->ss ast)
    (cond
     ((BlockStat? ast) (BlockStat-ss ast))
     (else ast)))
  (define f
    (topdown
     (lambda (ast)
       (match ast
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
  (define def-lst
    (thread1->
     defs
     defs-lift-locals
     defs->cxx
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
