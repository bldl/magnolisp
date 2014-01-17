#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt"
         "backend-util.rkt"
         "compiler-util.rkt" "strategy.rkt"
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
;;; C++ renaming
;;; 

;; Trickery to generate fresh, unique IDs for the purposes of the
;; backend. Should only be used in contexts where def-id is used for
;; comparison. The 'stem' argument must be a symbol.
(define (fresh-id stem)
  (syntax-property
   (datum->syntax #f stem)
   'def-id
   (generate-temporary)))

;; Renames Racket IDs to legal C++ symbols. Tracks renamings using a
;; map. Does fairly "stable" renaming by limiting the context of
;; locals to the function body.
(define (cxx-rename ast-lst)
  ;;(pretty-print ast-lst)
  
  ;; We use (next-gensym r sym) with this to do numbering. All the
  ;; global names are in this table. When visiting function bodies we
  ;; update functionally.
  (define r #hasheq())
  ;; ID to symbol mappings are stored here. Handled similarly to the
  ;; 'r' table. Use def-id to look up symbols from this table.
  (define id->sym (make-immutable-free-id-table #:phase 0))

  ;; We must collect all top-level IDs (and decide on their C++ name)
  ;; before renaming any locals.
  (for-each
   (lambda (ast)
     (match ast
       ((? CxxDefun?)
        (define a (Ast-annos ast))
        (define id (Def-id ast))
        (assert (not (dict-has-key? id->sym id)))
        (define export-name (get-export-name a))
        (define foreign-name (get-foreign-name a))
        (define orig-sym (syntax-e id))
        (define orig-s (symbol->string orig-sym))
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
        (set! id->sym (dict-set id->sym id n-sym)))
       (_ (void))))
   ast-lst)

  ;; Returns (values r sym).
  (define (decide-name-for-id r id stem)
    (define def-id (or (get-def-id id) id))
    (when (dict-has-key? id->sym def-id)
      (raise-assertion-error
       'decide-name-for-id
       "dictionary already has key ~s: ~s"
       def-id (dict->list id->sym)))
    (define orig-sym (syntax-e id))
    (define orig-s (symbol->string orig-sym))
    (define cand-s
      (string->internal-cxx-id orig-s #:default stem))
    (define-values (n-r n-sym) (next-gensym r (string->symbol cand-s)))
    (set! id->sym (dict-set id->sym def-id n-sym))
    (values n-r n-sym))

  (define (get-decision-for id [def-id (get-def-id id)])
    (define sym (dict-ref id->sym def-id #f))
    (unless sym
      (raise-assertion-error
       'cxx-rename
       "expected C++ name to have been decided for ~a" (syntax-e id)))
    sym)
  
  ;; Returns (values r ast).
  (define (rw r ast)
    (match ast
      ((Var a id)
       ;; The name for any binding should already have been decided
       ;; upon. And at this late stage any references to built-in
       ;; names should have been translated as well.
       (define def-id (get-def-id id))
       (unless def-id
         (error 'cxx-rename "unbound variable ~a: ~s"
                (syntax-e id) id))
       (define sym (get-decision-for id def-id))
       (values r (Var a sym)))
      ((CxxDefun a id m t ps bs)
       (define n-sym (dict-ref id->sym id))
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
           (subterm-all
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
         ((CxxNameT a id)
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
  
  (define (ast->cxx ast)
    ;;(writeln ast)
    (match ast
      ((Defun a id t ps b)
       (define foreign? (and (get-foreign-name a) #t))
       (CxxDefun a id null (ast->cxx (FunT-rt t))
                 (map ast->cxx ps)
                 (if foreign?
                     null
                     (list (annoless CxxReturnOne (ast->cxx b))))))
      ((Param a id t)
       (CxxParam a id (annoless RefT (annoless ConstT (ast->cxx t)))))
      ((DefVar a id t v)
       (DefVar a id (ast->cxx t) (ast->cxx v)))
      ((Let a dvs ss)
       (BlockStat a (map ast->cxx (append dvs ss))))
      ((? Var?)
       ast)
      ((Apply a f es) ;; xxx need to deal with operators and parenthesization
       (Apply a f (map ast->cxx es)))
      ((Return a e)
       (define tgt (b-tgt))
       (unless tgt
         (raise-language-error/ast
          "return without surrounding block expression"
          ast))
       (BlockStat a (list (annoless Assign
                                    (annoless Var (cdr tgt))
                                    (ast->cxx e))
                          (annoless Goto (car tgt)))))
      ((BlockExpr a ss)
       (define t (expr-get-type ast))
       (define cxx-t (ast->cxx t))
       (define lbl (fresh-id 'b))
       (define rval (fresh-id 'r))
       (define n-ss
         (parameterize ((b-tgt (cons lbl rval)))
           (map ast->cxx ss)))
       (GccStatExpr
        a
        (append (list
                 (annoless GccLabelDecl lbl)
                 (annoless CxxDeclVar rval cxx-t))
                n-ss
                (list (annoless CxxLabel lbl)))
        (annoless Var rval)))
      ((Literal a d)
       (Literal a (syntax->datum d)))
      ((NameT _ id)
       (define def-id (get-def-id id))
       (unless def-id
         (raise-language-error/ast
          "reference to unbound type ID"
          ast id))
       (define def (dict-ref defs-t def-id))
       (match def
         ((ForeignTypeDecl _ _ cxx-t)
          cxx-t)))
      (else
       (raise-argument-error
        'ast->cxx "supported Ast?" ast))))
  
  (filter
   values
   (map
    ast->cxx
    (filter
     Defun?
     (dict-values defs-t)))))

(define (cxx-decl-sort lst)
  (sort lst symbol<? #:key Def-id))

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

(define* (generate-cxx-file kinds defs path-stem stdout? banner?)
  (define def-lst (cxx-decl-sort (cxx-rename (defs->cxx defs))))
  (set-for-each
   kinds
   (lambda (kind)
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
        path stdout?
        (thunk
         (when banner?
           (display-banner "//" filename))
         (display-generated-notice "//")
         (display s))))
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
        path stdout?
        (thunk
         (when banner?
           (display-banner "//" filename))
         (display-generated-notice "//")
         (display s))))
      (else
       (raise-argument-error
        'generate-cxx-file
        "set of 'cc or 'hh"
        kinds)))))
  (void))
