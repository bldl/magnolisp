#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt"
         "backend-util.rkt"
         "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt"
         syntax/id-table)

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

(define (string->exported-cxx-id o-s)
  (define s o-s)
  (set! s (regexp-replace #rx"[!?=]+$" s ""))
  (set! s (string-underscorify s))
  (unless (string-cxx-id? s)
    (error
     'string->exported-cxx-id
     "illegal name for a C++ export: ~s" o-s))
  s)

(define (string->internal-cxx-id s #:default [default #f])
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  (if (and default (= (string-length s) 0))
      default s))

;; Renames Racket IDs to legal C++ symbols. Tracks renamings using a
;; map. Does fairly "stable" renaming by limiting the context of
;; locals to the function body.
(define (cxx-rename ast-lst)
  ;; We use (next-gensym r sym) with this to do numbering. All the
  ;; global names are in this table. When visiting function bodies we
  ;; update functionally.
  (define r #hasheq())
  ;; ID to symbol mappings are stored here. Handled similarly to the
  ;; 'r' table.
  (define id->sym (make-immutable-free-id-table))

  ;; We must collect all top-level IDs (and decide on their C++ name)
  ;; before renaming any locals.
  (for-each
   (lambda (ast)
     (match ast
       ((? CxxDefun?)
        (define a (Ast-annos ast))
        (define id (Def-id ast))
        (assert (not (dict-has-key? id->sym id)))
        (define export? (hash-ref a 'export #f))
        (define orig-sym (syntax-e id))
        (define orig-s (symbol->string orig-sym))
        (define cand-s
          (if export?
              (string->exported-cxx-id orig-s)
              (string->internal-cxx-id orig-s #:default "f")))
        (define n-sym (string->symbol cand-s))
        (set!-values (r n-sym) (next-gensym r n-sym))
        (set! id->sym (dict-set id->sym id n-sym)))
       (_ (void))))
   ast-lst)

  ;; Returns (values r ast).
  (define (rw r ast)
    (match ast
      ((Var a id)
       ;; The name for any variable reference should also have been
       ;; decided upon. And at this late stage any references to
       ;; built-in names should have been replaced as well.
       (define sym (dict-ref id->sym id #f))
       (unless sym
         (error 'cxx-rename "unbound variable ~a (~s)"
                (syntax-e id) id))
       (values r (Var a sym)))
      ((CxxDefun a id m t ps bs)
       (define n-sym (dict-ref id->sym id))
       (define-values (r- n-ast)
         (rw-all r (CxxDefun a n-sym m t ps bs)))
       (values r n-ast))
      ((CxxParam a t id)
       (assert (not (dict-has-key? id->sym id)))
       (define orig-sym (syntax-e id))
       (define orig-s (symbol->string orig-sym))
       (define cand-s
         (string->internal-cxx-id orig-s #:default "a"))
       (define-values (n-r n-sym) (next-gensym r (string->symbol cand-s)))
       (set! id->sym (dict-set id->sym id n-sym))
       (values n-r (CxxParam a t n-sym)))
      (else
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
  
  (map (fix rw-drop r) ast-lst))

;;; 
;;; C++ translation
;;; 

(define (def-type ast)
  (annoless NameT "int"))

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
     ;; xxx 'foreign functions are always just dropped (no proto either)
     (define export? (hash-ref a 'export #f))
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

(define (ast->cxx ast)
  ;;(writeln ast)
  (match ast
    ((Defun a id ps b)
     (define export? (hash-ref a 'export #f))
     (CxxDefun a id null (def-type ast)
               (map ast->cxx ps)
               (list (annoless CxxReturnOne (ast->cxx b)))))
    ((Param a id)
     (CxxParam a (def-type ast) id))
    ((Var a id)
     ast)
    ((Apply a f es) ;; xxx need to deal with operators and parenthesization
     (Apply a f (map ast->cxx es)))
    ((Literal a d)
     (Literal a (syntax->datum d)))
    (else
     (unsupported ast))))

(define (defs->cxx def-lst)
  (filter
   values
   (map
    ast->cxx
    (filter
     (negate Param?)
     def-lst))))

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
  (define def-lst (cxx-rename (defs->cxx (dict-values defs))))
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
         (display s))))
      (else
       (raise-argument-error
        'generate-cxx-file
        "set of 'cc or 'hh"
        kinds)))))
  (void))
