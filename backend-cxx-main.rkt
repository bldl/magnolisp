#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt"
         "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt")

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
;;; Elegant Weapons translation
;;; 

(require "backend-cxx-print.rkt")

(define (def-type ast)
  (annoless TypeName "int"))

(define (id->ew id)
  (symbol->string (syntax-e id)))

(define (ast->ew ast)
  ;;(writeln ast)
  (match ast
    ((Defun a id ps b)
     (define export? (hash-ref a 'export #f))
     ;; xxx need EW to support a modifier: "static" for locals, "MGL_API" for exports, and "MGLI_FUNC" for top-level internals (Lua-inspired naming)
     (define modif (if export? "MGL_API" "MGLI_FUNC"))
     (CxxDefun a (id->ew id)
               (list modif) (def-type ast)
               (map ast->ew ps)
               (list (annoless CxxReturnOne (ast->ew b)))))
    ((Param a id)
     (CxxParam a (def-type ast) (id->ew id)))
    ((Var a id)
     (Var a (id->ew id)))
    ((Apply a (Var va f) es) ;; xxx need to deal with operators and parenthesization
     (Apply a (Var va (id->ew f)) (map ast->ew es)))
    ((Literal a d)
     (Literal a (syntax->datum d)))
    (else
     (unsupported ast))))

(define (defs->ew defs)
  (map
   ast->ew
   (filter
    (negate Param?)
    (dict-values defs))))

;;; 
;;; driver routines
;;; 

(require "backend-util.rkt")

(define (get-suffix kind)
  (define tbl `((cc ".cpp")
                (hh ".hpp")))
  (define p (assq kind tbl))
  (unless p
    (raise-argument-error 'get-suffix
                          "either 'cc or 'hh" kind))
  (values (second p)))

(define* (generate-cxx-file kinds defs path-stem stdout? banner?)
  (set-for-each
   kinds
   (lambda (kind)
     (cond
      ((eq? kind 'cc)
       (define sfx (get-suffix kind))
       (define path (path-add-suffix path-stem sfx))
       (define filename (path-basename-as-string path))
       (define basename (path-basename-only-as-string filename))
       (define incl (annoless Include 'user (string-append basename "_config.hpp"))) ;; xxx for now, until we infer required includes
       (define c-unit (cons incl (defs->ew defs))) ;; xxx not this simple, we actually also need type decl and prototype sections also
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
       ;; xxx 'hh to be supported
       (unsupported kind)))))
  (void))
