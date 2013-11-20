#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt"
         "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt")

;;; 
;;; Scheme compatibility
;;; 

(require compatibility/mlist)

;; A recursive version of list->mlist.
(define (list->mlist/rec x)
  (if (list? x)
      (apply mlist (map list->mlist/rec x))
      x))

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

(define (ast->ew ast)
  (match ast
    ((Defun a id ps b)
     (define export? (hash-ref a 'export #f))
     ;; xxx need EW to support a modifier: "static" for locals, "MGL_API" for exports, and "MGLI_FUNC" for top-level internals (Lua-inspired naming)
     (define modif (if export? "MGL_API" "MGLI_FUNC"))
     `(func int ,(syntax-e id) ,(map ast->ew ps) ,(ast->ew b)))
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
       (define incl `(include ,(string-append basename "_config.hpp"))) ;; xxx for now, until we infer required includes
       (define sexp (cons incl (defs->ew defs))) ;; xxx not this simple, we actually also need type decl and prototype sections also
       (define msexp (list->mlist/rec sexp))
       (define s (format-c msexp))
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
