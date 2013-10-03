#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt" "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt" "util/system.rkt")

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

;;; 
;;; module naming
;;; 

;; We want to pick a name here for a module (path) such that it will
;; be suitable for use in C++. As input, we expect a resolved module
;; path, as such paths should be suitable unique. For consistency, we
;; use capitalized names. Underscores are preserved, although not
;; typically used in Lisps.

(define (string-keep-basic-chars s)
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  s)

(define (path-basename-only fn)
  (define-values (p f dir?) (split-path fn))
  (path-replace-suffix f ""))

(define (resolved-mp->preferred-name mp)
  (match mp
    ((? path?)
     (string->symbol
      (string-titlecase
       (string-keep-basic-chars
        (path->string
         (path-basename-only mp))))))
    ((? symbol?)
     (string->symbol
      (string-titlecase
       (string-keep-basic-chars
        (symbol->string mp)))))
    ((list 'submod sub-mp (? symbol? name) ...)
     ;; Might also be (list 'submod sub-mp (? symbol? name) ...), but
     ;; sub-mp might not be resolved, and it might already been
     ;; assigned a name as well.
     (unsupported "resolved submodule path" mp))))

;; 'h' gives any existing mp -> name mappings. 'r' maps reserved
;; symbols to the next free number. 'mp' is the resolved module path
;; for which to get an assigned, unique name.
(define-with-contract*
  (-> (and/c hash? hash-equal?) (and/c hash? hash-eq?) resolved-module-path?
      (values (and/c hash? hash-equal?) (and/c hash? hash-eq?) symbol?))
  (resolved-mp->assigned-name h r mp)
  (let/ec return
    (define n (hash-ref h mp #f))
    (when n
      (return h r n))
    (match mp
      ((or (? path?) (? symbol?))
       (let*-values (([n] (resolved-mp->preferred-name mp))
                     ([r n] (next-gensym r n)))
         (return (hash-set h mp n) r n)))
      ((list 'submod sub-mp (? symbol? name) ...)
       (let*-values (([h r sub-n] (resolved-mp->assigned-name h r sub-mp))
                     ([r n] (next-gensym
                             r
                             (string->symbol
                              (string-join
                               (cons
                                (symbol->string sub-n)
                                (map
                                 (compose string-titlecase
                                          string-keep-basic-chars
                                          symbol->string)
                                 name))
                               "_")))))
         (return (hash-set h mp n) r n))))))

;;; 
;;; other
;;; 

#|

;; Turn into legal C++ names by replacing non-alphanumerics with "_".
;; The current renaming is unsafe, which we could fix by tracking
;; renamings using a map. Once we have locals we will also want more
;; "stable" naming for them.
(define (cxx-rename ast)
   (define f
    (topdown
     (lambda (ast)
       (cond
        ((Var? ast)
         (let* ((o-n-sym (Var-name ast))
                (o-n-str (symbol->string o-n-sym))
                (re #rx"[^a-zA-Z0-9_]")
                (s (regexp-replace* re o-n-str "_"))
                (n (string->symbol s)))
           (Var-rename ast n)))
        (else ast)))))
   (f ast))

(define (to-cxx fw? ast)
  (define (f ast)
    (cond
     ((Var? ast)
      (symbol->string (Var-name ast)))
     ((Module? ast)
      (map f (Module-body ast)))
     ((Define? ast)
      (let ((kind (Define-kind ast)))
        (cond
         ((or (eq? kind 'procedure)
              (eq? kind 'primitive))
          (list (and (not (Ast-anno-ref ast 'export)) "static")
                "void" (f (Define-var ast)) "()"
                (if fw? ";\n"
                    (list
                     "{\n"
                     (map f (Define-body ast))
                     "}\n"))))
         (else (error "unsupported" ast)))))
     (fw?
      (error "unsupported" ast))
     ((Pass? ast)
      "/* pass */;")
     ((Call? ast)
      (string-append (f (Call-proc ast)) "();"))
     ((Verbatim? ast)
      (Verbatim-text ast))
     (else
      (error "unsupported" ast))))
  (f ast))

(define* (to-cxx-text ast)
  (let* ((ast (cxx-rename ast))
         (fw (to-cxx #t ast))
         (im (to-cxx #f ast))
         (lst (filter identity (flatten (list fw im)))))
    (uncrustify
     (apply string-append
            (add-between lst " ")))))

|#
