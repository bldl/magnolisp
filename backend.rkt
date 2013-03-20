#lang racket

#|

C++ back end.

|#

(require "ast.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt" "util/system.rkt")

;; Not quite perfect as does not appear to insert line breaks.
;; http://astyle.sourceforge.net/
;; (astyle "void main() { return; }")
(define (astyle s)
  (exe-filter s '("/usr/bin/astyle" "--mode=c"
                  "--lineend=linux" "--style=otbs"
                  "--quiet")))

;; This one does line breaking.
;; Needs a config file, so try
;; uncrustify --update-config > ~/.uncrustify.cfg
;; http://uncrustify.sourceforge.net/
;; (uncrustify "void main() { return; }")
(define (uncrustify s)
  (exe-filter s '("/usr/bin/uncrustify" "-l" "cpp" "-q")))

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

;; Forward declarations to be done later.
(define* (to-cxx-text ast)
  (define (f ast)
    (cond
     ((Var? ast)
      (symbol->string (Var-name ast)))
     ((Pass? ast)
      "/* pass */;")
     ((Call? ast)
      (string-append (f (Call-proc ast)) "();"))
     ((Module? ast)
      (map f (Module-body ast)))
     ((Define? ast)
      (case-eq (Define-kind ast)
               (procedure
                (list "void " (f (Define-var ast))
                      "{"
                      (map f (Define-body ast))
                      "}\n"))
               (else (error "unsupported" ast))))
     (else
      (error "unsupported" ast))))
  (uncrustify
   (apply string-append
          (add-between
           (flatten (list (f (cxx-rename ast)))) " "))))

