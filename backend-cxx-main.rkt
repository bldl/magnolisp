#lang racket

#|

C++ back end.

|#

(require "ast-magnolisp.rkt" "backend-cxx-print.rkt" "backend-util.rkt"
         "compiler-util.rkt" "strategy.rkt"
         "util.rkt" "util/case.rkt" "util/system.rkt"
         compatibility/mlist)

;;; 
;;; Scheme compatibility
;;; 

;; A recursive version of list->mlist.
(define (list->mlist/rec x)
  (if (list? x)
      (apply mlist (map list->mlist/rec x))
      x))

;;; 
;;; Elegant Weapons translation
;;; 

(define (defs->ew defs)
  (void)) ;;xxx

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

(define* (generate-cxx-file kinds defs path-stem stdout? banner?)
  (set-for-each
   kinds
   (lambda (kind)
     (when (eq? kind 'cc)
       (define msexp (list->mlist/rec (defs->ew defs)))
       (define s (format-c msexp))
       ;; xxx uncrustify
       (define sfx (get-suffix kind))
       (define path (path-add-suffix path-stem sfx))
       (define filename (path-basename-as-string path))
       
       (write-generated-output
        path stdout?
        (thunk
         (when banner?
           (display-banner "//" filename))
         (display s))))))
  (void))
