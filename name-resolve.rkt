#lang racket

#|

Module loading.

|#

(require "util.rkt"
         syntax/modresolve)

;;; 
;;; utilities
;;; 

(define-syntax-rule (may-fail b ...)
  (with-handlers
      ((exn:fail? (lambda (e) #f)))
    b ...))

;;; 
;;; module paths
;;;

;; Confusingly, 'resolve-module-path' does not appear to (always)
;; return a 'resolved-module-path?'. This predicate may be used
;; instead. It reflects the contract for the return value of
;; 'resolve-module-path'.
(define* (resolve-module-path-result? x)
  (matches? x
   (? path?)
   (? symbol?)
   (list 'submod (or (? path?) (? symbol?)) (? symbol?) ..1)))

;; Turns resolve-module-path result format into
;; make-resolved-module-path result format.
(define-with-contract*
  (-> resolve-module-path-result? resolved-module-path?)
  (r-mp->rr-mp r-mp)
  (define (f path)
    (simplify-path (cleanse-path path)))
  (make-resolved-module-path
   (match r-mp
     ((? path?) (f r-mp))
     ((? symbol?) r-mp)
     ((list 'submod
            (and (or (? path?) (? symbol?)) p)
            (? symbol? subs) ..1)
      (cons (if (path? p) (f p) p) subs)))))
      	
;;; 
;;; module
;;; 

;; [r-mp resolve-module-path-result?] is the path as used for loading
;; the module. [def-lst (listof Def?)] is the set of ASTs, as loaded
;; from the submodule; a non-Magnolisp module simply gets a null value
;; for its 'def-lst', since it contains no Magnolisp syntax.
;; [bind->binding hash?] contains binding information for Magnolisp
;; identifiers appearing in the module.
(concrete-struct* Mod
                  (r-mp bind->binding def-lst)
                  #:transparent)

;;; 
;;; loading
;;; 

;; Loads the specified module. It is an error if the module path does
;; not specify an existing module.
(define-with-contract*
  (-> resolve-module-path-result? module-path? Mod?)
  (load-mod-from-submod r-mp mp)
  
  ;; Visit the module to determine if it even exists, and is a valid
  ;; module. This must succeed.
  (dynamic-require r-mp (void)
                   (thunk
                    (error 'load-mod-from-submod
                           "no such module: ~s (~a)" mp r-mp)))
  
  (define (make-sub-mp mp name)
    (match mp
      ((list 'submod outer sub ..1)
       `(submod ,outer ,@sub ,name))
      (_
       `(submod ,mp ,name))))
  (define sub-mp (make-sub-mp r-mp 'magnolisp-info))

  (define bind->binding #hasheq())
  (define def-lst null)
  
  (define original-r-mp
    (may-fail
     (dynamic-require sub-mp 'r-mp (thunk #f))))
  
  (when original-r-mp
    (unless (equal? r-mp original-r-mp)
      (error 'load-mod-from-submod
             "~a (~a): ~a != ~a (used != recorded)"
             "resolved module path mismatch"
             mp r-mp original-r-mp))

    (set! bind->binding
          (dynamic-require
           sub-mp 'bind->binding
           (thunk
            (error 'load-mod-from-submod
                   "no 'bind->binding for Magnolisp module ~a" mp)))) 
    
    (set! def-lst
          (dynamic-require
           sub-mp 'def-lst
           (thunk
            (error 'load-mod-from-submod
                   "no 'def-lst for Magnolisp module ~a" mp)))))
  
  (Mod r-mp bind->binding def-lst))
