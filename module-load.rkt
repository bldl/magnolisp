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
   (list 'quote (? symbol?)) ;; custom extension
   (list 'submod (or (? path?) (? symbol?)) (? symbol?) ..1)))

;; Like resolve-module-path, but leaves (quote sym) paths as they are,
;; without resolving. The result is acceptable to dynamic-require.
(define-with-contract*
  (-> module-path? any/c resolve-module-path-result?)
  (resolve-module-path/primitive mp rel-to-path-v)
  (match mp
    ((list 'quote (? symbol?)) mp)
    (_ (resolve-module-path mp rel-to-path-v))))

;; Turns resolve-module-path result format into
;; make-resolved-module-path result format. The result is canonical
;; and interned, and thus suitable for eq? comparison.
(define-with-contract*
  (-> resolve-module-path-result? resolved-module-path?)
  (r-mp->rr-mp r-mp)
  (define (f path)
    (simplify-path (cleanse-path path)))
  (define (g r-mp)
   (match r-mp
     ((? path?) (f r-mp))
     ((? symbol?) r-mp)
     ((list 'quote (? symbol? sym)) sym)
     ((list 'submod base (? symbol? subs) ..1)
      (cons (g base) subs))))
  (make-resolved-module-path (g r-mp)))

;; Turns resolve-module-path result format into a module path, i.e.,
;; something that could be used with dynamic-require.
(define-with-contract*
  (-> resolve-module-path-result? module-path?)
  (r-mp->mp r-mp)
  (match r-mp
    ((? path?) r-mp)
    ((? symbol?) `(quote ,r-mp))
    ((list 'submod
           (and (or (? path?) (? symbol?)) p)
           (? symbol? subs) ..1)
     (if (path? p)
         r-mp
         `(submod `(quote ,r-mp) ,@subs)))))

;;; 
;;; module
;;; 

;; [r-mp resolve-module-path-result?] is the path as used for loading
;; the module. [def-lst (listof Def?)] is the set of ASTs, as loaded
;; from the submodule; a non-Magnolisp module simply gets a null value
;; for its 'def-lst', since it contains no Magnolisp syntax.
;; [bind->binding hash?] contains binding information for Magnolisp
;; identifiers appearing in the module. [ep? boolean?] indicates
;; whether the module is an entry point one.
(concrete-struct* Mod
                  (r-mp bind->binding def-lst ep?)
                  #:transparent)

;;; 
;;; loading
;;; 

;; Loads the specified module. It is an error if the module path does
;; not specify an existing module. The `ep?` value does not affect
;; loading, but is merely stored in the returned module object.
(define-with-contract*
  (-> resolve-module-path-result? module-path? boolean? Mod?)
  (Mod-load r-mp mp ep?)
  
  ;; Visit the module to determine if it even exists, and is a valid
  ;; module. This must succeed.
  (dynamic-require r-mp (void)
                   (thunk
                    (error 'Mod-load
                           "no such module: ~s (~a)" mp r-mp)))
  
  (define (make-sub-mp mp name)
    (match mp
      ((list 'submod outer sub ..1)
       `(submod ,outer ,@sub ,name))
      (_
       `(submod ,mp ,name))))
  (define sub-mp (make-sub-mp r-mp 'magnolisp-s2s))

  (define def-lst #f)

  ;; Using 'may-fail' here as there may be no submodule.
  (define bind->binding
    (may-fail
     (dynamic-require
      sub-mp 'bind->binding
      (thunk #f))))

  (when bind->binding
    (define original-r-mp
      (dynamic-require
       sub-mp 'r-mp
       (thunk
        (error 'Mod-load
               "missing symbol 'r-mp for Magnolisp module ~s" mp))))

    (when (and original-r-mp (not (equal? r-mp original-r-mp)))
      (error 'Mod-load
             "~a (~s): ~s != ~s (used != recorded)"
             "resolved module path mismatch"
             mp r-mp original-r-mp))

    (set! def-lst
          (dynamic-require
           sub-mp 'def-lst
           (thunk
            (error 'Mod-load
                   "missing symbol 'def-lst for Magnolisp module ~a" mp)))))
  
  (Mod r-mp
    (or bind->binding #hasheq())
    (or def-lst null)
    ep?))
