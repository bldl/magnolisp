#lang racket/base

#|

Module loading.

|#

(require (for-syntax racket/base)
         racket/contract/base
         racket/function
         racket/match
         syntax/modresolve
         "util.rkt")

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
;; from the submodule (a non-Magnolisp module simply gets a null value
;; for its 'def-lst', since it contains no Magnolisp syntax).
;; [bind->binding hash?] contains binding information for Magnolisp
;; identifiers appearing in the module (reflecting the module from
;; which each binding originates, not any re-exports). The
;; `prelude-lst` field is a list of module paths specifying the
;; runtime libraries required by the module. [core->bind hash?] maps
;; built-in symbols to local bind values. The `attrs` field contains a
;; mutable hasheq of attributes, with currently supported keys being
;; `ep?` and `prelude?`.
(concrete-struct* 
 Mod (r-mp bind->binding def-lst prelude-lst core->bind attrs)
 #:transparent)

(define* (Mod-ep? mod)
  (hash-ref (Mod-attrs mod) 'ep? #f))

(define* (Mod-prelude? mod)
  (hash-ref (Mod-attrs mod) 'prelude? #f))

;;; 
;;; loading
;;; 

(define (make-sub-mp mp name)
  (match mp
    [(list 'submod outer sub ..1)
     `(submod ,outer ,@sub ,name)]
    [_
     `(submod ,mp ,name)]))

;; Loads the specified module. It is an error if the module path does
;; not specify an existing module.
(define-with-contract*
  (-> resolve-module-path-result? module-path? Mod?)
  (Mod-load r-mp mp)

  ;;(writeln (list 'Mod-load r-mp mp))
  
  ;; Visit the module to determine if it even exists, and is a valid
  ;; module. This must succeed.
  (dynamic-require r-mp (void)
                   (thunk
                    (error 'Mod-load
                           "no such module: ~s (~a)" mp r-mp)))
  
  (define sub-mp (make-sub-mp r-mp 'magnolisp-s2s))

  (define has-submod?
    (may-fail (dynamic-require sub-mp #f) #t))

  (define (load-field sym compulsory?)
    (dynamic-require 
     sub-mp sym
     (thunk
      (and 
       compulsory?
       (error 'Mod-load
              "missing symbol ~s for Magnolisp module ~s" sym mp)))))
  
  (cond
    [(not has-submod?)
     (Mod r-mp #hasheq() null null #hasheq() (make-hasheq))]
    [else
     (define bind->binding (load-field 'bind->binding #t))
     (define def-lst (load-field 'def-lst #t))
       
     (Mod r-mp 
          bind->binding 
          def-lst 
          (load-field 'prelude-lst #t)
          (load-field 'core->bind #t)
          (make-hasheq))]))
