#lang racket

#|

Implements a compiler for Magnolisp. Loads the code to be compiled
from Racket module metadata, included as submodules by the Racket
'magnolisp' language implementation.

The compiler ignores module top-level expressions.

The compiler requires a fully typed program (although not all types
have to be written out explicitly -- think 'auto' in C++).

Compiles only the 'entry-point' operations of the specified modules,
and their dependencies. This essentially means full program/library
optimization.

Generates a single .cpp, .hpp, and .mk output file. The header file
only declares the entry-point operations, and the abstract types they
depend upon. The implementation file has an non-entry-points declared
as internal (e.g., static). The GNU Make include file lists the
external dependencies for the program/library, as well as the .cpp and
.hpp files.

|#

(require "util.rkt"
         syntax/id-table syntax/moddep)

;;; 
;;; canonical module naming
;;; 

;; We want to pick a name here such that it will also be suitable for
;; use in C++. For consistency, we use lowercase names, without
;; underscores.

;; Any module paths are resolved relative to this path. May be #f, in
;; which case the current directory is used.
(define* mp-root-path (make-parameter #f))

(define (next-gensym r sym)
  (define num (hash-ref r sym 0))
  (define n-sym
    (if (= num 0)
        sym
        (string->symbol
         (string-append (symbol->string sym)
                        (number->string num)))))
  (values (hash-set r sym (+ num 1)) n-sym))

(define (string-keep-basic-chars s)
  (set! s (regexp-replace #rx"^[^a-zA-Z_]+" s ""))
  (set! s (regexp-replace* #rx"[^a-zA-Z0-9_]+" s ""))
  s)

(define (path-basename-only fn)
  (define-values (p f dir?) (split-path fn))
  (path-replace-suffix f ""))

(define-syntax-rule (unsupported v ...)
  (error "unsupported" v ...))

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
;; symbols to the next free number. 'mp' is the module path for which
;; to get an assigned, unique name. Note that this function cannot
;; handle module paths such as (quote #%kernel).
(define (mp->assigned-name h r mp)
  (let/ec return
    (define n (hash-ref h mp #f))
    (when n
      (return h r n))
    (define r-mp (resolve-module-path mp (mp-root-path)))
    (unless (equal? mp r-mp)
      (let-values (([h r n] (mp->assigned-name h r r-mp)))
        (return (hash-set h mp n) r n)))
    (match mp
      ((or (? path?) (? symbol?))
       (let*-values (([n] (resolved-mp->preferred-name mp))
                     ([r n] (next-gensym r n)))
         (return (hash-set h mp n) r n)))
      ((list 'submod sub-mp (? symbol? name) ...)
       (let*-values (([h r sub-n] (mp->assigned-name h r sub-mp))
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

#;
(let ((h #hash())
      (r #hasheq())
      (n #f)
      (mp-lst '(racket/base
                "compiler-api.rkt"
                (submod "compiler-api.rkt" foo)
                (submod "compiler-api.rkt" foo-)
                (submod "compiler-api.rkt" foo)
                (submod racket/base foo)
                (submod "compiler-api.rkt" foo-)
                (submod "compiler-api.rkt" bar baz)
                racket/base
                "compiler-api.rkt")))
  (for ((mp mp-lst))
    (set!-values [h r n] (mp->assigned-name h r mp))
    (writeln (list mp n))))
   
;;; 
;;; other
;;; 

;; Compilation state.
(struct St () #:transparent)

;; Returns an empty compilation state.
(define* (new-state)
  (St))

;; Updates compilation state with all the entry points in the
;; specified module, and all dependencies thereof. Returns an updated
;; compilation state.
(define* (compile-module st mp)
  (define m-annos
    (dynamic-require `(submod ,mp definfo) 'm-annos))
  (define m-ast
    (dynamic-require `(submod ,mp definfo) 'm-ast))
  st)

;; Compiles the module defined in the specified file. Returns an
;; updated compilation state.
(define* (compile-file st fn)
  (define s (if (path? fn) (path->string fn) fn))
  (define mp `(file ,s))
  (compile-module st mp))

;;; 
;;; code generation
;;; 

(define* (write-cpp-file st cpp-file)
  (void)) ;;xxx

(define* (write-hpp-file st hpp-file)
  (void)) ;;xxx

(define* (write-mk-file st mk-file)
  (void)) ;;xxx

;;; 
;;; testing
;;; 

(module* main #f
  (define st (new-state))
  (set! st (compile-module st "test-6-prog.rkt"))
  )
