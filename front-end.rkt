#lang racket

#|

The 'ml-expand' macro expander expands all non-primitive syntax in the
'x' form, also recursively handling any 'import' constructs. Any
undefined names that appear are assumed to be top level ones, and may
not be macros, which must be defined before use.

We currently do not support modules for runtime code, and runtime code
has no phases, and hence we deal with a single runtime namespace.
Racket, which is used for metaprogramming (e.g., 'define-for-syntax'
and 'begin-for-syntax'), must effectively prefix names with module
names and phase numbers. Note that 'define-syntax' defined names are
still "runtime" names, as their uses can appear directly within
runtime code.

|#

(require "env.rkt" "form.rkt" "reader-ext.rkt" "syntax.rkt" "util.rkt")

;; Reads Magnolisp syntax. Produces a list of Racket syntax objects.
;; Any #lang directive is ignored. Filename extension matters not.
(define (load-as-syntaxes file)
  (let* ((path (cleanse-path file))
         (read (lambda (in)
                 (read-syntax path in))))
    (parameterize ((current-readtable
                    magnolisp-readtable))
      (call-with-input-file path
        (lambda (in)
          (port-count-lines! in)
          (read-language in (thunk (void)))
          (for/list ((obj (in-port read in)))
              obj))))))

(define (ml-load file)
  (let ((lst (load-as-syntaxes file)))
    (syntax-list->form lst)))

(define (load-as-forms file)
  (let ((lst (load-as-syntaxes file)))
    (map syntax->form lst)))

(define (form-print f)
  (pretty-print f)
  (parameterize ((print-annos? #t))
    (pretty-print f))
  (pretty-print (form->datum f)))

;;(form-print (ml-load "try-magnolisp-3.rkt"))

(define (ml-expand-form env ns x)
  (let ((d (form-datum x)))
    (or
     (lets
      then-if (pair? d)
      then-let y (car d)
      then-if (form? y)
      then-let a (form-datum y)
      then-if (symbol? a)
      then-if-let b (env-get env a)
      then-if-let expand (and (expandable? b)
                              (expandable-expand? b)
                              (expandable-expand b))
      ;; This must expand recursively. It is low-level enough to sort
      ;; of be a compiler extension mechanism.
      (expand env ns x))
     (values env
             (form
              (ml-expand/no-env env ns d)
              (form-annos x))))))

(define (ml-expand env ns x)
  (cond
   ((form? x)
    (ml-expand-form env ns x))
   ((list? x)
    (values env (map (fix ml-expand/no-env env ns) x)))
   (else
    (values env x))))

(define (ml-expand/no-env env ns x)
  (let-values (((env x) (ml-expand env ns x)))
    x))

(define (make-ns)
  (let ((ns (make-base-empty-namespace)))
    (parameterize ((current-namespace ns))
      (namespace-require 'racket))
    ns))

(define (expand-forms xs)
  (let ((env (env-with-specials))
        (ns (make-ns)))
    (map
     (lambda (x)
       (let-values (((n-env x) (ml-expand env ns x)))
         (set! env n-env)
         x))
     xs)))

;;; 
;;; special forms
;;; 

;; xxx probably want to prepopulate special forms into 'env'

;; xxx begin(?)
;; xxx define or define-values
;; xxx define-syntax or define-syntaxes
;; xxx define-values-for-syntax
;; xxx begin-for-syntax
;; xxx let or let-values or something
;; xxx let-syntax or something

(define (ensure-string s)
  (unless (string? s)
    (error "expected string" s))
  s)

(define (expand-include env ns x)
  (let ((d (form->datum x)))
    (match d
      ((list _ s)
       (let* ((file (ensure-string s))
              (lst (load-as-forms file)))
         (form
          (cons
           (form 'begin)
           lst)))))))

(define (env-with-specials)
  (env-new
   'include (expandable 'include #t expand-include)
   ))
