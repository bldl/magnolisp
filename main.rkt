#lang racket

#|

Defines the language for Magnolisp. This really just grabs the input
programs as syntax objects, and then transforms and evaluates those to
implement language semantics. Racket is a foreign language to an input
program.

|#

(require "form.rkt" "util.rkt")
(require (for-syntax racket "form.rkt" "util.rkt"))

(provide (rename-out (my-module-begin #%module-begin)))

(define-for-syntax (syntax->form stx)
  (let ((e (syntax-e stx)))
    (cond
     ((symbol? e)
      (form e))
     ((null? e)
      (form e))
     ((pair? e)
      (with-handlers
          ((exn:fail?
            (lambda (ex) ;; handle improper list case
              (form (cons (syntax->form (car e))
                          (syntax->form (cdr e)))))))
        ;; We preper this to get a single annotation for the entire
        ;; list.
        (syntax-list->form e)))
     (else
      (form e)))))

(define-for-syntax (syntax-list->form lst)
  (form (map syntax->form lst)))

(define-for-syntax (list-syntax->form stx)
  (let ((lst (syntax->list stx)))
    (syntax-list->form lst)))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
      ((_ body ...)
       (begin
         (let ((f (list-syntax->form #'(body ...))))
           (pretty-print f)
           (pretty-print (form->datum f)))
         #`(#%module-begin)))))
