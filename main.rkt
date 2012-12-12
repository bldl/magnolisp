#lang racket

#|

Defines the language for Magnolisp. This really just grabs the input
programs as syntax objects, and then transforms and evaluates those to
implement language semantics. Racket is a foreign language to an input
program.

|#

(require (for-syntax racket "form.rkt" "util.rkt"))

(provide (rename-out (my-module-begin #%module-begin)))

;; Note that 'syntax has special meaning to Racket, which affects
;; 'write', at least.
(define-for-syntax (annos/stx stx (annos #hasheq()))
  ;;(writeln `(,annos ,stx))
  (hash-set annos 'stx stx))

(define-for-syntax (form/stx stx datum (annos #hasheq()))
  (form datum (annos/stx stx annos)))

(define-for-syntax (set-anno->form lst annos)
  ;;(writeln (list annos lst))
  (when (not (= (length lst) 3))
    (error 'set-anno "expected form (set-anno (NAME VALUE) EXPR)"))
  (let ((a-lst (syntax->list (second lst))))
    (unless (and a-lst (= (length a-lst) 2))
      (error 'set-anno "expected anno of form (NAME VALUE)"))
    (let ((name-stx (first a-lst)))
      (unless (identifier? name-stx)
        (error 'set-anno "expected symbol as anno name"))
      (let ((name (syntax->datum name-stx))
            (value-stx (second a-lst)))
        (syntax->form
         (third lst)
         ;; Should we want to annotate annotations.
         ;;(hash-set annos name (syntax->form value-stx))
         (hash-set annos name (syntax->datum value-stx))
         )))))

(define-for-syntax (syntax->form stx (annos #hasheq()))
  (let ((e (syntax-e stx)))
    (cond
     ((symbol? e)
      (form/stx stx e annos))
     ((null? e)
      (form/stx stx e annos))
     ((pair? e)
      (if-let lst (syntax->list stx)
              (let ((h (car lst)))
                (if (and (identifier? h)
                         (eq? 'set-anno (syntax->datum h)))
                    (set-anno->form lst annos)
                    (syntax-list->form lst (annos/stx stx annos))))
              (form (cons (syntax->form (car e))
                          (syntax->form (cdr e)))
                    (annos/stx stx annos))))
     (else
      (form/stx stx e annos)))))

(define-for-syntax (syntax-list->form lst annos)
  (form (map syntax->form lst) annos))

(define-syntax (my-module-begin stx)
  (syntax-case stx ()
      ((_ body ...)
       (begin
         (let ((f (let ((lst (syntax->list #'(body ...))))
                    (syntax-list->form lst (annos/stx stx)))))
           (pretty-print f)
           (parameterize ((print-annos? #t))
             (pretty-print f))
           (pretty-print (form->datum f)))
         #`(#%module-begin)))))
