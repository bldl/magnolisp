#lang racket

#|

Integration with forms and Racket's syntax objects.

|#

(require "form.rkt" "util.rkt")

;; Note that 'syntax has special meaning to Racket, which affects
;; 'write', at least.
(define* (annos/stx stx (annos #hasheq()))
  ;;(writeln `(,annos ,stx))
  (hash-set annos 'stx (form stx)))

(define* (form/stx stx datum (annos #hasheq()))
  (form datum (annos/stx stx annos)))

(define* (set-anno->form lst annos)
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
         ;; Should we want to allow annotation of annotations.
         (hash-set annos name (syntax->form value-stx))
         ;;(hash-set annos name (syntax->datum value-stx))
         )))))

(define* (syntax->form stx (annos #hasheq()))
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

(define* (syntax-list->form lst annos)
  (form (map syntax->form lst) annos))
