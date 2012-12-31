#lang racket

#|
|#

(require "form.rkt" "reader-ext.rkt" "syntax.rkt" "util.rkt")

;; Reads Magnolisp syntax. Produces a list of Racket syntax objects.
;; Any #lang directive is ignored. Filename extension matters not.
(define* (load-as-syntaxes file)
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

;;(load-as-syntaxes "try-magnolisp-3.rkt")

(define* (load-as-form file)
  (let ((lst (load-as-syntaxes file)))
    (syntax-list->form lst)))

(define (form-print f)
  (pretty-print f)
  (parameterize ((print-annos? #t))
    (pretty-print f))
  (pretty-print (form->datum f)))

(form-print (load-as-form "try-magnolisp-3.rkt"))
