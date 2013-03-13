#lang racket

#|

|#

(require "util.rkt" (for-syntax "util.rkt"))
(require (for-syntax syntax/context))

(require "runtime-common.rkt")
(provide (all-from-out "runtime-common.rkt"))

;; Merely by exporting these almost anything may appear in a top-level
;; program. Such an expansion is not very useful, however. We can
;; instead just list our core language in a stoplist.
(provide #%app #%top)

(define-syntax* (%compilation-unit stx)
  (let ((stx-lst (cdr (syntax->list stx)))
        (def-ctx (syntax-local-make-definition-context))
        (ctx (generate-expand-context))
        )
    #`(begin #,@stx-lst)
    ))
    ;;local-expand  

;;; 
;;; declarations
;;; 

;;; 
;;; statements
;;; 

