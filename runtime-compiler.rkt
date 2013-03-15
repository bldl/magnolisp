#lang racket/base

#|

We cannot introduce new core language into Racket, and so we must be
able to express foreign syntax in terms of Racket syntax, without
using macros or runtime values. This is not really a problem since
unique identifiers for functions can be given here, and our syntax can
be expressed in terms of application of such functions. We cannot do
much quoting to make sure that we retain binding information, and to
make sure that macros get expanded. Again, no problem, as we can use
'lambda' as a container for code.

|#

(require "util.rkt")

(require (for-syntax "util.rkt" racket/base syntax/context))

;; racket/base exports nothing for-syntax
(provide (for-syntax (all-from-out racket/base)))
(provide (rename-out (my-module-begin #%module-begin)))
;(provide (rename-out (#%app %app)))

;; For now this is easier. We want to be more selective, though. We
;; want at least 'module' and '#%app', but we could define our own
;; version of the latter.
;;(provide (except-out (all-from-out racket/base) #%module-begin))

;; Merely by exporting these almost anything may appear in a top-level
;; program. Such an expansion is not very useful, however. We can
;; instead just list our core language in a stoplist. Which is not
;; useful either unless there is a way to do just a 'local-expand',
;; essentially, without ultimately doing the whole tree. Note also
;; that for language that we export from here, any #%app and such is
;; lexically bound already within the macro, and need not necessarily
;; exist in the use context.
; (provide #%app #%top)

;; Trying to make 'expand' insert this particular identifier.
(provide #%app)

;; Only required if we actually wrap the read code into a 'module'
;; before expansion.
(define-syntax-rule (my-module-begin form ...)
  (#%plain-module-begin form ...))

#;
(define-syntax* (%compilation-unit stx)
  (let ((stx-lst (cdr (syntax->list stx)))
        (def-ctx (syntax-local-make-definition-context))
        (ctx (generate-expand-context))
        )
    #`(begin #,@stx-lst)
    ))
    ;;local-expand  module

;; We bind accessors explicitly so that we get to bind the identifier
;; of the ctor ourselves, and know what it is.
(define-values (struct:%ast %ast %ast? %ast-ref %ast-set!)
  (make-struct-type '%ast #f 2 2))

;; Provide this so the compiler can compare syntax of ID against this
;; one to see if it is the same one.
(provide %ast)

(define-syntax-rule*
  (pass)
  (%ast 'pass #f))

(define-syntax-rule*
  (call n)
  (%ast 'call n))

(define-syntax* (procedure stx)
  (syntax-case stx ()
    ((_ (n) body ...)
     #'(define n
         (%ast 'procedure
               (lambda () body ...))))))
