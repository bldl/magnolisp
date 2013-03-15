#lang racket/base

#|
|#

(require "ast.rkt")
(require "util.rkt")
(require (prefix-in rt. "runtime-compiler.rkt"))

;;(syntax-local-phase-level)
;;(free-identifier=? #'rt.%app #'#%app)
;;(identifier-binding #'rt.#%app)
;;(identifier-binding #'#%app)

;; Drops macro definitions, does alpha conversion, and turns the input
;; syntax object into an AST.
(define* (parse mod-stx)
  (define (p stx)
    (syntax-case stx (module #%plain-module-begin)
      ((module n pn (#%plain-module-begin body ...))
       (Module (stx-annos stx) (p-lst (syntax->list #'(body ...)))))
      ((app more ...)
       (begin
         (let ((x (car (syntax-e stx))))
           (writeln (list x (identifier? x)
                          (free-identifier=? x #'#%app)
                          (identifier-binding x))))
         (writeln #'app)
         (writeln (identifier? #'app))
         (writeln (free-identifier=? #'rt.#%app #'app))
         (writeln (free-identifier=? #'#%app #'app))
         (writeln (and (identifier? #'app)
                       (free-identifier=? #'app #'rt.#%app)))
         ;;(and (identifier? #'a) (free-identifier=? #'a #'rt.%ast))
         (Pass (stx-annos stx))))
      (else (error "not core language" stx))))

  (define (p-lst lst)
    (filter Ast? (map p lst)))

  (p mod-stx))

