#lang racket/base

#|

Have thus far been unable to get free-identifier=? check to match for
#%app, despite a variety of attempts.

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
    (syntax-case stx (#%plain-module-begin
                      define-values
                      module quote rt.%ast)
      ((module n pn (#%plain-module-begin body ...))
       (Module (stx-annos stx) (p-lst (syntax->list #'(body ...)))))
      ((_ rt.ast% (quote n) _)
       (eq? 'pass (syntax-e #'n))
       (Pass (stx-annos stx)))
      ((_ rt.ast% (quote n) fn)
       (eq? 'call (syntax-e #'n))
       (Call (stx-annos stx) #'fn))
      ((define-values (n) e)
       (Define (stx-annos stx)
         (syntax-e #'n) #'e))
      (else (error "not core language" stx))))

  (define (p-lst lst)
    (filter Ast? (map p lst)))

  (p mod-stx))

(define* (print-stx-with-bindings stx)
  (define lst (syntax->list stx))
  (cond
   (lst (for-each print-stx-with-bindings lst))
   ((identifier? stx) (writeln (list stx (identifier-binding stx))))
   (else (writeln stx))))
