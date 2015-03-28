#lang racket/base

#|

|#

(module modbeg racket/base
  (provide module-begin/2014)
  (require "../modbeg.rkt" (for-syntax racket/base))
  (define-syntax (module-begin/2014 stx)
    (make-module-begin stx #:prelude #''(magnolisp/2014/prelude))))

(require (submod "." modbeg))

(provide (except-out (all-from-out racket/base) #%module-begin)
         (rename-out [module-begin/2014 #%module-begin]))
