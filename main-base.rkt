#lang racket/base

#|

Defines the base namespace for the magnolisp/base language. Only
racket/base and the metadata-preserving #%module-begin here.

|#

(provide (except-out (all-from-out racket/base) #%module-begin))

(require "modbeg.rkt")
(provide (rename-out (module-begin #%module-begin)))
