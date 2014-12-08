#lang racket/base

#|

The variable(s) encoding Magnolisp core syntax are here. They must be
shared across all surface languages, as the Magnolisp parser must have
a consistent understanding of the identifier(s) across language
variants.

|#

(provide #%magnolisp)

(define #%magnolisp #f)
