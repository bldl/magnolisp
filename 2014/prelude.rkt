#lang s-exp "base.rkt"

#|

|#

(require "../core.rkt"
         (only-in "../surface.rkt" declare)
         "surface.rkt")

(provide Bool Void) ;; re-exports

(declare #:type Bool #:: ([foreign mgl_Bool]))
(declare #:type Void #:: ([foreign void]))
