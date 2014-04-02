#lang racket/base

#|

Scheme compatibility.

|#

(require compatibility/mlist)

(provide list->mlist/rec)

;; A recursive version of list->mlist.
(define (list->mlist/rec x)
  (if (list? x)
      (apply mlist (map list->mlist/rec x))
      x))

