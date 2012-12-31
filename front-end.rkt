#lang racket

#|
|#

(require "reader-ext.rkt" "util.rkt")

;; Reads Magnolisp syntax. Produces a list of Racket syntax objects.
;; Any #lang directive is ignored. Filename extension matters not.
(define* (load-ml file)
  (let* ((path (cleanse-path file))
         (read (lambda (in)
                 (read-syntax path in))))
    (parameterize ((current-readtable
                    magnolisp-readtable))
      (call-with-input-file path
        (lambda (in)
          (read-language in (thunk (void)))
          (for/list ((obj (in-port read in)))
              obj))))))

(load-ml "try-magnolisp-3.rkt")



