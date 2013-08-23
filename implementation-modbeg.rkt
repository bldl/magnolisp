#lang racket

#|

From here we do not export the AST. We only export the metadata.

TODO Do not yet know how the metadata ends up from provide forms into
the id-table which we are exporting from here.

|#

(provide module-begin)

(require "metadata-parser.rkt"
         (for-syntax syntax/id-table
                     "util.rkt"))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ . bodies) 
     (with-syntax ([(mb . bodies)
                    (local-expand #'(#%module-begin . bodies)
                                 'module-begin
                                 null)])
       (with-syntax ([((d-n . d-k) ...)
                      (bound-id-table-map definfo-table cons)])
         #'(mb (begin-for-syntax
                (module* metadata #f
                  (define re-t (make-bound-id-table #:phase 0))
                  (bound-id-table-set! re-t #'d-n d-k) ...
                  (provide (rename-out [re-t m-definfo-tbl])))
                ) . bodies)))]))
