#lang racket

#|

We have some options for preserving type information, but we opt for
having the expansion itself generate a table as well as code to
persist the type information. It is notable that an id-table can be
used even for local names, since identifiers are unique.

Whatever we export should also have some location information, so we
do our best to preserve this information for any syntax objects we
include in our metadata. Should we discover errors only once we start
actual compilation or linking, then we need to be able to still report
errors properly.

To record metadata for the compiler, we use code that runs in phase
level 1, but concerns phase level 0. Since the recording code lives in
phase 1, the respective module's #%module-begin will be executed in
the same phase, and will hence have access to the information (via the
same variables at the same phase level).

|#

(provide module-begin base-module-begin)

(require "annos-store.rkt"
         (for-syntax
          racket/dict racket/pretty syntax/id-table syntax/quote
          "compiler-util.rkt" "util.rkt" "syntax-quote-macros.rkt"))

(begin-for-syntax
 ;; Given [h hash?], returns syntax for an expression that produces an
 ;; (and/c hash? hash-eq? immutable?) value. The hash table values are
 ;; assumed to be syntax objects, and they are preserved as such.
 (define (syntax-for-hasheq h)
   #`(make-immutable-hasheq
      (list #,@(hash-map
                h
                (lambda (n-sym val-stx)
                  #`(cons '#,n-sym (quote-syntax/keep-properties #,val-stx)))))))
 
 ;; Given id-tables [ts (listof dict?)], returns syntax for an
 ;; expression that produces something for which the dict? predicate
 ;; returns true. The table values are assumed to be syntax objects,
 ;; and they are preserved as such.
 (define (syntax-for-id-table-dict . ts)
   #`(list #,@(apply
               append
               (map
                (lambda (t)
                  (dict-map
                   t
                   (lambda (id-stx h)
                     #`(cons #'#,id-stx #,(syntax-for-hasheq h)))))
                ts))))

 (define (make-definfo-submodule ast)
   ;;(set! ast (disarm* ast))
   ;;(set! ast (syntax-disarm ast (current-code-inspector)))
   ;;(writeln (syntax-tainted? ast))
   ;;(set! ast (strip-phase-1+ ast))
   ;;(pretty-print (syntax->datum ast))
   #`(begin-for-syntax
      (module* magnolisp-info #f
        (define m-id-count #,(+ (dict-count definfo-table-b)
                                (dict-count definfo-table-f)))
        (define m-annos
          (make-immutable-free-id-table
           #,(syntax-for-id-table-dict definfo-table-b definfo-table-f)
           #:phase 0))
        (define m-ast (quote-syntax/keep-properties #,ast #:listed (in-racket local-ec origin paren-shape)))
        (provide m-id-count m-annos m-ast))))
 ) ;; end begin-for-syntax

(define-syntax (base-module-begin stx)
  (syntax-case stx ()
    ((_ . bodies)
     (let* ((ast (local-expand #'(#%module-begin . bodies)
                               'module-begin null))
            (sm-stx (make-definfo-submodule ast)))
       (with-syntax (((mb . bodies) ast)
                     (sm sm-stx))
         (let ((mb-stx #'(mb sm . bodies)))
           ;;(pretty-print (syntax->datum mb-stx))
           ;;(pretty-print (syntax->datum/free-id mb-stx))
           ;;(pretty-print (syntax->datum/binding ast))
           ;;(pretty-print (syntax->datum/binding sm-stx #:conv-id id->datum/phase))
           ;;(pretty-print (syntax->datum/binding sm-stx #:pred (lambda (x) (memq x '(equal? r.equal?)))))
           mb-stx))))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    ((_ . bodies)
     (with-syntax ((prelude (datum->syntax stx 'magnolisp/prelude)))
       #'(base-module-begin (require prelude) . bodies)))))
