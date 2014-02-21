#lang racket

#|

When not being required for compilation or for editor support or such,
we need nothing but the standard Racket behavior.

For when being required for compilation, we must make sure that we
also make available compilation-related metadata in a submodule, which
could be called 'magnolisp-info' or something. We take care to preserve type
information, and put it into said module. We also want to put an AST
in there.

The evaluator ignores type annotations (it does not even load them),
whereas the compiler requires a fully typed program (although not all
types have to be written out explicitly -- think 'auto' in C++).

We have some options for preserving type information, but we opt for
having the expansion itself generate a table as well as code to
persist the type information. It is notable that an id-table can be
used even for local names, since identifiers are unique.

Whatever we export should also have some location information, so we
do our best to preserve this information for any syntax objects we
include in our metadata. Should we discover errors only once we start
actual compilation or linking, then we need to be able to still report
errors properly.

When required for evaluation, we display the value of module top-level
expressions, whereas otherwise we do not. This can be useful for
testing.

To record metadata for the compiler, we use code that runs in phase
level 1, but concerns phase level 0. Since the recording code lives in
phase 1, the respective module's #%module-begin will be executed in
the same phase, and will hence have access to the information (via the
same variables at the same phase level).

|#

(provide module-begin)

(require "annos-store.rkt"
         (for-syntax
          racket/dict racket/pretty syntax/id-table syntax/quote
          typed-racket/utils/disarm ;; probably considered internal
          "syntax-quote.rkt" "util.rkt"))

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
   (set! ast (disarm* ast))
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
        (define m-ast (quote-syntax/keep-properties #,ast (paren-shape origin local-ec)))
        (provide m-id-count m-annos m-ast))))
 ) ;; end begin-for-syntax

(define-syntax (module-begin stx)
  (syntax-case stx ()
    ((_ . bodies)
     (let ((ast (local-expand #'(#%module-begin . bodies)
                              'module-begin null)))
       (with-syntax (((mb . bodies) ast)
                     (sm (make-definfo-submodule ast)))
         (let ((mb-stx
                #'(mb sm . bodies)))
           ;;(pretty-print (syntax->datum mb-stx))
           mb-stx))))))
