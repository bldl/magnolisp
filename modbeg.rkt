#lang racket/base

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
          racket/base racket/dict racket/list racket/pretty
          syntax/id-table syntax/modresolve syntax/quote
          "app-util.rkt" "ast-magnolisp.rkt" "ast-serialize.rkt"
          "parse.rkt" "util.rkt"))

(define-for-syntax (make-definfo-submodule modbeg-stx)
  (define annos (get-stored-definfo))
  ;;(pretty-print (dict->list info))
  ;;(pretty-print (syntax->datum modbeg-stx))
  ;;(pretty-print (syntax->datum/binding modbeg-stx #:conv-id id->datum/phase))
  (define-values (defs provs reqs)
    (parse-defs-from-module modbeg-stx annos #f)) ;; xxx rr-mp is obsolete
  ;;(pretty-print (dict->list defs)) (exit)

  (define id->bind (make-free-id-table #:phase 0))
  (define bind->binding (make-hasheq))
  (define next-r #hasheq())

  ;;(writeln `(current source ,(current-module-declare-source)))
  
  ;; (current-module-declare-name) as well as
  ;; (current-module-path-for-load) seem to be #f during byte
  ;; compilation.
  (define rel-to-path-v (resolved-module-path-name
                         (current-module-declare-name)))
  ;; (writeln rel-to-path-v)

  (define (rw-id id)
    (define bind (dict-ref id->bind id #f))
    (unless bind
      (set!-values (next-r bind) (next-gensym next-r (syntax-e id)))
      (dict-set! id->bind id bind))
    (define b (identifier-binding id 0))
    (define bi
      (if (not (list? b))
          b
          (let ([mpi (first b)]
                [sym (second b)]
                [ph (sixth b)])
            ;; Not bound as Magnolisp if the source phase level is not 0.
            (and (eqv? ph 0)
                 (let ((r-mp (resolve-module-path-index mpi rel-to-path-v)))
                   ;;(writeln (list r-mp sym))
                   (list r-mp sym))))))
    (define old-bi (hash-ref bind->binding bind #f))
    (when (and old-bi (not (equal? bi old-bi)))
      (error 'make-definfo-submodule
             "differing bindings for the same ID: ~s != ~s (~s)"
             old-bi bi id))
    (hash-set! bind->binding bind bi)
    (identifier->ast id #:bind bind))
  
  (define def-lst
    (for/list ([(id def) (in-dict defs)]
               #:when (ast-anno-maybe def 'top)) ;;xxx should not even put there
      (ast-rw-Ids rw-id def)))
  
  #`(begin-for-syntax
     (module magnolisp-info racket/base
       (require magnolisp/ast-magnolisp)
       (define r-mp #,(syntactifiable-mkstx rel-to-path-v))
       (define bind->binding #,(syntactifiable-mkstx bind->binding))
       (define def-lst #,(syntactifiable-mkstx def-lst))
       (provide r-mp bind->binding def-lst))))

(define-syntax (base-module-begin stx)
  (syntax-case stx ()
    ((_ . bodies)
     (let* ((ast (local-expand #'(#%module-begin . bodies)
                               'module-begin null))
            (sm-stx (make-definfo-submodule ast)))
       (with-syntax (((mb . bodies) ast)
                     (sm sm-stx))
         (let ((mb-stx #'(mb sm . bodies)))
           ;;(pretty-print (syntax->datum sm-stx))
           ;;(pretty-print (syntax->datum mb-stx))
           ;;(pretty-print (syntax->datum/free-id mb-stx))
           ;;(pretty-print (syntax->datum/binding ast))
           ;;(pretty-print (syntax->datum/binding sm-stx #:conv-id id->datum/phase))
           ;;(pretty-print (syntax->datum/binding sm-stx #:pred (lambda (x) (memq x '(equal? r.equal?)))))
           mb-stx))))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    ((orig-mb . bodies)
     (let ()
       ;;(writeln (syntax-source-module #'orig-mb #t))
       ;;(define mpi (syntax-source-module #'orig-mb #f))
       ;;(writeln (module-path-index-resolve mpi))
       ;;(writeln `(enclosing #%module-begin is ,#'orig-mb ,(syntax-source #'orig-mb) ,(syntax-source-module #'orig-mb #f) ,(syntax-source-module #'orig-mb #t)))
       (with-syntax ((prelude (datum->syntax stx 'magnolisp/prelude)))
         #'(base-module-begin (require prelude) . bodies))))))
