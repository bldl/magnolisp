#lang racket

#|

Definition and implementation of "views".

E.g.,

  (define-view* Def #:fields (annos id))

and then, within a macro, generate-view-methods may be used to
generate syntax for an implementation of the relevant generic methods
for a given view.

E.g.,

  (generate-view-methods #'DefVar #'Def)

  =>

  (list
    #'#:methods #'gen:Def
    #'[(define (Def-copy obj annos id)
         (struct-copy DefVar obj [annos annos] [id id]))
       (define (Def-annos obj) (DefVar-annos obj))
       (define (Def-id obj) (DefVar-id obj))
       (define (set-Def-annos obj annos)
         (struct-copy DefVar obj [annos annos]))
       (define (set-Def-id obj id)
         (struct-copy DefVar obj [id id]))])

|#

(require "util.rkt" racket/generic
         (for-syntax racket/pretty racket/syntax syntax/parse))

(define-syntax* (define-view* stx)
  (syntax-parse stx
    [(_ view:id #:fields (fld:id ...))
     (let ()
       (define view-id #'view)
       (define view-name (syntax-e view-id))
       (define fld-ids (syntax->list #'(fld ...)))
       (define (make-accessor-sigs fld-id)
         (define fld-name (syntax-e fld-id))
         (define getter-sig
           #`(#,(format-id stx "~a-~a" view-name fld-name) view))
         (define setter-sig
           #`(#,(format-id stx "set-~a-~a" view-name fld-name) view #,fld-id))
         (list getter-sig setter-sig))
       (define method-sig-lst
         (cons
          #`(#,(format-id stx "~a-copy" view-name) view fld ...)
          (apply append (map make-accessor-sigs fld-ids))))
       (with-syntax ([(method ...) method-sig-lst]
                     [view-info (format-id stx "view:~a" view-name)])
         #'(begin
             (define-syntax view-info
               (list #'fld ...))
             (define-generics* view
               method ...))))]))

;; Generates syntax for specifying the implementation of the methods
;; of the specified view for the specified concrete struct type. The
;; resulting list of syntax objects is intended to be spliced into a
;; (struct ...) declaration. E.g., (generate-view-methods #'C #'V)
;; -> (list #'#:methods #'gen:V (...)).
(define-for-syntax* (generate-view-methods conc-id view-id)
  (define view-name (syntax-e view-id))
  (define conc-name (syntax-e conc-id))
  (define fld-id-lst
    ;; Note that this call requires transformer context.
    (syntax-local-value (format-id view-id "view:~a" view-name)))
  ;;(pretty-print `(INFO ,info))
  (with-syntax ([view view-id]
                [conc conc-id])
    (define (make-accessor-impls fld-id)
      (define fld-name (syntax-e fld-id))
      (define getter-impl
        (with-syntax ([v-getter-id
                       (format-id view-id "~a-~a" view-name fld-name)]
                      [c-getter-id
                       (format-id conc-id "~a-~a" conc-name fld-name)])
          #'(define (v-getter-id view)
              (c-getter-id view))))
      (define setter-impl
        (with-syntax ([v-setter-id
                       (format-id view-id "set-~a-~a" view-name fld-name)]
                      [fld fld-id])
          #'(define (v-setter-id view fld)
              (struct-copy conc view [fld fld]))))
      (list getter-impl setter-impl))
    (define method-impl-lst
      (cons
       (with-syntax ([(fld ...) fld-id-lst])
         #`(define (#,(format-id view-id "~a-copy" view-name) view fld ...)
             (struct-copy conc view [fld fld] ...)))
       (apply append
              (map make-accessor-impls fld-id-lst))))
    (list
     (quote-syntax #:methods)
     (format-id view-id "gen:~a" view-name)
     #`(#,@method-impl-lst))))
