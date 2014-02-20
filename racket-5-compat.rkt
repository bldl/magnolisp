#lang racket/base

#|

A collection of routines providing Racket 6 compatibility for code
written for Racket 5. Currently only raise-misc-error.

|#

(require racket/contract/base)

;; A DetailsTable is (listof (cons Field any))
;; A Field is one of
;; - string
;; - (cons string (listof FieldOption))
;; A FieldOption is one of
;; - 'multi
;; - 'value
;; - 'maybe

(define field-option/c (or/c 'multi 'value 'maybe))
(define field/c (or/c string? (cons/c string? (listof field-option/c))))

(define details-list/c
  (recursive-contract
   (or/c '() (cons/c field/c (cons/c any/c details-list/c)))))

(provide/contract
 [raise-misc-error
  (->* (symbol? string?)
       (#:continued (or/c string? (listof string))
        #:constructor (-> string? continuation-mark-set? exn?))
       #:rest details-list/c
       any)])

;; ----

(define (raise-misc-error who message
                          #:details [detail-table null]
                          #:continued [continued-message null]
                          #:constructor [constructor exn:fail]
                          . field+detail-list)
  (raise
   (constructor
    (compose* who message
              continued-message
              (field+detail-list->table 'raise-misc-error field+detail-list detail-table))
    (current-continuation-marks))))

(define (compose-error-message who message
                               #:details [detail-table null]
                               #:continued [continued-message null]
                               . field+detail-list)
  (let ([details
         (field+detail-list->table 'compose-error-message field+detail-list detail-table)])
    (compose* who message continued-message details)))

(define (compose* who message continued-message details)
  (let* ([parts (let loop ([details details])
                  (cond [(null? details) null]
                        [else
                         (let* ([field+opts (car (car details))]
                                [options (if (pair? field+opts) (cdr field+opts) '())]
                                [value? (memq 'value options)]
                                [multi? (memq 'multi options)]
                                [maybe? (memq 'maybe options)]
                                [convert-value
                                 (cond [value?
                                        (lambda (v) ((error-value->string-handler) v (error-print-width)))]
                                       [else
                                        (lambda (v) (format "~a" v))])]
                                [field (if (pair? field+opts) (car field+opts) field+opts)]
                                [value (cdr (car details))])
                           (cond [(and (or maybe? multi? (not value?))
                                       (not value))
                                  (loop (cdr details))]
                                 [multi?
                                  (list* "\n  " field ": "
                                         (let value-loop ([value value])
                                           (cond [(pair? value)
                                                  (list* "\n   "
                                                         (convert-value (car value))
                                                         (value-loop (cdr value)))]
                                                 [(null? value)
                                                  (loop (cdr details))])))]
                                 [else
                                  (list* "\n  " field ": "
                                         (convert-value value)
                                         (loop (cdr details)))]))]))]
         [parts (let loop ([continued continued-message])
                  (cond [(pair? continued) (list* "\n " (car continued) (loop (cdr continued)))]
                        [(string? continued) (loop (list continued))]
                        [(null? continued) parts]))]
         [parts (list* message (if (null? continued-message) "" ";") parts)]
         [parts (if who
                    (list* (symbol->string who) ": " parts)
                    parts)])
    (apply string-append parts)))

;; ----

(define (field+detail-list->table who lst onto)
  (cond [(null? lst) onto]
        [else
         (let ([field (car lst)]
               [value (cadr lst)])
           (cons (cons field value)
                 (field+detail-list->table who (cddr lst) onto)))]))

#|

This library is derived from the unstable/error library of Racket
5.3.6.

Racket
Copyright (c) 2010-2013 PLT Design Inc.

Racket is distributed under the GNU Lesser General Public License
(LGPL).  This means that you can link Racket into proprietary
applications, provided you follow the rules stated in the LGPL.  You can
also modify Racket; if you distribute a modified version, you must
distribute it under the terms of the LGPL, which in particular means
that you must release the source code for the modified software.  See
doc/release-notes/COPYING.txt for more information.

|#
