#lang racket

#|
|#

(require "util.rkt")

;;;
;;; generic utilities
;;;

(define-syntax on-fail
  (syntax-rules ()
    ((_ fail-expr expr)
     (with-handlers
         ((exn:fail?
           (lambda (e) fail-expr)))
       expr))))

(define (file-read file)
  (call-with-input-file*
   file
   (lambda (in)
     (port->string in))))
     
;; Checks whether a file either does not exist or has been changed.
(define (file-changed? file s)
  ;; Would there be a good way to write a function for comparing two
  ;; input streams? Then we could handle large files as well. ((nin
  ;; (open-input-string s))) and then compare to file input.
  (on-fail #t (not (equal? (file-read file) s))))

(define (write-changed-file file s)
  (when (file-changed? file s)
    (call-with-output-file*
     file
     (lambda (out)
       (display s out))
     #:exists 'truncate/replace)
    (displayln file)))

(define (capture-output f)
  (let ((output (open-output-string)))
    (parameterize ((current-output-port output))
      (f))
    (get-output-string output)))

(define-syntax capture
  (syntax-rules ()
    ((_ body ...)
     (capture-output (lambda () body ...)))))

(define (space-join l)
  (string-join l " "))

(define (for-each-sep elemact sepact lst)
  (define first #t)
  (for-each
   (lambda (elem)
     (if first
         (set! first #f)
         (when sepact (sepact)))
     (when elemact (elemact elem)))
   lst))

;;;
;;; local utilities
;;;

(define (disp . args)
  (display (apply format args)))

(define (disp-nl . args)
  (apply disp args) (newline))

;;; 
;;; special values 
;;;

(concrete-struct* hexnum (num) #:transparent)

;;;
;;; pretty printing
;;;

(define* (display-generated-notice pfx)
  (display pfx)
  (displayln " generated -- do not edit"))

(define (write-scheme-symlink file target)
  (write-changed-file
   file
   (capture
    (display-generated-notice ";;")
    (displayln "#lang racket")
    (disp-nl "(require ~s)" target)
    (disp-nl "(provide (all-from-out ~s))" target))))

(define path-censor-re #rx"[-.]")

(define (path-h-ifdefy p)
  (string-append
   "__"
   (string-downcase
    (regexp-replace* path-censor-re (path->string (path-basename p)) "_"))
   "__"
   ))

(define ident-censor-re #rx"[-]")

(define (name-to-c sym)
  (string->symbol
   (string-append
    ;;"__"
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    ;;"__"
    )))

(define (name-to-ruby sym)
  (string->symbol
   (string-append
    "$"
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    )))

(define (name-to-gmake sym)
  (string->symbol
   (string-append
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    )))

(define (name-to-gmake/negate sym)
  (string->symbol
   (string-append
    "NOT__"
    (string-upcase
     (regexp-replace* ident-censor-re (symbol->string sym) "_"))
    )))

(define (bool-attr? attr)
  (boolean? (second attr)))

(define (true-attr? attr)
  (eqv? #t (second attr)))

;; Returns a list of symbols.
(define (bool-attrs-to-qmake-list attrs)
  (map
   (lambda (entry)
     (let ((name (first entry)))
       (name-to-gmake name)))
   (filter true-attr? attrs)))

;; Returns a list of symbols.
(define (bool-attrs-to-qmake-list/with-negates attrs)
  (map
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       ((if value name-to-gmake name-to-gmake/negate) name)))
   (filter bool-attr? attrs)))

(define (display/c value)
  (cond
   ((eqv? value #t)
    (display "1"))
   ((eqv? value #f)
    (display "0"))
   ((number? value)
    (write value))
   ((hexnum? value)
    (display (format "0x~a" (number->string (hexnum-num value) 16))))
   ((string? value)
    (write value))
   ((symbol? value)
    (display/c (symbol->string value)))
   ((list? value)
    (begin
      (display "{")
      (for-each-sep display/c (thunk (display ", ")) value)
      (display "}")))
   (else
    (error "cannot display as C" value))
   ))

(define (display-attr/c name value)
  (disp "#define ~a " (name-to-c name))
  (display/c value)
  (newline))

(define (display/ruby value)
  (cond
   ((eqv? value #t)
    (display "true"))
   ((eqv? value #f)
    (display "false"))
   ((number? value)
    (write value))
   ((hexnum? value)
    (disp "0x~a" (number->string (hexnum-num value) 16)))
   ((string? value)
    (write value))
   ((symbol? value)
    (display/ruby (symbol->string value)))
   ((list? value)
    (begin
      (display "[")
      (for-each-sep display/c (thunk (display ", ")) value)
      (display "]")))
   (else
    (error "cannot display as Ruby" value))
   ))

(define (display-attr/ruby name value)
  (display (name-to-ruby name))
  (display " = ")
  (display/ruby value)
  (newline))

(define (display/gmake value)
  (cond
   ((eqv? value #t)
    (display "true"))
   ((number? value)
    (write value))
   ((string? value)
    (display value))
   ((symbol? value)
    (display/gmake (symbol->string value)))
   ((list? value)
    (for-each-sep display/gmake (thunk (display " ")) value))
   (else
    (error "cannot display as GNU Make" value))
   ))

(define (display-attr/gmake name value)
  (set! name (name-to-gmake name))
  (cond
   ((eqv? value #t)
    (begin (disp-nl "~a := true" name)
           (disp-nl "NOT__~a :=" name)))
   ((eqv? value #f)
    (begin (disp-nl "~a :=" name)
           (disp-nl "NOT__~a := true" name)))
   ((hexnum? value)
    (begin
      (disp-nl "~a__DEC := ~s" name (hexnum-num value))
      (disp-nl "~a__HEX := ~a"
               name (number->string (hexnum-num value) 16))))
   (else
    (begin
      (display name)
      (display " := ")
      (display/gmake value)
      (newline)))
   ))

(define (display-attr/qmake name value)
  (set! name (name-to-gmake name))
  (cond
   ((eqv? value #t)
    (begin (disp-nl "~a = true" name)
           ;;(disp-nl "NOT__~a =" name)
           ))
   ((eqv? value #f)
    (begin (disp-nl "~a =" name)
           ;;(disp-nl "NOT__~a = true" name)
           ))
   ((hexnum? value)
    (begin
      (disp-nl "~a__DEC = ~s" name (hexnum-num value))
      (disp-nl "~a__HEX = ~a"
               name (number->string (hexnum-num value) 16))))
   (else
    (begin
      (display name)
      (display " = ")
      (display/gmake value)
      (newline)))
   ))

(define* (write-c-file file attrs)
  (let ((harness-name (path-h-ifdefy file)))
    (write-changed-file
     file
     (capture
      (display-generated-notice "//")
      (disp-nl "#ifndef ~a" harness-name)
      (disp-nl "#define ~a" harness-name)
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/c name value)
           ))
       attrs)
      (disp-nl "#endif // ~a" harness-name)
      ))))

(define* (write-ruby-file file attrs)
  (begin
    (write-changed-file
     file
     (capture
      (display-generated-notice "#")
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/ruby name value)
           ))
       attrs)
      ))))

(define* (write-gmake-file file attrs)
  (begin
    (write-changed-file
     file
     (capture
      (display-generated-notice "#")
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/gmake name value)
           ))
       attrs)
      ))))

(define* (write-qmake-file file attrs)
  (begin
    (write-changed-file
     file
     (capture
      (display-generated-notice "#")
      (for-each
       (lambda (entry)
         (let ((name (first entry))
               (value (second entry)))
           (display-attr/qmake name value)
           ))
       attrs)
      ;; For convenience, we add all boolean variables (or their
      ;; negations) to the CONFIG variable with the += operator.
      (begin
        (display "CONFIG += ")
        (for-each-sep display (thunk (display " "))
                      (bool-attrs-to-qmake-list attrs))
        (newline))
      ))))

#|

Copyright 2009 Helsinki Institute for Information Technology (HIIT)
and the authors. All rights reserved.

Authors: Tero Hasu <tero.hasu@hut.fi>

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
