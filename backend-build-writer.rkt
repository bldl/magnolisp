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

(define (display-divider n [pfx #f] #:cols [cols #f])
  (define margin (if pfx (+ (string-length pfx) 1) 0))
  (define s-lst
    (for/list ((i (in-range margin (+ n 1))))
      (if cols (format "~a" (modulo i 10)) "-")))
  (when pfx
    (display pfx) (display " "))
  (displayln (apply string-append s-lst)))

(define (display-banner pfx filename)
  (define n (let ((col (pretty-print-columns)))
              (if (exact-positive-integer? col)
                  col 40)))
  (display-divider n pfx)
  (display pfx)
  (display " ")
  (displayln filename)
  (display-divider n pfx #:cols #t))

(define (display-generated-notice pfx)
  (display pfx)
  (displayln " generated -- do not edit"))

(define path-censor-re #rx"[^a-z0-9_]")

(define-with-contract*
  (-> path-string? string?)
  (path-h-ifdefy p)
  (string-append
   "__"
   (regexp-replace* path-censor-re
                    (string-downcase (path-basename-as-string p)) "_")
   "__"))

(define (string-underscorify s)
  (regexp-replace* #rx"[-]" s "_"))

(define (name-to-c s)
  (string-upcase (string-underscorify s)))

(define (name-to-ruby s)
  (string-append
   "$"
   (string-upcase
    (string-underscorify s))))

(define (name-to-gmake s)
  (string-upcase (string-underscorify s)))

(define (name-to-gmake/negate s)
  (string-append
    "NOT__"
   (string-upcase
    (string-underscorify s))))

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
    (display ":")
    (display value))
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
    (disp-nl "~a := true" name))
   ((eqv? value #f)
    (disp-nl "~a :=" name))
   ((hexnum? value)
    (disp-nl "~a__DEC := ~s" name (hexnum-num value))
    (disp-nl "~a__HEX := ~a"
             name (number->string (hexnum-num value) 16)))
   (else
    (display name)
    (display " := ")
    (display/gmake value)
    (newline))))

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

;;; 
;;; language-specific writers
;;;

(define (write-c-file file attrs)
  (let ((harness-name (path-h-ifdefy file)))
    (display-generated-notice "//")
    (disp-nl "#ifndef ~a" harness-name)
    (disp-nl "#define ~a" harness-name)
    (for-each
     (lambda (entry)
       (let ((name (first entry))
             (value (second entry)))
         (display-attr/c name value)))
     attrs)
    (disp-nl "#endif // ~a" harness-name)))

(define (write-ruby-file file attrs)
  (display-generated-notice "#")
  (for-each
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       (display-attr/ruby name value)))
   attrs))

(define (write-gmake-file file attrs)
  (display-generated-notice "#")
  (for-each
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       (display-attr/gmake name value)))
   attrs))

(define (write-qmake-file file attrs)
  (display-generated-notice "#")
  (for-each
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       (display-attr/qmake name value)))
   attrs)
  ;; For convenience, we add all boolean variables (or their
  ;; negations) to the CONFIG variable with the += operator.
  (begin
    (display "CONFIG += ")
    (for-each-sep display (thunk (display " "))
                  (bool-attrs-to-qmake-list attrs))
    (newline)))

;;; 
;;; driver routines
;;;

(define (get-writer-etc kind)
  (define tbl `((c ,write-c-file ".h" "//")
                (gnu-make ,write-gmake-file ".mk" "#")
                (qmake ,write-qmake-file ".pri" "#")
                (ruby ,write-ruby-file ".rb" "#")))
  (define p (assq kind tbl))
  (assert p)
  (values (second p) (third p) (fourth p)))

(define (write-generated-output path stdout? writer)
  (if stdout?
      (writer)
      (write-changed-file
       path
       (capture-output writer))))

(define-with-contract*
  (-> symbol? list? path-string? boolean? boolean? void?)
  (generate-build-file kind attrs path-stem stdout? banner?)

  (define-values (writer sfx pfx) (get-writer-etc kind))
  (define path (path-add-suffix path-stem sfx))
  (define filename (path-basename-as-string path))
  
  (write-generated-output
   filename stdout?
   (thunk
    (when banner?
      (display-banner pfx filename))
    (writer path attrs)))

  (void))
  
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
