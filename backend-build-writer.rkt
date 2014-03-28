#lang racket

#|
|#

(require "backend-util.rkt"
         "util.rkt")

;;; 
;;; special values 
;;;

(concrete-struct* hexnum (num) #:transparent)

;;;
;;; local utilities
;;;

(define (disp . args)
  (display (apply format args)))

(define (disp-nl . args)
  (apply disp args) (newline))

;;;
;;; pretty printing
;;;

(define (name-to-c s)
  (string-upcase (string-underscorify s)))

(define (name-to-ruby s)
  (string-append
   "$"
   (string-upcase
    (string-underscorify s))))

(define (name-to-make s)
  (string-upcase (string-underscorify s)))

(define (name-to-make/negate s)
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
       (name-to-make name)))
   (filter true-attr? attrs)))

;; Returns a list of symbols.
(define (bool-attrs-to-qmake-list/with-negates attrs)
  (map
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       ((if value name-to-make name-to-make/negate) name)))
   (filter bool-attr? attrs)))

(define (cannot-display kind value)
  (error 'generate-build-file
         "cannot display as ~a: ~s"
         kind value))

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
    (display (string-underscorify (symbol->string value))))
   ((list? value)
    (begin
      (display "[")
      (for-each-sep display/ruby (thunk (display ", ")) value)
      (display "]")))
   (else
    (error "cannot display as Ruby" value))
   ))

(define (display-attr/ruby name value)
  (display (name-to-ruby name))
  (display " = ")
  (display/ruby value)
  (newline))

(define (display/make kind value)
  (cond
   ((number? value)
    (write value))
   ((string? value)
    (display value))
   ((symbol? value)
    (display value))
   ((list? value)
    (for-each-sep (fix display/make kind) (thunk (display " ")) value))
   (else
    (cannot-display kind value))))

(define (valid-value/gmake? v)
  (cond
   ;; Cannot really express a list with boolean values, since only the
   ;; empty string is false.
   ((and (list? v) (ormap boolean? v)) #f)
   ;; FIXME Do not know which strings are safe to output.
   (else #t)))

(define (display-attr/make name value kind)
  (define assign (if (eq? kind 'gnu-make) " := " " = "))
  (set! name (name-to-make name))
  (cond
   ((eqv? value #t)
    (display name) (display assign) (display "true") (newline))
   ((eqv? value #f)
    (display name) (display assign) (newline))
   ((hexnum? value)
    (define dec-name (format "~a__DEC" name))
    (define hex-name (format "~a__HEX" name))
    (display dec-name) (display assign) (writeln (hexnum-num value))
    (display hex-name) (display assign)
    (displayln (number->string (hexnum-num value) 16)))
   ((valid-value/gmake? value)
    (display name)
    (display assign)
    (display/make kind value)
    (newline))))

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
       (display-attr/make name value 'gnu-make)))
   attrs))

(define (write-qmake-file file attrs)
  (display-generated-notice "#")
  (for-each
   (lambda (entry)
     (let ((name (first entry))
           (value (second entry)))
       (display-attr/make name value 'qmake)))
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

(define* (get-writer-etc kind)
  (define tbl `((c ,write-c-file ".h" "//")
                (gnu-make ,write-gmake-file ".mk" "#")
                (qmake ,write-qmake-file ".pri" "#")
                (ruby ,write-ruby-file ".rb" "#")))
  (define p (assq kind tbl))
  (assert p)
  (values (second p) (third p) (fourth p)))

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
