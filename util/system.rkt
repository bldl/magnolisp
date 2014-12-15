#lang racket/base

#|

|#

(require racket/list racket/port racket/system)

;; E.g.
;; (exe-filter "foo bar" '("/bin/cat"))
;;   =>
;;     "foo bar"
(define (exe-filter in-text cmd)
  (let ((in (open-input-string in-text))
        (out (open-output-string))
        (err (open-output-nowhere)))
    ;; 'r' is something like
    ;; (#f #f 22847 #f #<procedure:control>)
    ;; where the number is the process ID
    ;; and the procedure is the control one
    (let* ((r (apply process*/ports out in err cmd))
           (ctrl (fifth r)))
      (ctrl 'wait)
      (get-output-string out))))

;; Feeds standard input into the system command `cmd`. Function `f`
;; takes an output port as an argument, and should write the input
;; there. Returns the exit code, unless altogether fails to run.
(define (exe-consume-input/exit-code cmd f #:out out #:err err)
  (define-values (in user-out) (make-pipe #f 'exe-in 'out-via-exe-consume-input))
  (define r (apply process*/ports out in err cmd))
  (define ctrl (fifth r))
  (with-handlers ((exn:fail? (lambda (e) 
                               (ctrl 'interrupt)
                               (raise e))))
    (f user-out))
  (close-output-port user-out)
  (ctrl 'wait)
  (define exit-code (ctrl 'exit-code))
  (unless exit-code
    (error 'exe-consume-input 
           "command unexpectedly still running after wait: ~s" cmd))
  exit-code)

;; Feeds standard input into the system command `cmd`. Function `f`
;; takes an output port as an argument, and should write the input
;; there. Throws an exception on failure, including a non-zero exit
;; code, including output from the command if `detail` is 'stdout,
;; 'stderr, or 'both.
(define (exe-consume-input cmd f #:detail [detail #f])
  (define out? (memq detail '(stdout both)))
  (define err? (memq detail '(stderr both)))
  (define out (if out? (open-output-string) (open-output-nowhere)))
  (define err (if err? (open-output-string) (open-output-nowhere)))
  (define exit-code
    (exe-consume-input/exit-code cmd f #:out out #:err err))
  (define (get-detail)
    (with-output-to-string
      (lambda ()
       (when out? (newline) (display (get-output-string out)))
       (when err? (newline) (display (get-output-string err))))))
  (unless (= exit-code 0)
    (error 'exe-consume-input 
           "command exited with error code ~a: ~s~a" exit-code cmd (get-detail))))

;; Gets standard output of the specified command, which is not subject
;; to shell expansion. Returns #f if the command fails.
;; E.g.
;; (exe-std-output/maybe `(,(find-executable-path "clang") "--version"))
(define (exe-std-output/maybe cmd)
  (let ((out (open-output-string))
        (err (open-output-nowhere)))
    (parameterize ((current-output-port out)
                   (current-error-port err))
      (and (apply system* cmd)
           (get-output-string out)))))

(provide exe-filter exe-consume-input exe-std-output/maybe)

#|

Copyright 2012 the authors. All rights reserved.

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
