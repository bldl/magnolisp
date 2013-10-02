#lang racket

#|

Implements a command-line interface (CLI) for the Magnolisp compiler.

|#

(require "compiler-api.rkt" "util.rkt")

;;; 
;;; command-line interface
;;; 

(module* main #f
  (define (do-main)
    (define hpp-file #f)
    (define cpp-file #f)
    (define mk-file #f)

    (define fn-lst
      (command-line
       #:once-each
       (("--chdir") dirname "change to directory"
        (current-directory (expand-user-path dirname)))
       (("--cpp") filename "generated C++ implementation"
        (set! cpp-file filename))
       (("--hpp") filename "generated C++ header"
        (set! hpp-file filename))
       (("--mk") filename "generated Make include file"
        (set! mk-file filename))
       #:args lst lst))

    (unless (null? fn-lst)
      (void)) ;; xxx
    
    (void))

  (do-main))
