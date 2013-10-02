#lang racket

#|

Implements a command-line interface (CLI) for the Magnolisp compiler.

|#

(require "compiler-api.rkt" "util.rkt")

;;; 
;;; command-line interface
;;; 

(define (adjust-path path)
  (simplify-path (expand-user-path path)))

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
       #:args filename filename))

    (unless (null? fn-lst)
      (set! fn-lst (map adjust-path fn-lst))
      (define base-fn (first fn-lst))
      (define-syntax-rule (set!-out-fn fn suffix)
        (set! fn (if fn
                     (adjust-path fn)
                     (path-replace-suffix base-fn suffix))))
      (set!-out-fn cpp-file ".cpp")
      (set!-out-fn hpp-file ".hpp")
      (set!-out-fn mk-file ".mk")
      (define st (new-state))
      (for ((fn fn-lst))
        (set! st (compile-file st fn)))
      (write-cpp-file st cpp-file)
      (write-hpp-file st hpp-file)
      (write-mk-file st mk-file))
    
    (void))

  (do-main))
