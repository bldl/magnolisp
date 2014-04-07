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
    (define out-dir #f)
    (define out-basename #f)
    (define stdout? #f)
    (define banner? #f)
    (define cxx? #f)
    (define tools null)
    (define supported-tools '(c gnu-make qmake ruby))

    (define fn-lst
      (command-line
       #:once-each
       (("--banner" "-n") ("display filename banners"
                           "(only meaningful with --stdout)")
        (set! banner? #t))
       (("--basename" "-b") basename
        ("for naming generated files"
         "(default: basename of first source file)")
        (set! out-basename basename))
       (("--chdir") dirname "change to directory"
        (current-directory (expand-user-path dirname)))
       (("--cxx" "-c") "generate C++ implementation"
        (set! cxx? #t))
       (("--outdir" "-o") dirname "output directory"
        (set! out-dir (path->directory-path (adjust-path dirname))))
       (("--stdout" "-s") "output to stdout"
        (set! stdout? #t))
       #:multi
       (("--build" "-m") kind ("generate build include file of kind"
                               (format "(kind is one of: ~a)"
                                       (string-join
                                        (map symbol->string supported-tools)
                                        ", ")))
        (define k (string->symbol kind))
        (unless (memq k supported-tools)
          (error 'command-line
                 "one of ~a as build include file, got ~s"
                 supported-tools kind))
        (set! tools (cons k tools)))
       #:args filename filename))

    (unless (null? fn-lst)
      (set! fn-lst (map adjust-path fn-lst))
      (unless out-basename
        (set! out-basename
              (path-basename-only-as-string (first fn-lst))))
      (define st (apply compile-files fn-lst))
      (generate-files st
                      (filter identity
                       (list (and cxx? '(cxx (cc hh)))
                             (and (pair? tools) `(build ,tools))))
                      #:outdir (or out-dir (current-directory))
                      #:basename out-basename
                      #:out (and stdout? (current-output-port))
                      #:banner banner?))
    
    (void))

  (do-main))
