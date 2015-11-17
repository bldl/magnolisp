#lang racket/base

#|

Implements a command-line interface (CLI) for the Magnolisp compiler.

|#

(require racket/cmdline racket/dict racket/function
         racket/list racket/string
         "compiler-api.rkt" "util.rkt")

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
    (define mgl? #f)
    (define tools #f)
    (define supported-tools '(c gnu-make qmake ruby))
    (define dont-touch? #f)
    (define backends null)

    (define (raise-usage-error fmt . vs)
      (apply raise-user-error 'mglc fmt vs))
    
    (define fn-lst
      (command-line
       #:once-each
       (("--backends" "-B") sexp "use back ends as specified"
        (set! backends (read (open-input-string sexp)))
        (unless (matches? backends
                  (list (cons (or 'build 'cxx 'mgl) _) ...))
          (raise-usage-error
           "expected an alist for --backends sexp: ~s"
           backends)))
       (("--banner" "-n") ("display filename banners"
                           "(only meaningful with --stdout)")
        (set! banner? #t))
       (("--name" "--basename") basename
        ("for naming generated files"
         "(default: basename of first source file)")
        (set! out-basename basename))
       (("--chdir") dirname "change to directory"
        (current-directory (expand-user-path dirname)))
       (("--cxx" "-c") "generate C++ implementation"
        (set! cxx? #t))
       (("--dont-touch") "don't overwrite unmodified files"
        (set! dont-touch? #t))
       (("--mgl" "-l") "generate IR dump as Magnolisp"
        (set! mgl? #t))
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
          (raise-usage-error
           "expected one of ~a as build include file: ~s"
           supported-tools kind))
        (set! tools (if tools (cons k tools) (list k))))
       #:args filename filename))

    (let ()
      (define (add! sexp)
        (set! backends (cons sexp backends)))
      (when mgl? (add! '(mgl)))
      (when cxx? (add! '(cxx (parts cc hh))))
      (when tools (add! `(build (targets ,@tools)))))

    (unless (null? fn-lst)
      (set! fn-lst (map adjust-path fn-lst))
      (unless out-basename
        (set! out-basename
              (path-basename-only-as-string (first fn-lst))))
      (define st (apply compile-files fn-lst))
      (generate-files st backends
                      #:outdir (or out-dir (current-directory))
                      #:basename out-basename
                      #:out (and stdout? (current-output-port))
                      #:dont-touch dont-touch?
                      #:banner banner?))
    
    (void))

  (do-main))
