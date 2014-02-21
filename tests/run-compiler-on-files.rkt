#lang racket

#|
|#

(require magnolisp/compiler-api rackunit)

(define-syntax (this-source stx)
  (quasisyntax/loc stx (unsyntax (syntax-source stx))))

(define mgl-file-dir
  (let ((this-path (this-source)))
    (define-values (b n mbd)
      (split-path this-path))
    b))

(define (compile-mgl-file fn)
  (check-not-exn
   (thunk
    (let ((st (compile-files fn)))
      (generate-files
       st '((build (gnu-make qmake c ruby))
            (cxx (cc hh)))
       #:out (open-output-nowhere)
       )))
   (format "failed to compile program ~a" fn)))
    
(define (compile-mgl-files)
  (for ((bn (directory-list mgl-file-dir))
        #:when (regexp-match? "^test-.*[.]rkt$" bn))
    (define fn (build-path mgl-file-dir bn))
    (compile-mgl-file fn)))

(compile-mgl-files)
