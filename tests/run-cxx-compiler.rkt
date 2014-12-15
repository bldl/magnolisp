#lang racket

#|

A test case runner for processing Magnolisp test files by compiling
them into C++, compiling the C++ into an executable, running the
executable, and then comparing actual output against expected output.

|#

(require (relative-in magnolisp 
                      "compiler-api.rkt" "util.rkt"
                      "util/system.rkt")
         data/order rackunit)

(define datum<? (order-<? datum-order))

(define (query-clang-version exe)
  (let-and s (exe-std-output/maybe (list exe "--version"))
    (let ((re #px"^clang[[:space:]]+version[[:space:]]+([[:digit:]]+)[.]([[:digit:]]+)[.]([[:digit:]]+)[[:space:]]+"))
      (let-and m (regexp-match re s)
        (map string->number (take (cdr m) 3))))))
    
(define-syntax (this-source stx)
  (quasisyntax/loc stx (unsyntax (syntax-source stx))))

(define mgl-file-dir
  (let ((this-path (this-source)))
    (define-values (b n mbd)
      (split-path this-path))
    b))

;; Like `delete-file`, but does not throw if the file does not exist.
(define (maybe-delete-file fn)
  (with-handlers ((exn:fail:filesystem? void))
    (delete-file fn)))

;; The provided cleanup `action` expression should not fail. Produces
;; the result of `body`, or throws an exception. Always executes
;; `action` as the last one.
(define-syntax-rule (ensure body #:finally action)
  (let ((cleanup (lambda () action)))
    (with-handlers ((exn:fail? (lambda (e) 
                                 (cleanup) 
                                 (raise e))))
      (begin0 
        body
        (cleanup)))))

(define (with-temporary-file f)
  (let ((fn (make-temporary-file)))
    (ensure (f fn) #:finally (maybe-delete-file fn))))

(define show-cxx-code? (make-parameter #f))

(define (compile-and-run-one-mgl-file cc-fun fn)
  (define st (compile-files fn))

  (define expected 
    (let ((v (get-expected-anno-value st)))
      (if v
          (syntax->datum v)
          (error 'compile-and-run-one-mgl-file
                 "test program ~a is missing `expected` annotation" fn))))
  
  (define preamble (build-path mgl-file-dir "cxx-preamble.cc"))
  (define postamble (build-path mgl-file-dir "cxx-postamble.cc"))

  (define (generate out)
    (call-with-input-file preamble (lambda (in) (copy-port in out)))
    (generate-files st '((cxx (cc))) #:banner #f #:out out)
    (call-with-input-file postamble (lambda (in) (copy-port in out))))
  
  (when (show-cxx-code?)
    (generate (current-output-port)))

  (define (cc-and-run a.out)
    (cc-fun a.out generate)
    (or (exe-std-output/maybe (list a.out))
        (error 'compile-and-run-one-mgl-file
               "running of compiled test program ~a for ~a failed" a.out fn)))
  
  (define out-str
    (with-temporary-file cc-and-run))
  
  (define actual 
    (for/list ((dat (in-port read (open-input-string out-str))))
      dat))

  (values actual expected))
  
(define (compile-and-run-mgl-files [list-files
                                    (thunk (directory-list mgl-file-dir))])
  (define cc-fun ;; (-> path-string? (-> output-port? any/c) void?)
    (let-and exe (find-executable-path "clang")
      (let-and ver (query-clang-version exe)
        (and (not (datum<? ver '(3 5 0)))
             (lambda (a.out generate)
               (let ((cmd `(,exe "-x" "c++" "-std=c++14" 
                            "-o" ,a.out "-lstdc++" "-")))
                 (exe-consume-input cmd generate #:detail 'stderr)))))))
  
  ;; Run no tests without a suitable compiler.
  (when cc-fun
    (for ((bn (list-files))
          #:when (regexp-match-exact? #rx"test-run-.*[.]rkt" bn))
      (define fn (build-path mgl-file-dir bn))
      (with-handlers ((exn:fail?
                       (lambda (e)
                         (fail 
                          (format 
                           "run-via-C++ test: failed with ~s for ~a"
                           e bn)))))
        (let-values (((actual expected)
                      (compile-and-run-one-mgl-file cc-fun fn)))
          (check-equal? 
           actual expected
           (format "run-via-C++ test: un`expected` output for ~a" bn)))))))

;;(parameterize ((show-cxx-code? #f)) (compile-and-run-mgl-files (thunk '("test-run-sum-3.rkt"))))

(module* test #f
  (compile-and-run-mgl-files))
