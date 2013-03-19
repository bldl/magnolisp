#lang racket

#|

C++ back end.

|#

(require "ast.rkt" "strategy.rkt"
         "util.rkt" "util/system.rkt")

;; Note quite perfect as does not appear to insert line breaks.
;; http://astyle.sourceforge.net/
;; (astyle "void main() { return; }")
(define (astyle s)
  (exe-filter s '("/usr/bin/astyle" "--mode=c"
                  "--lineend=linux" "--style=otbs"
                  "--quiet")))

;; This one does line breaking.
;; Needs a config file, so try
;; uncrustify --update-config > ~/.uncrustify.cfg
;; http://uncrustify.sourceforge.net/
;; (uncrustify "void main() { return; }")
(define (uncrustify s)
  (exe-filter s '("/usr/bin/uncrustify" "-l" "cpp" "-q")))

