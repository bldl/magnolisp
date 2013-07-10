#!/bin/sh
#| # -*- scheme -*-
exec racket -e '(begin (putenv "COMPILE_IT" "true") (void))' -u "$0" -- ${1+"$@"}
|#

#lang racket

#|
|#

(require "util.rkt")
