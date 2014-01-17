#lang racket

#|
|#

(require "ast-magnolisp.rkt" "backend-util.rkt"
         "pgf.rkt" "util.rkt")

(define (join sep lst)
  (string-join lst sep))

(define (ew-error sym msg v)
  (error sym (string-append msg ": ~s") v))

(define-match-expander produces
  (syntax-rules ()
    ((_ f pat)
     (app f (and (? values) pat)))))

(define ew-indent (make-parameter 0))

(define (push-indent) (ew-indent (+ (ew-indent) 1)))
(define (pop-indent) (ew-indent (- (ew-indent) 1)))

(define-syntax indent-more
  (syntax-rules ()
    ((_ expr)
     (parameterize ((ew-indent (+ 1 (ew-indent)))) expr))))

(define (indent-before str)
  (let loop ((i (ew-indent)))
    (if (zero? i) str (string-append "  " (loop (- i 1))))))

(define* (format-c decl*)
  (let ((decl* (map format-decl decl*)))
    (string-append (join "\n\n" decl*) "\n")))

(define (format-defun name modifs type args [stmt* null])
  (string-append (space-join (append modifs (list type)))
                 " " (symbol->string name) "(" args ")"
                 (if (null? stmt*)
                     ";\n"
                     (string-append
                      " {\n"
                      (join "\n" (indent-more (map format-stat stmt*)))
                      "\n}\n"))))

(define (format-decl decl)
  (match decl
    ((struct* Include ([kind kind] [s header]))
     (define-values (lq rq)
       (if (eq? kind 'user)
           (values "\"" "\"")
           (values "<" ">")))
     (string-append "#include " lq header rq "\n\n"))
    ((TlVerbatim _ s)
     (string-append s "\n"))
    ((CxxDefun _ name modifs
               [format-type . produces . type]
               [format-params . produces . args] stmt*)
     (format-defun name modifs type args stmt*))
    ((Proto _ name modifs
            [format-type . produces . type]
            [format-params . produces . args])
     (format-defun name modifs type args))
    ;; ((extern ,[format-type . produces . type] ,[format-ident . produces . name]
    ;;          (,[format-type . produces . args] ...))
    ;;  (string-append type " " name "(" (join ", " args) ");\n"))
    ;; ((typedef ,[format-ident . produces . name] ,[format-type . produces . type])
    ;;  (string-append "typedef " type " " name " ;\n"))
    (else (ew-error 'format-decl "could not format" else))))

(define (format-stat stmt)
  (match stmt
    ((BlockStat _ stmt*)
     (string-append
      (indent-before "{\n")
      (join "\n" (indent-more (map format-stat stmt*)))
      "\n"
      (indent-before "}")))
    ((DefVar _ [format-ident . produces . ident]
       [format-type . produces . type]
       [format-expr . produces . expr])
     (indent-before
      (string-append type " " ident " = " expr ";")))
    ((CxxDeclVar _ [format-ident . produces . ident]
       [format-type . produces . type])
     (indent-before
      (string-append type " " ident ";")))
    ;; ((let ,[format-ident . produces . ident] (fixed-array ,[format-type . produces . type] ,i)
    ;;       ,[format-expr . produces . expr])
    ;;  (indent-before
    ;;   (string-append type " " ident "[" (number->string i) "] = " expr ";")))
    ;; ((let ,[format-ident . produces . ident] ,[format-type . produces . type]
    ;;       ,[format-expr . produces . expr])
    ;;  (indent-before
    ;;   (string-append type " " ident " = " expr ";")))
    ;; ((let ,[format-ident . produces . ident] ,[format-type . produces . type])
    ;;  (indent-before
    ;;   (string-append type " " ident ";")))
    ;; ((if ,[format-expr . produces . test] ,conseq)
    ;;  (string-append
    ;;   (indent-before (string-append "if(" test ")\n"))
    ;;   (indent-more (format-stat conseq))))
    ;; ((if ,[format-expr . produces . test] ,conseq ,alt)
    ;;  (string-append
    ;;   (indent-before (string-append "if(" test ")\n"))
    ;;   (indent-more (format-stat conseq))
    ;;   "\n"
    ;;   (indent-before "else\n")
    ;;   (indent-more (format-stat alt))))
    ((Return _ [format-expr . produces . expr]) ;; xxx temporary
     (indent-before (string-append "BLOCK_ESCAPE " expr ";")))
    ((CxxReturnOne _ [format-expr . produces . expr])
     (indent-before (string-append "return " expr ";")))
    ((Assign _ [format-expr . produces . x] [format-expr . produces . v])
     (indent-before
      (string-append x " = " v ";")))
    ;; ((vector-set! ,[format-expr . produces . vec-expr]
    ;;               ,[format-expr . produces . i-expr] ,[format-expr . produces . val-expr])
    ;;  (indent-before
    ;;   (string-append vec-expr "[" i-expr "] = " val-expr ";")))
    ((Goto _ name)
     (indent-before (string-append "goto " (format-ident name) ";")))
    ((GccLabelDecl _ name)
     (string-append "__label__ " (format-ident name) ";"))
    ((CxxLabel _ name)
     (string-append (format-ident name) ":"))
    ;; ((while ,[format-expr . produces . expr] ,stmt)
    ;;  (string-append
    ;;   (indent-before (string-append "while(" expr ")\n"))
    ;;   (indent-more (format-stat stmt))))
    ;; ((for (,[format-ident . produces . i]
    ;;        ,[format-expr . produces . start]
    ;;        ,[format-expr . produces . end])
    ;;    ,stmt)
    ;;  (string-append
    ;;   (indent-before
    ;;    (string-append
    ;;     "for(int " i " = " start "; " i " < " end "; ++" i ")\n"))
    ;;   (indent-more (format-stat stmt))))
    ;; ((for (,[format-ident . produces . i]
    ;;        ,[format-expr . produces . start]
    ;;        ,[format-expr . produces . end]
    ;;        ,[format-expr . produces . step])
    ;;    ,stmt)
    ;;  (string-append
    ;;   (indent-before
    ;;    (string-append
    ;;     "for(int " i " = " start "; " i " < " end "; " i "= (" i " + " step "))\n"))
    ;;   (indent-more (format-stat stmt))))
    ;; ((do ,[format-expr . produces . e])
    ;;  (indent-before (string-append e ";")))
    (else (ew-error 'format-stat "could not format" else))))

(define (format-expr expr)
  (match expr
    ((GccStatExpr _ ss e)
     (string-append
      "({ "
      (join "\n" (append (map format-stat ss)
                         (list (string-append (format-expr e) ";"))))
      " })"))
    ;; ((empty-struct) "{0}")
    ;; ((field ,[obj] ,x)
    ;;  (string-append obj "." (format-ident x)))
    ;; ((field ,[obj] ,x ,[format-type . produces . t])
    ;;  (string-append obj "." (format-ident x) "<" t ">"))
    ;; ((if ,[format-expr . produces . test]
    ;;      ,[format-expr . produces . conseq]
    ;;      ,[format-expr . produces . alt])
    ;;  (string-append "(" test ") ? (" conseq ") : (" alt ")"))
    ;; ((vector-ref ,[format-expr . produces . v]
    ;;              ,[format-expr . produces . i])
    ;;  (string-append v "[" i "]"))
    ;; ((sizeof ,[format-type . produces . t])
    ;;  (string-append "sizeof(" t ")"))
    ;; ((deref ,[format-expr . produces . p])
    ;;  (string-append "*" p))
    ;; ((cast ,[format-type . produces . t] ,[e])
    ;;  (string-append "((" t ")(" e "))"))
    ;; ((addressof ,[format-expr . produces . e])
    ;;  (string-append "(&(" e "))"))
    ;; ((,op ,[format-expr . produces . lhs] ,[format-expr . produces . rhs])
    ;;  (guard (binop? op))
    ;;  (string-append "(" lhs ") " (format-binop op) " (" rhs ")"))
    ;; ((,op ,[format-expr . produces . lhs] ,[format-expr . produces . rhs])
    ;;  (guard (relop? op))
    ;;  (string-append "(" lhs ") " (format-relop op) " (" rhs ")"))
    ;; ((not ,[format-expr . produces . lhs])
    ;;  (string-append "!(" lhs ")"))
    ((Var _ var) (symbol->string var))
    ;; ((char ,c) (format-char-literal c))
    ((Literal _ (? number? n))
     (number->string n))
    ((Literal _ (? boolean? b))
     (if (not b) "false" "true"))
    ((Literal _ (? string? s))
     (string-append "\"" (escape-string-literal s) "\""))
    ;; ((c-expr ,x) (symbol->string x))
    ((Apply _ f [format-args . produces . args])
     (string-append (format-expr f) "(" args ")"))
    (else (ew-error 'format-expr "could not format" else))))

(require (only-in rnrs/base-6 string-for-each))

(define (mangle-ident x)
  (let ((y ""))
    (define (push c)
      (set! y (string-append y c)))
    (string-for-each
     (lambda (c)
       (case c
         ((#\-) (push "$"))
         ((#\$) (push "$$"))
         ((#\.) (push "$_"))
         ((#\$) (push "$$$"))
         ((#\>) (push "$v"))
         ((#\!) (push "$b"))
         (else (push (string c)))))
     x)
    y))

(define (format-ident ident)
  (unless (symbol? ident)
    (ew-error 'format-ident "could not format" ident))
  (let ((reserved-words '(complex)))
    (if (memq ident reserved-words)
        (string-append "$$" (symbol->string ident))
        (mangle-ident (symbol->string ident)))))

(define (format-type t)
  ;;(writeln t)
  (match t
    ((CxxNameT _ (? symbol? s))
     (symbol->string s))
    ((ConstT _ t)
     (string-append (format-type t) " const"))
    ((RefT _ t)
     (string-append (format-type t) "&"))
    ;; (u64 "uint64_t")
    ;; ((ptr ,[t])
    ;;  (string-append t " __global *"))
    ;; ((fixed-array ,t ,i)
    ;;  (ew-error 'format-type
    ;;         "Directly formatting fixed-size arrays is a bad idea."
    ;;         `(fixed-array ,t ,i)))
    ;; ((struct (,[format-ident . produces . x] ,[t]) ...)
    ;;  (string-append "struct {\n"
    ;;                 (indent-more
    ;;                  (join "" (map (lambda (x t)
    ;;                                  (indent-before
    ;;                                   (string-append t " " x ";\n")))
    ;;                                x t)))
    ;;                 "}"))
    ;; ((union (,[format-ident . produces . x] ,[t]) ...)
    ;;  (string-append "union {\n"
    ;;                 (indent-more
    ;;                  (join "" (map (lambda (x t)
    ;;                                  (indent-before
    ;;                                   (string-append t " " x ";\n")))
    ;;                                x t)))
    ;;                 "}"))
    ;; ((,[t] ,[t*] ...)
    ;;  (if (null? t*)
    ;;      t
    ;;      (string-append t "< " (join ", " t*) " >")))
    ;; (,x (guard (symbol? x))
    ;;     (format-ident x))
    (else (ew-error 'format-type "could not format" else))))

(define (format-params args)
  (join ", " (map format-param args)))

(define (format-args args)
  (join ", " (map format-expr args)))

(define (format-param arg)
  (match arg
    ((CxxParam _ n [format-type . produces . t])
     (string-append t " " (symbol->string n)))
    (arg (ew-error 'format-param "could not format" arg))))

(define (format-binop op)
  (case op
    ((bitwise-or) "|")
    ((+) "+")
    ((*) "*")
    ((-) "-")
    ((/) "/")
    ((mod) "%")
    (else (ew-error 'format-binop "could not format" op))))

(define (format-relop op)
  (case op
    ((== =) "==")
    ((<) "<")
    ((>) ">")
    ((<=) "<=")
    ((>=) ">=")
    (else (ew-error 'format-relop "could not format" op))))

(define (escape-string-literal s)
  (if (zero? (string-length s))
      ""
      (string-append
       (case (string-ref s 0)
         ((#\newline) "\\n\"\n\"")
         ((#\") "\\\"")
         (else (string (string-ref s 0))))
       (escape-string-literal
        (substring s 1 (string-length s))))))

(define (format-char-literal c)
  (string-append "'"
                 (case c
                   ((#\nul) "\\0")
                   (else (string c)))
                 "'"))

#|

The contents of this file are derived from the print-c.scm file of
Elegant Weapons. Any changes to this file are copyright University of
Bergen and the authors, and the following license applies to this
file.

Copyright (c) 2013      University of Bergen

Copyright (c) 2011-2013 The Trustees of Indiana University and Indiana
                        University Research and Technology
                        Corporation.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

- Redistributions of source code must retain the above copyright
  notice, this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright
  notice, this list of conditions and the following disclaimer listed
  in this license in the documentation and/or other materials
  provided with the distribution.

- Neither the name of the copyright holders nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

The copyright holders provide no reassurances that the source code
provided does not infringe any patent, copyright, or any other
intellectual property rights of third parties.  The copyright holders
disclaim any liability to any recipient for claims brought against
recipient by any third party for infringement of that parties
intellectual property rights.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

|#
