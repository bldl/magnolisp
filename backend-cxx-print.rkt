#lang racket

#|
|#

(require "app-util.rkt" "ast-magnolisp.rkt" "backend-util.rkt" 
         (rename-in "pp-yield.rkt" [pp pp-y]) "util.rkt")

(define (pp . ds)
  (call-with-output-string
   (lambda (out)
     (define (yield s)
       (display s out))
     (pp-y #:yield yield ds))))

(define (ew-error sym msg v)
  (error sym (string-append msg ": ~s") v))

(define-match-expander produces
  (syntax-rules ()
    ((_ f pat)
     (app f (and (? values) pat)))))

(define* (format-c decl*)
  (let ((decl* (map format-decl decl*)))
    (string-join decl* "\n\n")))

(define (format-defun name modifs type args stmt)
  (pp (add-between (append modifs (list type)) " ")
      " " (symbol->string name) "(" `(in (gr sp ,args)) " )"
      (if (NoBody? stmt)
          ";"
          `((in " {"
                ,(for/list ([x (CxxBlockStat-ss stmt)])
                   `(br ,(format-stat x)))
                ) br "}"))))

(define (format-decl decl)
  (match decl
    ((struct* Include ([kind kind] [s header]))
     (define-values (lq rq)
       (if (eq? kind 'user)
           (values "\"" "\"")
           (values "<" ">")))
     (string-append "#include " lq header rq))
    ((TlVerbatim _ s)
     s)
    ((CxxDefun _ name modifs
               [format-type . produces . type]
               [format-params . produces . args]
               stmt)
     (format-defun name modifs type args stmt))
    ((Proto _ name modifs
            [format-type . produces . type]
            [format-params . produces . args])
     (format-defun name modifs type args the-NoBody))
    (else (ew-error 'format-decl "could not format" else))))

(define (format-sub-stats ss)
  (if (null? ss)
      '(in br "{}")
      `(" {" (in ,(for/list ((s ss))
                   `(br ,(format-stat s)))) br "}")))

(define (format-stat stmt)
  (match stmt
    ((CxxBlockStat _ stmt*)
     (if (null? stmt*)
         "{}"
         `("{"
           (in ,(for/list ((s stmt*))
                  `(br ,(format-stat s))))
           br "}")))
    ((DefVar _ [format-ident . produces . ident]
       [format-type . produces . type]
       [format-expr . produces . expr])
     (list type " " ident " = " expr ";"))
    ((DeclVar _ [format-ident . produces . ident]
       [format-type . produces . type])
     (list type " " ident ";"))
    ((PpCxxIfStat _ [format-expr . produces . test] ts es)
     `("if (" ,test ")"
       ,(format-sub-stats ts)
       ,(and (not (null? es))
             `(,(if (null? ts) 'br " ")
               "else"
               ,(format-sub-stats es)))))
    ((ReturnStat _ [format-expr . produces . expr])
     (list "return " expr ";"))
    ((ReturnStat _ (? VoidStat?))
     (list "return;"))
    ((AssignStat _ [format-expr . produces . x] [format-expr . produces . v])
     (list x " = " v ";"))
    ((Goto _ name)
     (string-append "goto " (format-ident name) ";"))
    ((LabelDef _ name)
     (string-append (format-ident name) ":"))
    ((ExprStat _ [format-expr . produces . expr])
     (list expr ";"))
    (else (ew-error 'format-stat "could not format" else))))

(define (format-custom-literal anno-e typename dat)
  (match-define (list-rest 'literal pp-lst)
    (syntax->datum anno-e))
  
  (for/list ([pp pp-lst])
    (match pp
      [(? string? s) 
       s]
      [(list fmt obj)
       (define f
         (match fmt
           ['display ~a]
           ['write ~s]
           ['print ~v]
           ['cxx-str (compose string->narrow-cxx-string-literal ~a)]
           [_ (raise-language-error
               #f "illegal literal formatting style directive"
               anno-e fmt)]))
       (define v
         (match obj
           ['datum dat]
           ['type-id typename]
           [(? string?) obj]
           [_ (raise-language-error
               #f "illegal literal formatting object specifier"
               anno-e obj)]))
       (f v)]
      [_ (raise-language-error
          #f "illegal literal formatting item"
          anno-e pp)])))

(define (format-expr expr)
  (match expr
    ((IfExpr _ [format-expr . produces . test]
             [format-expr . produces . conseq]
             [format-expr . produces . alt])
     `("(" ,test (in (gr " ?" sp ,conseq " :" sp ,alt ")"))))
    ((Var _ var) (symbol->string var))
    ((Literal (app (lambda (h) (hash-ref h 'literal #f)) anno-e) dat)
     #:when anno-e
     (match-define (ForeignNameT _ tn-stx) (Expr-type expr))
     (format-custom-literal anno-e (syntax-e tn-stx) dat))
    ((Literal _ (? number? n))
     (number->string n))
    ((Literal _ (? boolean? b))
     (if (not b) "false" "true"))
    ((Literal _ (? string? s))
     (string->narrow-cxx-string-literal s))
    ((Literal _ (? symbol? sym))
     `("mgl_SYMBOL(" ,(string->narrow-cxx-string-literal 
                       (symbol->string sym)) ")"))
    ((ApplyExpr a f [format-args . produces . args])
     (define ts (hash-ref a 'type<> #f))
     (list (format-expr f)
           (and ts `("<" (in (gr ,(add-between 
                                   (map format-type ts) 
                                   '("," sp)))) 
                     ">"))
           "(" `(in (gr ,args)) ")"))
    ((AssignExpr _ [format-expr . produces . x] [format-expr . produces . v])
     (list "(" x " = " v ")"))
    ((SeqExpr _ es)
     (define xs (add-between (map format-expr es) '("," sp)))
     (list "(" `(in (gr ,xs)) ")"))
    ((VoidExpr _)
     "(void)0")
    (else (ew-error 'format-expr "could not format" else))))

(define (mangle-ident x)
  (let ((y ""))
    (define (push c)
      (set! y (string-append y c)))
    (for ((c (in-string x)))
      (case c
        ((#\-) (push "$"))
        ((#\$) (push "$$"))
        ((#\.) (push "$_"))
        ((#\$) (push "$$$"))
        ((#\>) (push "$v"))
        ((#\!) (push "$b"))
        (else (push (string c)))))
    y))

(define (format-ident ident)
  (unless (symbol? ident)
    (ew-error 'format-ident "could not format" ident))
  (let ((reserved-words '(complex)))
    (if (memq ident reserved-words)
        (string-append "$$" (symbol->string ident))
        (mangle-ident (symbol->string ident)))))

(define (format-type t)
  (match t
    ((NameT _ (? symbol? s))
     (symbol->string s))
    ((ForeignNameT _ (? identifier? s))
     (symbol->string (syntax-e s)))
    ((ConstT _ t)
     (list (format-type t) " const"))
    ((RefT _ t)
     (list (format-type t) "&"))
    ((ParamT _ t ps)
     `(,(format-type t) "<"
       ,(add-between (map format-type ps) '("," sp))
       ">"))
    (else (ew-error 'format-type "could not format" else))))

(define (format-params args)
  (add-between (map format-param args) '("," sp)))

(define (format-args args)
  (add-between (map format-expr args) '("," sp)))

(define (format-param arg)
  (match arg
    ((Param _ n [format-type . produces . t])
     (list t " " (symbol->string n)))
    (arg (ew-error 'format-param "could not format" arg))))

(define (string-pad s up-to-len pad-ch)
  (define len (string-length s))
  (if (>= len up-to-len)
      s
      (let ((n (- up-to-len len)))
        (string-append (make-string n pad-ch) s))))

(define (char->octal-escape c)
  (define s (format "~o" (char->integer c)))
  (list "\\" (string-pad s 3 #\0)))

(define (string->narrow-cxx-string-literal s)
  (list
   "\""
   (for/list ([c (in-string s)])
     (cond
       [(char=? c #\newline) "\\n"]
       [(char=? c #\return) "\\r"]
       [(char=? c #\tab) "\\t"]
       [(char=? c #\\) "\\\\"]
       [(char=? c #\") "\\\""]
       [(char-iso-control? c) (char->octal-escape c)]
       [else
        (define n (char->integer c))
        (when (> n 127)
          (raise-argument-error
           'string->narrow-cxx-string-literal
           "ASCII string" s))
        c]))
    "\""))

#|

The contents of this file are derived from the print-c.scm file of
Elegant Weapons. Any changes to this file are copyright University of
Bergen and the authors, and the following license applies to this
file.

Copyright (c) 2013-2014 Tero Hasu and University of Bergen

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
