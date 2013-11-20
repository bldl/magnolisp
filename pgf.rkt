#lang racket

#|

A pretty printer library derived from code in Pretty Good Formatter.
Based on Wadler's algorithm, with some extensions.

https://github.com/nuthatchery/pgf

|#

(require "util.rkt")

;;; 
;;; indentation levels
;;; 

(abstract-struct* Lv () #:transparent)

(define-syntax-rule (define-level* n fld-spec ...)
  (concrete-struct* n Lv (fld-spec ...) #:transparent))

(define-level* LvInc n) ;; integer -> Lv
(define-level* LvStr s) ;; string -> Lv
(define-level* LvAbs n) ;; integer -> Lv
(define-level* LvRel n) ;; integer -> Lv
(define-level* LvPop) ;; -> Lv

;;; 
;;; tokens
;;; 

(abstract-struct* Token () #:transparent)

(define-syntax-rule (define-token* n fld-spec ...)
  (concrete-struct* n Token (fld-spec ...) #:transparent))

(define-token* Text s) ;; unbreakable text
(define-token* Line) ;; forced line break
(define-token* Nest lv) ;; nest open or close
(define-token* Union l r) ;; choice
(define-token* Width w) ;; 'w' is a rational
(define-token* Flush) ;; forces flushing of buffers

;;; 
;;; token sequences
;;; 

(require "util/bankers-deque.rkt")

(define* (tseq? x)
  (or (dq? x)
      (null? x) (pair? x)
      (Token? x) (string? x)
      (promise? x)))

(define* tseq-null dq-null)

;; Optimizes for the common, performance critical case, namely
;; prepending or appending a token into a sequence using the
;; constructor.
(define* tseq
  (case-lambda
    [() tseq-null]
    [(s) (dq s)]
    [(s1 s2) (cond
              ((and (dq? s1) (dq? s2))
               (dq-append s1 s2))
              ((dq? s1)
               (dq-cons-r s1 s2))
              ((dq? s2)
               (dq-cons-f s2 s1))
              ((null? s1)
               (dq s2))
              ((null? s2)
               (dq s1))
              (else
               (dq s1 s2)))]
    [ss (apply dq ss)]))

(define* (tseq-cons e s)
  (cond
   ((dq? s) (dq-cons-f s e))
   ((null? s) (dq e))
   (else (dq e s))))

(define* (tseq-put s e)
  (cond
   ((dq? s) (dq-cons-r s e))
   ((null? s) (dq e))
   (else (dq s e))))

(define (tseq-put* s . es)
  (for/fold ((s s)) ((e es)) (tseq-put s e)))

(define-syntax-rule* (tseq/lazy s ...)
  (tseq (lazy s) ...))

(define-syntax-rule* (tseq-cons/lazy e s)
  (tseq-cons (lazy e) (lazy s)))

(define-syntax-rule* (tseq-put/lazy s e)
  (tseq-put (lazy s) (lazy e)))

;; Must account for all tseq constructors.
(define* (tseq-get s)
  (define (f h t)
    (if (not h)
        (tseq-get t) ;; allow #f within a dq or list
        (let-values (((hh ht) (tseq-get h)))
          (if (not hh)
              (tseq-get t)
              (values hh (cons ht t))))))
  
  (cond
   ((pair? s) (f (car s) (cdr s)))
   ((dq? s) (if (dq-null? s)
                (values #f tseq-null)
                (call-with-values (thunk (dq-pop-f s)) f)))
   ((null? s) (values #f tseq-null))
   ((Token? s) (values s tseq-null))
   ((string? s) (values (Text s) tseq-null))
   ((promise? s) (tseq-get (force s)))
   (else
    (raise-argument-error 'tseq-get "tseq" s))))

(define* (tseq-first s)
  (let-values (((e r) (tseq-get s))) e))

(define* (tseq-rest s)
  (let-values (((e r) (tseq-get s)))
    (unless e
      (raise-argument-error 'tseq-rest "non-empty tseq" s))
    r))

(define* (tseq-null? s)
  (not (tseq-first s)))

(define* (tseq->list s)
  (let loop ((r '()) (s s))
    (let-values (((h t) (tseq-get s)))
      (if (not h) (reverse r)
          (loop (cons h r) t)))))

(define* (tseq-for-each s f)
  (let-values (((h t) (tseq-get s)))
    (when h
      (f h)
      (tseq-for-each t f))))

;; E.g., (for/list ((t (in-tseq '("a" "b" "c")))) t)
(define* (in-tseq s)
  (make-do-sequence
   (thunk
    (values tseq-first ;; current position -> current element
            tseq-rest ;; current position -> next position
            s ;; initial position
            (negate tseq-null?) ;; current position -> whether at end
            #f
            #f))))

;; E.g., (for/list ((t (in-tseq (tseq-add-between '("a" "b" "c") ",")))) t)
(define* (tseq-add-between s e)
  (let next ((s s))
    (lazy
     (let-values (((h t) (tseq-get s)))
       (if (not h)
           tseq-null
           (if (tseq-null? t)
               h
               (tseq h e (next t))))))))

;;; 
;;; formatting state
;;;

(concrete-struct* FmtSt
                  (
                   cw ;; specified page width (integer, constant)
                   w ;; page width (rational)
                   outDoc ;; formatted document (tseq of Token)
                   inDoc ;; unread input (tseq of Token)
                   k ;; current column (integer)
                   lvStack ;; nesting stack (list of string)
                   bt ;; backtracking state (FmtSt or #f; can be chained)
                   ) #:transparent)

;;; 
;;; stack
;;; 

(define (spush st t)
  (cons t st))

;;; 
;;; indentation
;;; 

(define (spaces n)
  (make-string n #\space))

(define (string-chop-n n s)
  (let* ((len (string-length s))
         (nlen (+ len n)))
    (if (> nlen 0)
        (substring s 0 nlen)
        "")))

;; st:: old indentation state (stack of string)
;; k:: current column (integer)
;; lv:: level specification (Lv)
;; Returns:: new indentation state (stack of string)
(define (margin st k lv)
  (cond
   ((LvInc? lv)
    (spush st
           (let ((s (car st))
                 (n (LvInc-n lv)))
             (if (>= n 0)
                 (string-append s (spaces n))
                 (string-chop-n n s)))))
   ((LvStr? lv)
    (spush st
           (string-append (car st) (LvStr-s lv))))
   ((LvAbs? lv)
    (spush st
           (let ((n (LvAbs-n lv)))
             (if (> n 0) (spaces n) ""))))
   ((LvRel? lv)
    (margin st k (LvAbs (+ k (LvRel-n lv)))))
   ((LvPop? lv)
    ;; Stack must remain non-empty.
    (if (or (null? st) (null? (cdr st)))
        (error "margin: LvPop without matching Lv push")
        (cdr st)))
   (else (error "margin: unexpected" lv))))

;;; 
;;; formatting algorithm
;;; 

;; w:: page width (integer)
;; inDoc:: unread input (tseq of Token, optional)
(define* (new-FmtSt w (inDoc tseq-null))
  (FmtSt w w tseq-null inDoc 0 '("") #f))

;; Flushes buffered documents, committing decisions made thus far.
;; After this it is safe to consume all of 'outDoc'. Note that this
;; just resets state, you'll want to ensure that 'inDoc' contents have
;; already been processed.
(define* (FmtSt-flush st) ;; FmtSt -> FmtSt
  (struct-copy FmtSt st (bt #f)))

;; Prepends input 's' into state.
;;
;; st:: current state (FmtSt)
;; s:: stream to prepend to inDoc (tseq)
;; Returns:: new state (FmtSt)
(define (FmtSt-cons st s)
  (let ((inDoc (FmtSt-inDoc st)))
    (struct-copy FmtSt st (inDoc (tseq-cons s inDoc)))))

;; Before calling this function ensure that all state (except for the
;; argument token) is consistent and in 'st'.
;;
;; st:: current state (FmtSt)
;; d:: token to process (Token)
;; Returns:: new state (FmtSt)
(define (process-token/algo st d)
  (let ((k (FmtSt-k st))
        (w (FmtSt-w st))
        (outDoc (FmtSt-outDoc st))
        (i (car (FmtSt-lvStack st))))
    (cond
     ((Nest? d)
      (struct-copy FmtSt st
                   (lvStack
                    (margin (FmtSt-lvStack st) k (Nest-lv d)))))
     ((Text? d)
      ;; Here we must check whether the text still fits. If it
      ;; doesn't, we'll only continue if we don't have a way back.
      (let ((s (Text-s d)))
        (let ((k (+ k (string-length s)))
              (bt (FmtSt-bt st)))
          (if (and bt (> k w))
              bt ;; backtrack
              (struct-copy FmtSt st
                           (k k) (outDoc (tseq-put outDoc d)))))))
     ((Line? d)
      ;; A break always fits, and then we're committed, and
      ;; won't backtrack from here.
      (struct-copy FmtSt st
                   (k (string-length i))
                   (bt #f)
                   (outDoc
                    (tseq-put* outDoc (Text "\n") (Text i)))))
     ((Union? d)
      ;; Pick left option, leave right for backtracking.
      (let ((l (Union-l d))
            (r (Union-r d)))
        (let ((r-st (FmtSt-cons st r)))
          (FmtSt-cons 
           (struct-copy FmtSt st (bt r-st))
           l))))
     ((Width? d)
      ;; May be used to change page width 'w'. The default (constant)
      ;; width 'cw' never changes.
      (struct-copy FmtSt st (w (Width-w d))))
     ((Flush? d)
      (FmtSt-flush st))
     (else
      (error "process-token/algo: unexpected" d)))))

;; st:: current state (FmtSt)
;; Returns:: new state (FmtSt)
(define (process-token st)
  (let-values (((d inDoc) (tseq-get (FmtSt-inDoc st))))
    (if (not d)
        st
        (let ((st (struct-copy FmtSt st (inDoc inDoc))))
          (process-token/algo st d)))))

;; Whether the state has any data to be processed.
(define* (FmtSt-pending? st) ;; FmtSt -> boolean
  (not (tseq-null? (FmtSt-inDoc st))))

;; Adds a tseq to input.
(define* (FmtSt-write st s) ;; FmtSt, tseq of Token -> St
  (struct-copy FmtSt st (inDoc (tseq (FmtSt-inDoc st) s))))

;; Processes tokens for as long as there is input.
(define* (FmtSt-run st) ;; FmtSt -> FmtSt
  (let loop ((st st))
    (if (FmtSt-pending? st)
        (loop (process-token st))
        st)))

;;; 
;;; text output
;;; 

;; d:: formatted token
;; Returns:: pretty-printed string
(define (f-token->string d) ;; Token -> string
  (cond
   ((Text? d)
    (Text-s d))
   (else
    (error "f-token->string: unexpected" d))))

;; ts:: formatted document
;; Returns:: pretty-printed string
(define (f-tseq->string ts) ;; tseq of Token -> string
  (apply string-append
         (for/list ((t (in-tseq ts)))
                   (f-token->string t))))

;; Process all input and return the result as a string. Effectively
;; does a flush.
(define* (FmtSt->string st) ;; FmtSt -> string
  (f-tseq->string
   (FmtSt-outDoc
    (FmtSt-run st))))

;; Clears output buffer by printing it all out.
(define (b-FmtSt-print st (out (current-output-port)))
  (for ((t (in-tseq (FmtSt-outDoc st))))
       (display (f-token->string t) out))
  (struct-copy FmtSt st (outDoc tseq-null)))

;; Processes as much input as is available, and prints as much as
;; safely can. Works incrementally so that printing happens as soon as
;; there is text ready for output.
(define* (pgf-print/st/safe st (out (current-output-port)))
  (let loop ()
    (unless (FmtSt-bt st)
      (set! st (b-FmtSt-print st out)))
    (set! st (process-token st))
    (if (FmtSt-pending? st) (loop) st)))

;; ts:: unformatted token sequence (tseq)
;; w:: page width
;; out:: output port
;; flush?:: whether to flush
;; newline?:: whether to append newline (implies flush?)
(define* (pgf-print ts
                    #:width [w (pretty-print-columns)]
                    #:out [out (current-output-port)]
                    #:flush [flush? #t]
                    #:newline [newline? #f])
  (define st (pgf-print/st/safe (new-FmtSt w ts) out))
  (when (or newline? flush?)
    (b-FmtSt-print (FmtSt-flush st) out))
  (when newline?
    (newline out)))

;; ts:: unformatted token sequence (tseq)
;; w:: page width
;; Returns:: pretty-printed string
(define* (pgf-string ts #:width [w (pretty-print-columns)])
  (FmtSt->string (new-FmtSt w ts)))

;;; 
;;; common constructions
;;; 

;; l:: left choice (tseq of Token)
;; r:: right choice (tseq of Token)
;; Returns:: tseq of Token
(define* (union l r)
  (Union l r))

;; Behaves lazily.
(define* (flatten s) ;; tseq of Token -> tseq of Token
  (let-values (((t ts) (tseq-get s)))
    (if (not t)
        s
        (cond
         ((Line? t)
          (tseq-cons/lazy (Text " ") (flatten ts)))
         ((Union? t)
          (flatten (tseq/lazy (Union-l t) ts)))
         (else
          (tseq-cons/lazy t (flatten ts)))))))

 ;; tseq of Token -> tseq of Token
(define* (group ts)
  (union (flatten ts) ts))

;;; 
;;; conveniences
;;; 

(define* br (Line))
(define* nbsp (Text " "))
(define* sp (union nbsp br))

(define* indent0 (Nest (LvAbs 0)))
(define* align (Nest (LvRel 0)))
(define* dedent (Nest (LvPop)))
(define* (indent n) (Nest (LvInc n)))
(define* (exdent n) (Nest (LvInc (- n))))


#|

Except where otherwise noted, all code is authored by Tero Hasu,
copyright University of Bergen, and the following license applies.

Copyright (C) 2013 University of Bergen.

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation files
(the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge,
publish, distribute, sublicense, and/or sell copies of the Software,
and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#
