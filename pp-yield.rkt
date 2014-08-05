#lang racket/base

#|

Implements the algorithm from Kiselyov et al: Lazy v. Yield:
Incremental, Linear Pretty-Printing (2012). With some extensions, one
notable modification, and with the caveat that some of the auxiliary
data structures used here do not have the required algorithmic
properties.

|#

(require racket/contract racket/match
         "util/bankers-deque.rkt" "util.rkt")

;;; 
;;; indentation levels
;;; 

(abstract-struct Lv () #:transparent)

(define-syntax-rule (define-level n fld-spec ...)
  (concrete-struct n Lv (fld-spec ...) #:transparent))

(define-level LvInc n) ;; integer -> Lv
(define-level LvStr s) ;; string -> Lv
(define-level LvAbs n) ;; integer -> Lv
(define-level LvRel n) ;; integer -> Lv
(define-level LvPop) ;; -> Lv

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
   [(LvInc? lv)
    (cons (let ((s (car st))
                (n (LvInc-n lv)))
            (if (>= n 0)
                (string-append s (spaces n))
                (string-chop-n n s))) 
          st)]
   [(LvStr? lv)
    (cons (string-append (car st) (LvStr-s lv)) st)]
   [(LvAbs? lv)
    (cons (let ((n (LvAbs-n lv)))
            (if (> n 0) (spaces n) ""))
          st)]
   [(LvRel? lv)
    (margin st k (LvAbs (+ k (LvRel-n lv))))]
   [(LvPop? lv)
    ;; Stack must remain non-empty.
    (if (or (null? st) (null? (cdr st)))
        (error 'margin "LvPop without matching Lv push")
        (cdr st))]
   [else 
    (raise-argument-error 'margin "Lv?" 2 st k lv)]))

;;; 
;;; tokens
;;; 

(abstract-struct Token () #:transparent)

(define-syntax-rule (define-token n fld-spec ...)
  (concrete-struct n Token (fld-spec ...) #:transparent))

(define-token TE a s) ;; text
(define-token LE a ind) ;; breakable space
(define-token GBeg beg end) ;; group begin
(define-token GEnd a) ;; group end

;; extensions
(define-token Reset) ;; unconditional newline

;;; 
;;; algorithm
;;; 

;; Note that since this generator has state, it is necessary to create
;; a new instance for every pipeline instance. If there are any
;; characters on the line to begin with, `init-pos` should be
;; specified accordingly.
(define (make-annotate-position init-pos yield)
  (let ([pos init-pos])
    (lambda (t)
      (match t
	[(TE _ s)
         (set! pos (+ pos (string-length s)))
         (yield (TE pos s))]
	[(LE _ ind) 
         (set! pos (+ pos 1))
         (yield (LE pos ind))]
	[(GBeg _ _) 
         (yield (GBeg pos #f))]
	[(GEnd _) 
         (yield (GEnd pos))]
        [(Reset) 
         (set! pos 0)
         (yield t)]
        [_ 
         (yield t)]))))

;; We require very stringent algorithmic properties for state updates
;; and queries for the overall algorithm to get the original
;; properties. We miss the mark here only because dq-append is not
;; O(1) amortized time, and because dq-for-each is not constant space.
(struct BufferP (p q) #:transparent)

;; Each entry of `q` (above) is of type `Grp`.
(struct Grp (p q beg) #:transparent)

(define BufferP-empty (BufferP 0 dq-null))

(define (BufferP-empty? buffer)
  (dq-null? (BufferP-q buffer)))

(define (BufferP-p-q buffer)
  (values (BufferP-p buffer) (BufferP-q buffer)))

(define (BufferP-add-grp buffer grp)
  (define-values (p q) (BufferP-p-q buffer))
  (BufferP p (dq-conj-r q grp)))

(define (Grp-add-token grp t)
  (define b (Grp-q grp))
  (struct-copy Grp grp [q (dq-conj-r b t)]))

(define (Grp-append-tokens grp ts)
  (define b (Grp-q grp))
  (struct-copy Grp grp [q (dq-append b ts)]))

;; Determines group widths, essentially, and annotates `GBeg` end
;; positions accordingly, using 'too-far where they definitely will
;; not fit. (This code is so intricate that we retain some of the
;; original structure for easier comparison.)
(define (make-annotate-width w yield)
  (let ((buffer BufferP-empty))
    (define (go buffer t)
      (if (BufferP-empty? buffer)
	  (match t
	    [(GBeg p _)
	     (let ((p+w (+ p w)))
               (BufferP p+w (dq (Grp p+w dq-null p))))]
            ;; `GEnd` may appear here, presumably if we have pruned a
            ;; `GBeg` and emptied our buffers before the `GEnd` comes in.
            [(or (? TE?) (? LE?) (? GEnd?) (? Reset?))
             (begin (yield t) buffer)])
	  (match t
	    [(GBeg p _)
             (define grp (Grp (+ p w) dq-null p))
             (define st (BufferP-add-grp buffer grp))
             (check st p)]
	    [(GEnd p)
	     (let*-values ([(p0 q) (BufferP-p-q buffer)]
			   [(grp q^) (dq-pop-r q)])
               (match grp
                 [(Grp _ b beg)
                  (pop p0 q^ (dq-conj-f-r b (GBeg beg p) (GEnd p)))]))]
	    [(or (TE p _) (LE p _))
             (define-values (p0 q) (BufferP-p-q buffer))
             (check (BufferP p0 (push t q)) p)]
            [(Reset)
             (error 'make-annotate-width 
                    "received Reset while within a Group")])))

    ;; Append token `t` to the buffer of the rightmost group in `q`.
    (define (push t q)
      (dq-modify-r q (lambda (grp) (Grp-add-token grp t))))

    ;; Processes buffered token sequence `b`, where `p` and `q` is the
    ;; remaining group state (as for BufferP).
    (define (pop p q b)
      (if (dq-null? q)
	  (begin
	    (dq-for-each b yield)
	    BufferP-empty)
	  (let ()
	    (define n-q 
              (dq-modify-r q (lambda (grp) (Grp-append-tokens grp b))))
	    (BufferP p n-q))))

    ;; Prunes look-ahead `st` in case that the current horizontal
    ;; position `p` is too far for the outermost group to fit. Or,
    ;; assuming normalized input, in the case that we have more than
    ;; `w` groups, in which case they will be wider than `w` (at least
    ;; 1 char per group).
    (define (check st p) ;; BufferP, pos -> BufferP
      (define-values (p0 q) (BufferP-p-q st))
      (if (and (<= p p0) (<= (dq-length q) w))
	  st ;; unchanged
	  ;; The outermost group does not fit.
	  (let*-values ([(grp q^) (dq-pop-f q)])
	    (yield (GBeg (Grp-beg grp) 'too-far))
	    (dq-for-each (Grp-q grp) yield)
            ;; Also check inner ones.
	    (check^ q^ p))))
    
    ;; dq, pos -> BufferP
    (define (check^ q p)
      (if (dq-null? q)
	  BufferP-empty
	  (let ([p^ (Grp-p (dq-car-f q))])
	    (check (BufferP p^ q) p))))
    
    (lambda (t)
      (set! buffer (go buffer t)))))

;;; 
;;; DSL
;;; 

(abstract-struct Doc () #:transparent)

(define-syntax-rule (define-doc* n fld-spec ...)
  (concrete-struct* n Doc (fld-spec ...) #:transparent))

(define-doc* Space)
(define-doc* Group d)
(define-doc* Nest lv d)

;; Normalizes a `d` that is immediately within an outer group. Removes
;; any redundant immediate `Group` elements, or anything that has no
;; width. Returns #f if nothing of interest is left.
(define (normalize d)
  (match d
    [(Group d0) (normalize d0)]
    [(? list? lst)
     (match lst
       [(? null?) #f]
       [(list d0) (normalize d0)]
       [(list-rest (not (? normalize)) ds)
        (normalize ds)]
       [_ d])]
    [(Nest _ (not (? normalize))) #f]
    ["" #f]
    [_ d]))

(define* (doc? x)
  (or (string? x) (list? x) (Doc? x) (not x)
      (memq x '(sp br))
      (memv x '(#\newline #\space))))

(define-with-contract*
  (->* ()
       (#:pos integer?
        #:indent (listof string?)
        #:page-width integer?
        #:tab-width integer?
        #:yield procedure?)
       #:rest (listof doc?)
       integer?)
  (pp #:pos [st-col 0] ;; output column
      #:indent [init-ind '("")] ;; indentation string stack
      #:page-width [w 75]
      #:tab-width [tab 2]
      #:yield [yield-out display]
      . spec-lst)
  
  ;; Like `yield-out`, but also updates `st-col`.
  (define (track-pos s) ;; (-> string? any/c)
    (match s
      ["\n" 
       (set! st-col 0)]
      [(? string?) 
       (set! st-col (+ st-col (string-length s)))])
    (yield-out s))

  ;; Based on collected information and state, decides where to break
  ;; lines. Here we deviate from the original algorithm in order to
  ;; support indentation. Here we are able to make the ultimate
  ;; decision based on actual position `st-col`, since there is no
  ;; buffering.
  (define (make-emit)
    (let ([yield track-pos]
          [fits 0]) ;; number of fitting groups around position
      (lambda (t)
        (match t
          [(TE _ s) 
           (yield s)]
          [(LE p ind) 
           (cond
            [(= fits 0)
             (yield "\n")
             (yield ind)]
            [else
             (yield " ")])]
          [(GBeg beg end)
           (cond
            [(= fits 0)
             (when (and (not (eq? end 'too-far))
                        (<= (+ st-col (- end beg)) w))
               (set! fits 1))]
            [else
             ;; If an outer group fits, so will an inner one.
             (set! fits (add1 fits))])]
          [(GEnd _) 
           (unless (= fits 0)
             (set! fits (sub1 fits)))]
          [(Reset)
           (set! fits 0)
           (yield "\n")]))))
  
  ;; The position `st-col` is a theoretical starting position for an
  ;; entire document that fits on a single line. Hence there is no
  ;; indentation, but the start position might be non-zero. It is
  ;; enough if the position numbers are unique within a document (or
  ;; any groups, really), as we are ultimately interested in
  ;; differences, i.e. whether something will fit within two
  ;; positions.
  (define pgf-yield
    (let ((annotate-width
           (make-annotate-width w (make-emit))))
      (make-annotate-position st-col annotate-width)))
    
  (let ([in-group 0]
        [ind-col st-col] ;; (or/c integer? #f), tracked for LvRel
        [ind init-ind]
        [yield pgf-yield])
    (for ([spec spec-lst])
      (let parse ([d spec])
        [match d
          [(or "" #f) 
           (void)]
          [(or "\n" #\newline 'br)
           (unless (= in-group 0)
             (error 'pp "linebreak within a Group: ~s" spec))
           (define s (car ind))
           (define s-len (string-length s))
           (set! ind-col s-len)
           (yield (Reset))
           (yield (TE #f s))]
          [(? string? s) 
           (yield (TE #f s))
           (when ind-col
             (set! ind-col (+ ind-col (string-length s))))]
          [(or (Space) #\space 'sp)
           (cond
            [(= in-group 0)
             (parse " ")]
            [else
             (yield (LE #f (car ind)))
             (set! ind-col #f)])]
          [(list-rest 'in ds)
           (parse (Nest (LvInc tab) ds))] 
          [(list-rest 'al ds)
           (parse (Nest (LvRel 0) ds))] 
          [(list-rest 'gr ds)
           (parse (Group ds))]
          [(? list? ds) 
           (for-each parse ds)]
          [(Group (? doc? d0))
           (let ((d (normalize d0)))
             (when d
               (yield (GBeg #f #f))
               (set! in-group (add1 in-group))
               (parse d)
               (yield (GEnd #f))
               (set! in-group (sub1 in-group))))]
          [(Nest (? Lv? lv) (? doc? d0))
           (when (and (LvRel? lv) (not ind-col))
             (error 'pp "LvRel after Space in a Group" d))
           (set! ind (margin ind ind-col lv))
           (parse d0)
           (set! ind (margin ind ind-col (LvPop)))]
          [_ 
           (error 'pp "unknown document element: ~s" d)]])))

  st-col)

(module+ test
  (require rackunit)
  (define doc1 (Group `("A" #\space ,(Group `("B" #\space "C")))))
  (for ([w '(2 4 6)])
    (displayln `(w = ,w))
    (pp doc1 #:page-width w)
    (newline))
  (for* ([doc (list 
               `("class" sp "C" sp "{};") 
               `("class C {" (in br
                 "void f();") br
                 "};")
               `("int x[] = {" (in (gr "1," sp "2," sp "3")) "};")
               `("int x[] = {" (al (gr "1," sp "2," sp "3")) "};")
               `(gr (gr "1," sp "2,") sp (gr "3," sp "4"))
               `(gr (gr "11," sp "22,") sp (gr "3," sp "4"))
               )]
         [w '(5 10 20)])
    (displayln `(w = ,w))
    (pp doc #:page-width w)
    (newline)))
