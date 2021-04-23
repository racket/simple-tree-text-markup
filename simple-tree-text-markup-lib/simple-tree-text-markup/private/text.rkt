; Render markup to text
; private version with everything exported for testing
#lang racket/base
(require racket/contract)
(provide (all-defined-out)) ; specific provides + contracts in non-private module
(require racket/list
         simple-tree-text-markup/data)

(define (listify thing)
  (if (or (null? thing)
          (pair? thing))
      thing
      (list thing)))

(define (block-width block)
  (if (null? block)
      0
      (string-length (car block))))

(define (block-adjust block height)
  (let ((block-height (length block))
        (width (block-width block)))
    (if (< block-height height)
        (let ((height-diff (abs (- height block-height))))
          (append (make-list (quotient height-diff 2) (make-string width #\space))
                  block
                  (make-list (quotient (+ height-diff 1) 2) (make-string width #\space))))
        block)))
      
(define (append-blocks-2 block1 block2)
  (let ((height1 (length block1))
        (height2 (length block2)))
    (let ((height (max height1 height2)))
      (map string-append
           (block-adjust block1 height)
           (block-adjust block2 height)))))

(define (append-blocks . blocks)
  (foldr append-blocks-2 '() blocks))

(define (block-box block)
  (let ((width (block-width block)))
    (append (list (string-append "┌─" (make-string width #\─) "─┐"))
            (map (lambda (line)
                   (string-append "│ " line " │"))
                 block)
            (list (string-append "└─" (make-string width #\─) "─┘")))))

(define (block-display port block)
  (for-each (lambda (line)
              (display line port)
              (newline port))
            block))

(define (pad-to line width)
  (let ((diff (max 0 (- width (string-length line)))))
    (if (zero? diff)
        line
        (string-append line (make-string diff #\space)))))

(define (normalize-lines lines)
  (let ((width (apply max (map string-length lines))))
    (map (lambda (line)
           (pad-to line width))
         lines)))

(define (markup->block markup)
  (cond
    ((string? markup) (list markup))
    ((empty-markup? markup) '())
    ((horizontal-markup? markup)
     (apply append-blocks
            (map markup->block (horizontal-markup-markups markup))))
    ((vertical-markup? markup)
     (normalize-lines
      (append-map markup->block (vertical-markup-markups markup))))
    ((srcloc-markup? markup)
     (markup->block (srcloc-markup-markup markup)))
    ((framed-markup? markup)
     (block-box (markup->block (framed-markup-markup markup))))
    ((image-markup? markup)
     (markup->block (image-markup-alt-markup markup)))
    ((number-markup? markup)
     (list (number-markup->string (number-markup-number markup)
                                  (number-markup-prefix? markup)
                                  (number-markup-fraction-view markup))))))

(define (number-markup->string number prefix? fraction-view)
  (cond
    [(inexact? number)
     (string-append (if prefix? "#i" "") (number->string number))]
    [(integer? number)
     (string-append (if prefix? "#e" "") (number->string number))]
    [else
     (number-markup->string/exact number prefix? fraction-view)]))

; stolen from number-snip code
(define (number-markup->string/exact number prefix? fraction-view)

  (define decimal-prefix (if prefix? "#e" ""))
  
  ;; clickable-portion : (union #f string)
  (define clickable-portion #f)
  ;; unbarred-portion : string
  (define unbarred-portion "")
  ;; barred-portion : (union #f string)
  (define barred-portion #f)

  ;; wholes/frac : string
  ;; the whole-number portion of the number as a fraction
  (define wholes/frac
    (cond
      [(= (floor number) 0) ""]
      [(= (ceiling number) 0) "-"]
      [(< number 0)
       (number->string (ceiling number))]
      [else
       (number->string (floor number))]))
      
  ;; wholes/dec : string
  ;; the whole-number portion of decimal expansion
  (define wholes/dec
    (cond
      [(= (floor number) 0) "0"]
      [(= (ceiling number) 0) "-0"]
      [(< number 0)
       (number->string (ceiling number))]
      [else
       (number->string (floor number))]))
      
  ;; nums : string
  ;; the numerator of the mixed fraction, as a string
  (define nums (number->string (numerator (- (abs number) (floor (abs number))))))
  
  ;; improper-nums : string
  ;; the numerator of the improper fraction, as a string
  (define improper-nums (number->string (numerator (abs number))))
  
  ;; mixed-prefix : string
  ;; a prefix on the front of the mixed number (indicates if negative)
  (define improper-prefix (if (number . < . 0) "-" ""))

  ;; dens : string
  ;; the denominator, as a string
  (define dens (number->string (denominator (- (abs number) (floor (abs number))))))

  ;; for the decimal expansion calculation code
  (define init-num (* 10 (numerator (- (abs number) (floor (abs number))))))
  (define den (denominator (- (abs number) (floor (abs number)))))
    

  ;; ht : number -o> (cons digit number)
  ;; this maps from divisors of the denominator to
  ;; digit and new divisor pairs. Use this
  ;; to read off the decimal expansion.
  (define ht (make-hash))

  ;; this field holds the state of the current computation
  ;; of the numbers digits. If it is a number, it corresponds
  ;; to the next starting divisor in the iteration.
  ;; if it is #f, it means that the string of digits is
  ;; fully computed.
  (define state init-num)

  ;; repeat : (union 'unk number #f)
  ;; this field correlates with `state'. If `state' is a number,
  ;; this field is 'unk. Otherwise, this is either a number of #f.
  ;; #f indicates no repeat.
  ;; a number indiates a repeat starting at `number' in `ht'.
  (define repeat 'unk)

  ;; cut-off : number
  ;; indicates how many digits to fetch for each click
  (define cut-off 25)

  ;; one-step-division : number -> number number
  ;; given a numerator and denominator,
  ;; returns a digits and a new numerator to consider
  (define (one-step-division num)
    (cond
      [(num . < . den) (values 0 (* 10 num))]
      [else
       (let ([qu (quotient num den)])
         (values qu (* 10 (- num (* qu den)))))]))
      
  ;; expand-number : -> void
  ;; iterates until the numbers decimal expansion is completely computed,
  ;; or the number's decimal expansion terminates.
  (define (expand-number)
    (let loop ([num state]
               [counter cut-off])
      (cond
        [(hash-has-key? ht num)
         (set! state #f)
         (set! repeat num)]
        [(zero? counter) 
         (set! state num)]
        [else
         (let-values ([(dig next-num) (one-step-division num)])
           (if (zero? next-num)
               (begin
                 (hash-set! ht num (cons dig #t))
                 (set! state #f)
                 (set! repeat #f))
               (begin
                 (hash-set! ht num (cons dig next-num))
                 (loop next-num (- counter 1)))))])))

  ;; extract-cycle : -> (listof digit)
  ;; pre: (number? repeat)
  (define (extract-cycle)
    (let ([pr (hash-ref ht repeat)])
      (cons (car pr)
            (extract-helper (cdr pr)))))
      
  ;; extract-non-cycle : -> (listof digit)
  (define (extract-non-cycle) (extract-helper init-num))
      
  (define (extract-helper start)
    (let loop ([ind start])
      (cond
        [(equal? ind repeat) null]
        [else
         (let* ([iter (hash-ref ht ind)]
                [dig (car iter)]
                [next-num (cdr iter)])
           (cons dig
                 (if (hash-has-key? ht next-num)
                     (loop next-num)
                     null)))])))

  (expand-number)
  
  (cond
    [(number? state) 
     (set! unbarred-portion
           (string-append
            decimal-prefix
            wholes/dec
            "."
            (apply string-append (map number->string (extract-non-cycle)))))
     (set! barred-portion #f)
     (set! clickable-portion "...")]
    [(number? repeat)
     (set! unbarred-portion
           (string-append
            decimal-prefix
            wholes/dec
            "."
            (apply string-append 
                   (map number->string (extract-non-cycle)))))
     (set! barred-portion (apply string-append (map number->string (extract-cycle))))
     (set! clickable-portion #f)]
    [else
     (set! unbarred-portion
           (string-append
            decimal-prefix
            wholes/dec
            "."
            (apply string-append
                   (map number->string (extract-non-cycle)))))
     (set! barred-portion #f)
     (set! clickable-portion #f)])

  (case fraction-view
    [(mixed)
     (cond
       [(string=? wholes/frac "")
        (string-append nums "/" dens)]
       [(string=? wholes/frac "-")
        (string-append wholes/frac nums "/" dens)]
       [else
        (string-append wholes/frac " " nums "/" dens)])]
    [(decimal)
     (string-append 
      unbarred-portion
      (if barred-portion
          (string-append barred-portion "_")
          "")
      (or clickable-portion ""))]
    [(improper) (string-append 
                 improper-prefix
                 improper-nums
                 "/"
                 dens)]))

(define display-markup
  (case-lambda
    ((markup) (display-markup markup (current-output-port)))
    ((markup port)
     (block-display port (markup->block markup)))))
