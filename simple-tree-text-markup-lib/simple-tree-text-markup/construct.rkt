; Smart constructors and better names for markup
#lang racket/base
(require racket/contract
         (only-in simple-tree-text-markup/data markup?))
(provide
 (contract-out
  (srcloc-markup (srcloc? markup? . -> . markup?))
  (framed-markup (markup? . -> . markup?))
  (empty-markup markup?)
  (empty-line markup?)
  (horizontal (markup? ... . -> . markup?))
  (vertical (markup? ... . -> . markup?))))
(require (rename-in simple-tree-text-markup/data (empty-markup make-empty-markup))
         (only-in racket/list splitf-at append-map))

(define empty-markup (make-empty-markup))

(define empty-line (horizontal-markup '()))

; flatten out nested markup elements, merge adjacent strings
(define (normalize-horizontal markups)
  (let ((flattened
         (append-map (lambda (markup)
                       (cond
                         ((empty-markup? markup) '())
                         ((horizontal-markup? markup)
                          (horizontal-markup-markups markup))
                         (else
                          (list markup))))
                     markups)))
    (merge-adjacent-strings flattened)))

(define (merge-adjacent-strings markups)
  (call-with-values
   (lambda () (splitf-at markups string?))
   (lambda (strings after)
     (let ((after-merged
            (if (null? after)
                '()
                (cons (car after)
                      (merge-adjacent-strings (cdr after))))))
       (if (null? strings)
           after-merged
           (cons (apply string-append strings)
                 after-merged))))))

(define (horizontal . markups)
  (let ((markups (normalize-horizontal markups)))
    (cond
      ((null? markups) empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else (horizontal-markup markups)))))

(define (flatten-vertical markups)
  (append-map (lambda (markup)
                (cond
                  ((empty-markup? markup) '())
                  ((vertical-markup? markup)
                   (vertical-markup-markups markup))
                  (else (list markup))))
              markups))

(define (vertical . markups)
  (let ((markups (flatten-vertical markups)))
    (cond
      ((null? markups)
       empty-markup)
      ((null? (cdr markups))
       (car markups))
      (else
       (vertical-markup markups)))))

(module+ test
  (require rackunit)
  (check-equal? (horizontal "foo" "bar" "baz")
                "foobarbaz")
  (check-equal? (horizontal "foo" "bar"
                            (horizontal "baz" "bla")
                            "bam" "wup")
                "foobarbazblabamwup")
  (check-equal? (horizontal "foo" "bar"
                            (framed-markup "baz")
                            "bam" "wup")
                (horizontal "foobar" (framed-markup "baz") "bamwup"))

  (check-equal? (vertical)
                empty-markup)
  (check-equal? (vertical "foo")
                "foo")
  (check-equal? (vertical "foo")
                "foo")
  (check-equal? (vertical "foo" empty-markup)
                "foo")
  (check-equal? (vertical "foo" empty-markup "bar")
                (vertical "foo" "bar"))
  (check-equal? (vertical "foo" empty-line "bar")
                (vertical "foo" empty-line "bar"))
  (check-equal? (vertical "foo" "bar")
                (vertical-markup '("foo" "bar")))
  (check-equal? (vertical "foo" (vertical "bla" "baz") "bar")
                (vertical "foo" "bla" "baz" "bar")))