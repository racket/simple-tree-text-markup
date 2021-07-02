#lang racket/base
(require simple-tree-text-markup/data
         rackunit)

(define collect-strings
  (markup-folder (lambda (a b) (append b a)) '() `((,string? . ,list))))

(check-equal? (collect-strings
               (horizontal-markup (list "foo" "bar" "baz")))
              '("foo" "bar" "baz"))

(check-equal? (collect-strings
               (horizontal-markup
                (list
                 "foo"
                 (vertical-markup
                  (list
                   "bar"
                   (framed-markup
                    (srcloc-markup (srcloc 1 2 3 4 5)
                                   (image-markup
                                    'data
                                    (vertical-markup
                                     (list
                                      "bla"
                                      (empty-markup)
                                      "blub"))))))))))
              '("foo" "bar" "bla" "blub"))

(check-equal? (transform-markup
               `((,string? . ,(lambda (s) (string-append s "-transformed")))
                 (,horizontal-markup? . ,(lambda (markups) (horizontal-markup (reverse markups))))
                 (,number-markup? . ,(lambda (number exact-prefix inexact-prefix fraction-view)
                                       (number-markup (+ 1 number) exact-prefix inexact-prefix fraction-view))))
               (horizontal-markup
                (list
                 "foo"
                 (vertical-markup
                  (list
                   "bar"
                   (framed-markup
                    (number-markup  5 'always 'always 'mixed)))))))
              (horizontal-markup
               (list
                (vertical-markup
                 (list "bar-transformed"
                       (framed-markup (number-markup 6 'always 'always 'mixed))))
                "foo-transformed")))
