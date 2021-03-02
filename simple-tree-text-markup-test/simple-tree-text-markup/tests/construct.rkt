#lang racket/base
(require simple-tree-text-markup/construct
         (only-in simple-tree-text-markup/data vertical-markup)
         rackunit)
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
              (vertical "foo" "bla" "baz" "bar"))

(check-equal? (markup-transform-image-data (lambda (data width height)
                                             (format "~a transformed ~a ~a" data width height))
                                           (horizontal
                                            "foo"
                                            (image-markup "horizontal" "image" 1 2)
                                            empty-markup
                                            (vertical
                                             "bar"
                                             (image-markup "vertical" "image" 3 4)
                                             (srcloc-markup (srcloc 'source 1 2 3 4)
                                                            (image-markup "srcloc" "image" 5 6))
                                             (framed-markup
                                              (image-markup "framed" "image" 7 8)))))
              (horizontal
               "foo"
               (image-markup "horizontal transformed 1 2" "image" 1 2)
               empty-markup
               (vertical
                "bar"
                (image-markup "vertical transformed 3 4" "image" 3 4)
                (srcloc-markup (srcloc 'source 1 2 3 4)
                               (image-markup "srcloc transformed 5 6" "image" 5 6))
                (framed-markup
                 (image-markup "framed transformed 7 8" "image" 7 8)))))

             
