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

(check-equal? (markup-transform-image-data (lambda (data)
                                             (format "~a transformed" data))
                                           (horizontal
                                            "foo"
                                            (image-markup "horizontal" "image")
                                            empty-markup
                                            (vertical
                                             "bar"
                                             (image-markup "vertical" "image")
                                             (srcloc-markup (srcloc 'source 1 2 3 4)
                                                            (image-markup "srcloc" "image"))
                                             (framed-markup
                                              (image-markup "framed" "image")))))
              (horizontal
               "foo"
               (image-markup "horizontal transformed" "image")
               empty-markup
               (vertical
                "bar"
                (image-markup "vertical transformed" "image")
                (srcloc-markup (srcloc 'source 1 2 3 4)
                               (image-markup "srcloc transformed" "image"))
                (framed-markup
                 (image-markup "framed transformed" "image")))))

             
