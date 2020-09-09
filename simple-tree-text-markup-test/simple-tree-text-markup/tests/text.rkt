#lang racket/base
(require rackunit
         simple-tree-text-markup/construct
         simple-tree-text-markup/private/text)

(check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx" "yyyy" "zzzz"))
              '("aaaxxxx" "bbbyyyy" "ccczzzz"))
(check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx" "zzzz"))
              '("aaaxxxx" "bbbzzzz" "ccc    "))
(check-equal? (append-blocks (list "aaa" "bbb" "ccc") (list "xxxx"))
              '("aaa    " "bbbxxxx" "ccc    "))
(check-equal? (append-blocks (list "aaa" "ccc") (list "xxxx" "yyyy" "zzzz"))
              '("aaaxxxx" "cccyyyy" "   zzzz"))
(check-equal? (append-blocks (list "ccc") (list "xxxx" "yyyy" "zzzz"))
              '("   xxxx" "cccyyyy" "   zzzz"))

(check-equal? (block-box (list "xxx" "yyy" "zzz"))
              '("┌─────┐" "│ xxx │" "│ yyy │" "│ zzz │" "└─────┘"))


(check-equal? (markup->block (horizontal "foo" "bar"
                                         (framed-markup "baz")
                                         "bam" "wup"))
              '("      ┌─────┐      " "foobar│ baz │bamwup" "      └─────┘      "))

(check-equal? (markup->block (horizontal "foo" "bar"
                                         (framed-markup "baz")
                                         (vertical "bam" (framed-markup "wup"))))
              '("      ┌─────┐bam    "
                "foobar│ baz │┌─────┐"
                "      └─────┘│ wup │"
                "             └─────┘"))
(let ((srcloc (srcloc "source" 12 25 100 200)))
  (check-equal? (markup->block (srcloc-markup srcloc (srcloc->string srcloc)))
                '("source:12:25")))
