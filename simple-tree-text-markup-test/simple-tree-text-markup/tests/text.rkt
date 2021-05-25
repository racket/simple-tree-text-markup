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

(check-equal? (markup->block (horizontal "foo" (image-markup 'data "<image>")))
              '("foo<image>"))

(check-equal? (markup->block (number-markup 1/2 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal))
              '("0.5"))

(let ((srcloc (srcloc "source" 12 25 100 200)))
  (check-equal? (markup->block (srcloc-markup srcloc (srcloc->string srcloc)))
                '("source:12:25")))

(check-equal? (number-markup->string 3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)
              "3")
(check-equal? (number-markup->string 1/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)
              "1/3")
(check-equal? (number-markup->string 1/2 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'decimal)
              "0.5")
(check-equal? (number-markup->string 4/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view  'mixed)
              "4/3")
(check-equal? (number-markup->string 4/3 #:exact-prefix 'never #:inexact-prefix 'never #:fraction-view 'improper)
              "4/3")
(check-equal? (number-markup->string #i0.5 #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view  'decimal)
              "#i0.5")
(check-equal? (number-markup->string #e0.5 #:exact-prefix 'when-necessary #:inexact-prefix 'never #:fraction-view 'decimal)
              "#e0.5")
(check-equal? (number-markup->string #e0.5 #:exact-prefix  'always #:inexact-prefix 'never #:fraction-view 'decimal)
              "#e0.5")
(check-equal? (number-markup->string #i0.5 #:exact-prefix 'never #:inexact-prefix 'when-necessary #:fraction-view 'decimal)
              "0.5")
(check-equal? (number-markup->string #i0.5 #:exact-prefix 'never #:inexact-prefix 'always #:fraction-view 'decimal)
              "#i0.5")
(check-equal? (number-markup->string (sqrt -1) #:exact-prefix 'always #:inexact-prefix 'never #:fraction-view 'decimal)
              "#e0+1i")
(check-equal? (number-markup->string 1/2+3/5i #:exact-prefix 'always #:inexact-prefix 'never #:fraction-view 'decimal)
              "#e0.5+0.6i")
(check-equal? (number-markup->string 1/2+3/5i #:exact-prefix 'always #:inexact-prefix 'never #:fraction-view 'mixed)
              "#e1/2+3/5i")
(check-equal? (number-markup->string #i1/2+3/5i #:exact-prefix'always #:inexact-prefix 'always #:fraction-view 'mixed)
              "#i0.5+0.6i")

