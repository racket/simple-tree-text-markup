#lang info

(define collection 'multi)

(define deps '("simple-tree-text-markup-lib"
               "simple-tree-text-markup-doc"))
(define implies '("simple-tree-text-markup-lib"
                  "simple-tree-text-markup-doc"))

(define pkg-desc "Combinator library for simple markup, mainly for displaying messages in a REPL")

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
