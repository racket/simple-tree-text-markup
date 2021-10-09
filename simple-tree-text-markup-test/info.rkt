#lang info

(define collection 'multi)

(define deps '("base"))
(define build-deps '("simple-tree-text-markup-lib"
                     "rackunit-lib"))
(define update-implies '("simple-tree-text-markup-lib"))

(define pkg-desc "tests for \"simple-tree-text-markup\"")

(define pkg-authors '(sperber))

(define license
  '(Apache-2.0 OR MIT))
