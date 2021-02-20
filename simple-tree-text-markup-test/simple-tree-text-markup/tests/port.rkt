#lang racket/base

(require simple-tree-text-markup/data
         simple-tree-text-markup/port
         rackunit)

(define (check-with make-markup-output-port)
  (check-equal? (call-with-values
                 (lambda () (make-markup-output-port (lambda (special)
                                                       (framed-markup "special"))))
                 (lambda (port get-verticals)
                   (display "foo\nbäm\n\n" port)
                   (display "blaz" port)
                   (newline port)
                   (newline port)
                   (display "bar" port)
                   (write-special 'special port)
                   (newline port)
                   (get-verticals)))
                (vertical-markup
                 (list "foo" "bäm" "" "blaz" ""
                       (horizontal-markup (list "bar" (framed-markup "special")))))))

(check-with make-markup-output-port)
(check-with make-markup-output-port/unsafe)

