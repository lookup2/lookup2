(load "tag.scm")
(read-hash-extend #\# tag-read)

(define (href url) ##(a #:href url url))
