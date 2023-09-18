#lang racket/base

(require buid/private/base62)

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (check-equal? n ((compose1 base62-string->number number->base62-string) n)))))
