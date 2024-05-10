#lang racket/base

(require buid/private/base62)

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (check-equal? n ((compose1 base62-string->number number->base62-string) n))))

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (define s1 (number->base62-string n))
     (define s2 (make-string (string-length s1) #\0))
     (base62-into-string! s2 n)
     (check-equal? s1 s2))))
