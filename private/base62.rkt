#lang racket/base

(provide
 number->base62-string
 base62-string->number)

(define ALPHABET
  (apply vector (string->list "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")))

(define ALPHABET_REVERSE
  (for/hasheqv ([(c i) (in-indexed ALPHABET)])
    (values c i)))

(define (number->base62-string n)
  (string->immutable-string
   (apply string (let loop ([n  n]
                            [cs null])
                   (if (< n 62)
                       (cons (vector-ref ALPHABET n) cs)
                       (loop (quotient n 62)
                             (cons (vector-ref ALPHABET (remainder n 62)) cs)))))))

(define (base62-string->number s)
  (for/fold ([n 0])
            ([c (in-string s)])
    (define d
      (hash-ref ALPHABET_REVERSE c (lambda ()
                                     (raise-argument-error 'base62-string->number "a base62 character" c))))

    (+ (* n 62) d)))

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (check-equal? n ((compose1 base62-string->number number->base62-string) n)))))
