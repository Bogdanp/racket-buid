#lang racket/base

(provide
 base62-into-string!
 number->base62-string
 base62-string->number)

(define NUMBER_TO_CHAR
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(define CHAR_TO_NUMBER
  (for/hasheqv ([(c idx) (in-indexed (in-string NUMBER_TO_CHAR))])
    (values c idx)))

(define (number->char n)
  (string-ref NUMBER_TO_CHAR n))

(define (char->number c)
  (hash-ref CHAR_TO_NUMBER c))

(define (base62-into-string! dst n [end (sub1 (string-length dst))])
  (let loop ([n   n]
             [idx end])
    (string-set! dst idx (number->char (remainder n 62)))
    (unless (< n 62)
      (loop (quotient n 62) (sub1 idx)))))

(define (number->base62-string n)
  (string->immutable-string
   (apply string (let loop ([n  n]
                            [cs null])
                   (if (< n 62)
                       (cons (number->char n) cs)
                       (loop (quotient n 62)
                             (cons (number->char (remainder n 62)) cs)))))))

(define (base62-string->number s)
  (for/fold ([n 0])
            ([c (in-string s)])
    (+ (* n 62)
       (char->number c))))
