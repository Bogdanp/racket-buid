#lang racket/base

(require (for-syntax racket/base
                     syntax/parse))

(provide
 number->base62-string
 base62-string->number)

(begin-for-syntax
  (define ALPHABET
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))

(define-syntax (make-lookup-table stx)
  (syntax-parse stx
    [(_ n:expr)
     #:with ((v c) ...) (for/list ([(c i) (in-indexed ALPHABET)])
                          (list (datum->syntax stx i)
                                (datum->syntax stx c)))
     #'(case n
         [(v) c] ...
         [else (raise-argument-error 'number->base62-string "a number in the range [0, 62)" n)])]))

(define-syntax (make-reverse-table stx)
  (syntax-parse stx
    [(_ c:expr)
     #:with ((v n) ...) (for/list ([(c i) (in-indexed ALPHABET)])
                          (list (datum->syntax stx c)
                                (datum->syntax stx i)))
     #'(case c
         [(v) n] ...
         [else (raise-argument-error 'base62-string->number "a base62 character" c)])]))

(define (number->char n)
  (make-lookup-table n))

(define (char->number c)
  (make-reverse-table c))

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

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (property ([n (gen:integer-in 0 #xFFFFFF)])
     (check-equal? n ((compose1 base62-string->number number->base62-string) n)))))
