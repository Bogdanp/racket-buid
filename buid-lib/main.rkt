#lang racket/base

(require file/sha1
         racket/contract/base
         racket/contract/combinator
         racket/math
         racket/random
         "private/base62.rkt")

(provide
 make-buid-factory
 buid/c
 buid
 (contract-out
  [buid-time (-> buid/c exact-nonnegative-integer?)]
  [buid-posix-time (-> buid/c exact-nonnegative-integer?)]
  [buid-randomness (-> buid/c exact-nonnegative-integer?)]))

(define-logger buid)

(define EPOCH
  1586026830000)

;; We zero out the most significant bit of the randomness component
;; to guarantee that monotonically-generated ids during the same
;; centisecond can effectively never exhaust the randomness space.
(define RANDOMNESS_MASK
  #x7FFFFFFFFFFFFFFFFFFFFF)

(define (current-centiseconds)
  (exact-truncate (/ (- (current-inexact-milliseconds) EPOCH) 10)))

(struct state (t n))

(define (make-buid-factory)
  (define s-box
    (box (state -1 0)))
  (lambda ()
    (define s (unbox* s-box))
    (define t (current-centiseconds))
    (define n
      (if (= t (state-t s))
          (add1 (state-n s))
          (bitwise-and
           (unpack (crypto-random-bytes 11))
           RANDOMNESS_MASK)))
    (try-set-box! s-box s (state t n))
    (make-buid-string t n)))

(define (make-buid-string t r)
  (define out-s (make-string 22 #\0))
  (base62-into-string! out-s t 6)
  (base62-into-string! out-s r 21)
  (string->immutable-string out-s))

(define (try-set-box! b old-v new-v)
  (let loop ([gas 3])
    (cond
      [(box-cas! b old-v new-v)]
      [(eq? (unbox* b) old-v)
       (cond
         [(zero? gas)
          (void)]
         [else
          (log-buid-warning "try-set-box!: spurious failure [~a gas]" gas)
          (loop (sub1 gas))])]
      [else (void)])))

(define buid
  (make-buid-factory))

(define buid/c
  (make-flat-contract
   #:name 'buid/c
   #:first-order (lambda (s)
                   (and (string? s)
                        (= (string-length s) 22)))))

(define (buid-time s)
  (base62-string->number (substring s 0 7)))

(define (buid-posix-time s)
  (+ (* (buid-time s) 10) EPOCH))

(define (buid-randomness s)
  (base62-string->number (substring s 7)))

(define (pack v n)
  (let loop ([n  n]
             [v  v]
             [bs null])
    (if (zero? n)
        (apply bytes bs)
        (loop (sub1 n)
              (arithmetic-shift v -8)
              (cons (bitwise-and v #xFF) bs)))))

(define (unpack bs)
  (for/fold ([n 0])
            ([b (in-bytes bs)])
    (+ (arithmetic-shift n 8) b)))

(module+ private
  (provide make-buid-string pack unpack))


;; binary representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 buid-bytes/c
 (contract-out
  [buid->bytes (-> buid/c buid-bytes/c)]
  [bytes->buid (-> buid-bytes/c buid/c)]))

(define buid-bytes/c
  (make-flat-contract
   #:name 'buid-bytes/c
   #:first-order (lambda (bs)
                   (and (bytes? bs)
                        (= (bytes-length bs) 16)))))

(define (buid->bytes s)
  (bytes-append
   (pack (buid-time s) 5)
   (pack (buid-randomness s) 11)))

(define (bytes->buid bs)
  (make-buid-string
   (unpack (subbytes bs 0 5))
   (unpack (subbytes bs 5))))


;; uuid representation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 buid->uuid
 uuid->buid)

(define (buid->uuid v)
  (define res (make-string 36 #\-))
  (define hex (bytes->hex-string (buid->bytes v)))
  (begin0 res
    (string-copy! res 0  hex 0  8)
    (string-copy! res 9  hex 8  12)
    (string-copy! res 14 hex 12 16)
    (string-copy! res 19 hex 16 20)
    (string-copy! res 24 hex 20)))

(define (uuid->buid s)
  (define bs (make-bytes 16))
  (define len (string-length s))
  (let loop ([idx 0] [dst 0])
    (if (< (add1 idx) len) ;; noqa
        (let ([c0 (string-ref s idx)]
              [c1 (string-ref s (add1 idx))])
          (cond
            [(eqv? c0 #\-)
             (loop (add1 idx) dst)]
            [else
             (define b
               (+ (* 16 (hexdigit->integer c0))
                  (hexdigit->integer c1)))
             (bytes-set! bs dst b)
             (loop (+ idx 2) (add1 dst))]))
        (bytes->buid bs))))

(define (hexdigit->integer c)
  (case c
    [(#\0) 0]
    [(#\1) 1]
    [(#\2) 2]
    [(#\3) 3]
    [(#\4) 4]
    [(#\5) 5]
    [(#\6) 6]
    [(#\7) 7]
    [(#\8) 8]
    [(#\9) 9]
    [(#\a #\A) 10]
    [(#\b #\B) 11]
    [(#\c #\C) 12]
    [(#\d #\D) 13]
    [(#\e #\E) 14]
    [(#\f #\F) 15]))
