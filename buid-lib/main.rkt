#lang racket/base

(require racket/contract/base
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

(define EPOCH
  1586026830000)

;; We zero out the most significant bit of the randomness component
;; to guarantee that monotonically-generated ids during the same
;; centisecond can effectively never exhaust the randomness space.
(define RANDOMNESS_MASK
  #b0111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

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
  (define t-str (number->base62-string t))
  (define r-str (number->base62-string r))
  (string-copy! out-s (max 0 (- 7  (string-length t-str))) t-str)
  (string-copy! out-s (max 7 (- 22 (string-length r-str))) r-str)
  (string->immutable-string out-s))

(define (try-set-box! b old-v new-v)
  (let loop ()
    (cond
      [(box-cas! b old-v new-v)]
      [(eq? (unbox* b) old-v) (loop)])))

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
