#lang racket/base

(require buid (submod buid private))

(module+ test
  (require rackcheck
           rackunit)

  (check-property
   (make-config #:tests 10000)
   (property ([n gen:natural])
     (check-eqv? n (unpack (pack n 8)))))

  (define gen:buid
    (gen:let ([t1 (gen:integer-in 0 #xFFFFFF)]
              [t2 (gen:integer-in 0 #xFFFF)]
              [r1 (gen:integer-in 0 (sub1 #xFFFFFF))]
              [r2 (gen:integer-in 0 (sub1 #xFFFFFF))]
              [r3 (gen:integer-in 0 (sub1 #xFFFFFF))]
              [r4 (gen:integer-in 0 (sub1 #xFFFF))])
      (make-buid-string (+ (arithmetic-shift t1 16)
                           t2)
                        (+ (arithmetic-shift r1 64)
                           (arithmetic-shift r2 40)
                           (arithmetic-shift r3 16)
                           r4))))

  (check-property
   (make-config #:tests 10000)
   (property ([id gen:buid])
     (check-equal? id (bytes->buid (buid->bytes id))))))
