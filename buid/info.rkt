#lang info

(define license 'BSD-3-Clause)
(define collection "buid")
(define deps '("base"
               "buid-lib"))
(define build-deps '("racket-doc"
                     "scribble-lib"))
(define scribblings '(("buid.scrbl")))
(define implies '("buid-lib"))
