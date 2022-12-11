#lang info

(define license 'BSD-3-Clause)
(define version "0.0.0")
(define collection "buid")
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackcheck-lib"
                     "rackunit-lib"
                     "scribble-lib"))
(define scribblings '(("buid.scrbl")))
