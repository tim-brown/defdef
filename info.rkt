#lang info
(define collection "defdef")
(define deps '("base"
               "rackunit-lib"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/defdef.scrbl" ())))
(define pkg-desc "Macros to define function definers")
(define version "0.0")
(define pkg-authors '(tim))
