; lib.rkt
#lang typed/racket

(provide increment)

(: increment : Number -> Number)
(define (increment n)
  (+ n 1))
