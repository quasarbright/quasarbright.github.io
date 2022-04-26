; main.rkt
#lang typed/racket

(require/typed "./index-of.rkt"
               [index-of (-> String Char Integer)])

(index-of "hello" #\p)


