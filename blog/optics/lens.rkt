#lang racket

(struct lens (getter setter) #:transparent)

(define (lens-get l s) ((lens-getter l) s))
(define (lens-set l s t) ((lens-setter l) s t))
(define (lens-update l s f) (lens-set l s (f (lens-get l s))))
(define identity-lens (lens identity (lambda (s t) t)))
(define (lens-compose . lenses)
  (define (lens-compose/bin outer inner)
    (lens (lambda (s-outer) (lens-get inner (lens-get outer s-outer)))
          (lambda (s-outer t-inner) (lens-update outer s-outer (lambda (s-inner) (lens-set inner s-inner t-inner))))))
  (foldr lens-compose/bin identity-lens lenses))


; example

; A Posn is a (posn Number Number)
(struct posn (x y) #:transparent)
; A Rect is a (rect Posn Posn)
(struct rect (tl br) #:transparent)

(define posn-x-lens (lens posn-x (lambda (p x) (struct-copy posn p [x x]))))
(define posn-y-lens (lens posn-y (lambda (p y) (struct-copy posn p [y y]))))

(define rect-tl-lens (lens rect-tl (lambda (r tl) (struct-copy rect r [tl tl]))))
(define rect-br-lens (lens rect-br (lambda (r br) (struct-copy rect r [br br]))))

(define rect-left-lens (lens-compose rect-tl-lens posn-x-lens))

(module+ test
  (require rackunit)
  (check-equal? (lens-get rect-tl-lens (rect (posn 1 2) (posn 3 4)))
                (posn 1 2))
  (check-equal? (lens-set rect-tl-lens (rect (posn 1 2) (posn 3 4)) (posn 5 6))
                (rect (posn 5 6) (posn 3 4)))
  (check-equal? (lens-get rect-left-lens (rect (posn 1 2) (posn 3 4)))
                1)
  (check-equal? (lens-set rect-left-lens (rect (posn 1 2) (posn 3 4)) 5)
                (rect (posn 5 2) (posn 3 4)))
  (check-equal? (lens-update rect-left-lens (rect (posn 1 2) (posn 3 4)) add1)
                (rect (posn 2 2) (posn 3 4))))
