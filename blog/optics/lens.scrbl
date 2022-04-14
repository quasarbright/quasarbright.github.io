#lang scribble/manual

@(require scribble/eval)

@title{lenses}
@author[@author+email["Mike Delmonaco" "mdelmonacochs@gmail.com"]]


@para{A lens is a getter and a setter. And this setter isn't mutable. Rather, it's the kind of setter that makes a new copy of something with a change. If you've ever found yourself trying to perform changes deep within a structure and the awkwardness that comes with that, you would appreciate lenses (and other optics). They are an abstraction over the notion of a reference, and they compose with each other. So you can have a lens that targets a field deep within a structure. There are other kinds of useful optics which are more exotic, but this post will just focus on lenses.}

@table-of-contents[]

@section{An example}

@codeblock{
  ; A Posn is a (posn Number Number)
  (struct posn (x y) #:transparent)
  ; A Rect is a (rect Posn Posn)
  (struct rect (tl br) #:transparent)

  (define posn-x-lens (lens posn-x (lambda (p x) (struct-copy posn p [x x]))))
  (define posn-y-lens (lens posn-y (lambda (p y) (struct-copy posn p [y y]))))

  (define rect-tl-lens (lens rect-tl (lambda (r tl) (struct-copy rect r [tl tl]))))
  (define rect-br-lens (lens rect-br (lambda (r br) (struct-copy rect r [br br]))))

  (define rect-left-lens (lens-compose rect-tl-lens posn-x-lens))

  (lens-update rect-left-lens (rect (posn 1 2) (posn 3 4)) add1)
  ; evaluates to (rect (posn 2 2) (posn 3 4))
}

In this example, we have a rectangle, which has two positions, and we make a lens that targets the x-value of the top-left of the rectangle.
