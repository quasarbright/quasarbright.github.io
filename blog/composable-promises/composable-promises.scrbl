#lang scribble/manual

@(require scribble/example racket/sandbox (for-label racket))
@(define (make-racket-evaluator)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator 'racket)))

@title{Composable Promises: Adding Laziness to a Strict Language and Collapsing Indirection}
@author{Mike Delmonaco}

Before there is any confusion, I'm not talking about JavaScript promises that are used for asynchronous computations.
In this case, a promise is just a delayed computation. For example, a simple form of a promise is a function that
takes in no arguments and returns a result. In this blog post, we will be focusing on promises that remember their
results and promises that may evaluate to other promises. Promises are useful for control flow and implementing
lazy semantics in a strict language.

In this blog post, we will learn what promises are and how to implement them efficiently. Promises are useful and
interesting, but honestly, I mainly wrote this just to talk about the algorithm for forcing composable promises because I
think it's very cool!

@section{What is a Promise?}

A promise is a delayed computation. The computation executes at most once and remembers its result.
Here are some examples of using promises:

@(define real-promises-eval (make-racket-evaluator))
@examples[
    #:eval real-promises-eval
    #:label #f
    (define p (delay (println "in the delay") 2))
    (println "in top-level")
    (force p)
    (force p)
]

Notice that the @racket["in the delay"] is only printed after the promise is @racket[force]d, not when the promise is created with @racket[delay].
And the print does not run when we @racket[force] the promise again. This tells us that the promise remembers its result and
does not re-evaluate its body upon subsequent @racket[force]s. In general, the body of a promise runs at most once. These simple promises can be
implemented by storing the return value of a zero-argument function.

@examples[
  #:eval (make-base-eval '(require racket/function))
(struct simple-promise (thunk [result #:mutable] [forced? #:mutable]))
(define (make-simple-promise thunk) (simple-promise thunk #f #f))
(define (force promise)
  (cond
    [(simple-promise-forced? promise) (simple-promise-result promise)]
    [else (let ([result ((simple-promise-thunk promise))])
            (set-simple-promise-result! promise result)
            (set-simple-promise-forced?! promise #t)
            result)]))
(define-syntax-rule (delay body ...) (make-simple-promise (lambda () body ...)))
(define p (delay (println "in the delay") 2))
(println "in top-level")
(force p)
(force p)
]

If the promise has already been forced, we return the stored result. Otherwise, we call the thunk, store the result, and return it.

One practical use of promises is for lazy streams.

@(define stream-eval (make-base-eval '(require racket/promise)))
@examples[
    #:eval stream-eval
    #:label #f
    (define empty-stream '())
    (define-syntax-rule (stream-cons a d) (cons a (delay d)))
    (define (stream-take s n)
            (cond [(or (null? s) (zero? n)) '()]
                  [(= n 1) (list (car s))]
                  [else (cons (car s) (stream-take (force (cdr s)) (sub1 n)))]))
    (define s1 (stream-cons 1 (begin (println "in the tail") (stream-cons 2 (error "boom")))))
    (stream-take s1 2)
    (stream-take s1 2)
]

@section{Composable Promises}

What if you have a promise in a promise?

@examples[
    #:eval (make-base-eval #:lang 'racket)
    #:label #f
    (define p (delay (delay 2)))
    (force p)
    (force (force p))
]

If you are using promises in a complicated way, you may end up producing promises inside of other promises like this.
For example, let's say you're implementing an interpreter for a language with lazy semantics. If you use promises,
you will likely end up producing deeply nested promises. @racket[lazy] allows us to work with these nested promises:

@examples[
    #:eval (make-base-eval #:lang 'racket)
    #:label #f
    (define p (lazy (lazy (lazy 2))))
    (force p)
]

With @racket[lazy], forcing the outer promise forces all the intermediate promises and they all remember the inner result.

@section{How Does it Work?}

Let's use the following chain of promises as a running example:

@racketblock[
(define p4 (lazy (begin (sleep 5) "hello")))
(define p3 (lazy p4))
(define p2 (lazy p3))
(define p1 (lazy p2))
]

Here is a diagram of the situation:

@codeblock{
p1 -?> p2 -?> p3 -?> p4 -?> (begin (sleep 5) "hello")
}

Here is a diagram of what we want after forcing @code{p1}:

@codeblock{
    p1 -> "hello"
    p2 -> "hello"
    p3 -> "hello"
    p4 -> "hello"
}

In these diagrams, @code{-?>} represents an un@racket[force]d promise and @code{->} represents a @racket[force]d promise.

How can we implement this? Naively, you'd write a function that just @racket[force]s promises until you get a result. Something like this:

@racketblock[
(define (force-deep p)
  (cond [(promise? p) (force-deep (force p))]
        [else p]))
]

Here is what the chain would look like after we used @code{force-deep} on @code{p1}:

@codeblock{
    p1 -> p2 -> p3 -> p4 -> "hello"
}

If we apply this function to @code{p1}, we will get back @code{"hello"}. However, if we @racket[force] @code{p1} again,
we will get @code{p2}, not @code{"hello"}. If we use @code{force-deep} again, we will get @code{"hello"}
and it will be faster than the first time since the result is remembered and the @racket[sleep] would not run again.
However, each promise stores its child, not the inner result. This means that when we
use @code{force-deep} on @code{p1} again, even though all the promises were already forced, we would still have to traverse
the whole "chain" again just to access the inner result stored in the innermost promise. We want each promise to remember the inner result,
not its child promise.

Here is another attempt, ignoring regular promises for the moment:

@racketblock[
    (struct composable-promise (thunk [result #:mutable] [forced? #:mutable]))
    (define (make-composable-promise thunk) (composable-promise thunk #f #f))
    (define (force promise)
      (cond
        [(composable-promise-forced? promise) (composable-promise-result promise)]
        [else (let ([initial-result ((composable-promise-thunk promise))])
                (if (composable-promise? initial-result)
                    (let ([result (force initial-result)])
                      (set-composable-promise-result! promise result)
                      (set-composable-promise-forced?! promise #t)
                      result)
                    (begin (set-composable-promise-result! promise initial-result)
                           (set-composable-promise-forced?! promise #t)
                           result)))]))
(define-syntax-rule (lazy body) (make-composable-promise (lambda () body)))
]

As before, if the promise has already been forced, we return the stored result. Otherwise, we start by calling the thunk to get an initial result.
If that is yet another composable promise, we force that one too. Then we store and return that promise's result. If the thunk returned a regular
value, we store and return it. The recursive case will go all the way down the chain, forcing every inner promise and storing the inner value, not
the inner promise itself.

This is better. If we have a chain of promises and we force it, each promise will remember its result. Here is the diagram after forcing:

@codeblock{
    p1 -> "hello"
    p2 -> "hello"
    p3 -> "hello"
    p4 -> "hello"
}

That's exactly what we want! Forcing @code{p1} once more will immediately return the stored result and wouldn't
have to traverse the chain again. But there is still one problem: that @code{(force initial-result)} does not occur in tail position. This means that the forcing algorithm will use stack space
that grows linearly with respect to the chain length. How can we implement this with tail recursion?

Consider this diagram:

@codeblock{
    a -?> b -?> c -?> ...
}

Let's say we shallow-ly force @code{a}.

@codeblock{
    a -> b -?> c -?> ...
}

We realize that @code{a} results in another promise, so we shallow-ly force that too.

@codeblock{
    a -> b -> c -?> ...
}

Next, we swap @code{a} and @code{b}.

@codeblock{
    b -> a -> c -?> ...
}

Then, we repeat from @code{a}.

Shallow-ly force it.

@codeblock{
    b -> a -> c -?> ...
}

It was already forced, so nothing changed. Next, we realize that the result is another promise, namely @code{c}. So we shallow-ly force that too.

@codeblock{
    b -> a -> c -> ...
}

Next, swap @code{a} and @code{c}.

@codeblock{
    b -> a -> ...
         ↑
         c
}

Then, we repeat from @code{a} and continue until @code{a} points to a non-promise or its child points to a non-promise. In either of those two cases,
We store the non-promise.

Here is what we'd get if we ran this algorithm on @code{p1}:

@codeblock{
    p2 -> p1 -> "hello"
          ↑     ↑
          p3    p4
}

In general, the pattern will be

@codeblock{
    p2   \
    ... - * -> p1 -> v
    pn-1 /           ↑
                     pn
}

Intermediate promises will store the outer-most promise, the outer-most promise will store the final value, and the inner-most promise will store the final value.
Each promise is at most 2 levels of indirection away from the final value. If we forced @code{p2} from this example after forcing @code{p1}, it would end up pointing directly to the final value.

This is not quite what we wanted. We wanted all promises to store the final value directly. Realistically, however, a single layer of indirection for inner promises doesn't matter. Especially when it would end up being collapsed
if an inner promise was forced afterwards. Asymptotically, we still get linear time with respect to chain length on the first force and constant time on any subsequent force on a promise in the chain. That's the same as our
naive algorithm. However, our naive algorithm used linear space, whereas this algorithm uses constant space. This is a tradeoff.

Either way, I think this algorithm is pretty cool! In a single, linear-time, constant-space pass, we collapse the indirection. Here is the code:

@codeblock{
    (struct composable-promise (thunk [result #:mutable] [forced? #:mutable]))
    (define (make-composable-promise thunk) (composable-promise thunk #f #f))
    (define (shallow-force promise)
      (cond
        [(composable-promise-forced? promise) (composable-promise-result promise)]
        [else (let ([result ((composable-promise-thunk promise))])
                (set-composable-promise-result! result)
                (set-composable-promise-forced?! #t)
                result)]))
    (define (force promise)
      (let ([result (shallow-force promise)])
        (cond
          [(composable-promise? result)
           (let ([inner-result (shallow-force result)])
             (cond
               [(composable-promise? inner-result)
                ; swap and recur
                (set-promise-result! result promise)
                (set-promise-result! promise inner-result)
                (force promise)]
               [else
                (set-promise-result! promise inner-result)
                inner-result]))]
          [else result])))
    (define-syntax-rule (lazy body) (make-composable-promise (lambda () body)))
}

This implementation is missing a lot of things. There are no regular promises, @code{lazy} doesn't support multi-expression bodies,
there is no error handling, there is no support for @racket[values], and many more subtle things the real promise library handles. Regardless,
this provides insights into the inner workings of the library.
