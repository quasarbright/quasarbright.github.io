---
title: Programming Languages
---
* TOC
{:toc}

These are programming languages and programming langauge-related projects I have made alone and with friends. Programming language theory is currently my favorite area of computer science. There are so many great ideas
and I really love learning more about languages and implementing them myself. I love interpreters, macros, type systems, and compilers.

Some of these are general purpose languages made from scratch, some are domain-specific languages embedded in other languages, and some are in between.

This is arranged roughly chronologically.

# [Subduce](https://subduce.readthedocs.io/en/latest/#)
[Subduce](https://github.com/quasarbright/Subduce) is the first programming language I made. It is a functional programming language based on Python and racket's advanced student language.
It has racket-like expression syntax,
but python-like function definitions and variable assignments. I like racket and pure functional programming, but I'm not a fan of all the parentheses of racket.
I really like python and its syntax, so I tried to get the best of both worlds. The code is stored in its own repository, Subduce.

Note a few years later: Since subduce, I've taken a course on compilers, learned Haskell, and made many little programming languages. Java wasn't a good choice for
language development, but I didn't know about Haskell at the time. I'm impressed by what I managed to figure out, since my only learning resource for creating
programming languages was my professor quickly showing me how to make a tiny lambda calculus interpreter on his whiteboard.

# [PongChamp](https://github.com/quasarbright/PongChamp)

[PongChamp](https://github.com/quasarbright/PongChamp) is an imperative programming language my friend [Ryan Mitchell](https://github.com/RyanRio) and I made together. It is a scripting language
with syntax inspired by JavaScript. It was originally created as a scripting language for a game engine he was developing for a class on game engines. As such, it has
a foreign-function-interface for C/C++.

The language was developed in Haskell. Currently, the language is directly interpreted by directly evaluating/running an AST,
but we are considering byte-code compilation as of writing this.

There are many features planned, including classes, a module system, continuations, and algebraic effects

# [OCovid](https://github.com/quasarbright/OCovid)

An OCaml subset with algebraic data types, deep pattern matching, and type inference written in Haskell.

Type inference is a really cool language feature and I think this was my first time getting it totally right in a language with ADTs and pattern matching

I named the language "OCovid" because it uses OCaml syntax and I made it while I had Covid-19. This is the most impressive language I've made as of writing this IMO

# [HCC](https://github.com/quasarbright/HCC)

A C compiler written in Haskell.

Supports functions, arrays, pointers, ints. Targets x86 assembly. Has a type-checker.

C pointers are cool. They can be messy, but I really like the concept, and I wanted to implement a compiler for a language with pointers.

This is the biggest hobby compiler I've made. Making a compiler involves a lot of book keeping. I wrote this before I learned about lenses. If you don't know lenses,
it's a huge pain to expand your context/state in your monad transformer stack. Maybe one day, I'll revisit the project, add lenses for the context and state, and
add more features like structs, which require a lot of book keeping.

# [Funduce](https://github.com/quasarbright/Funduce)

A Haskell remake of subduce, the first language I made. This language is literally an implementation of a subset of advanced student language. Like subduce, it has
structs, a repl, primitive types, etc. Unlike subduce, it uses ASL's S-expression syntax.

# [A language with lisp-like macros and quasi-quoting](https://github.com/quasarbright/learn-racket/blob/master/macro-interpreter.rkt)

I made a little language with lisp-like macros in racket. Racket is great for making little interpreters since you get a free lexer by interpreting racket datums. I've
never been able to add a feature to a language as quickly as I did in racket. The pattern matching is genius, and I love quasi-quoting

Anyway, this language is an enriched lambda calculus with quasi-quoting and a construct for defining macros which are syntax-to-syntax functions.
These macros are immediately expanded in the rest of the program after they are defined. Quasi-quoting was the most interesting thing to implement in that project. Very cool!

# [ContEffects](https://github.com/quasarbright/ContEffects)

This a tiny language with simple algebraic effects. I learned about algebraic effects from [this article](https://overreacted.io/algebraic-effects-for-the-rest-of-us/) and 
reading about [the koka language](https://koka-lang.github.io/koka/doc/book.html) and was very fascinated. It seemed like a nice way of structuring effectful code.

I noticed that `Cont` in Haskell could be used to do something similar to algebraic effects, but it doesn't handle union data well and is awkward to work with in that way.
Regardless, I explored the functor, applicative, and monad instances for `Cont` and `ContT`, and gained some insight on the nature of continuations and these kinds of effects.

I treat effects as "resumable exceptions". You "perform" an effect in the same way that you'd throw an exception, but some handler must handle the effect and resume the computation,
providing a value. The "perform" expression evaluates to this value, and control flow resumes at the performance site.

If an effect reaches the top-level unhandled, the program crashes.

This is accomplished by translating handlers into functions and running effectful code with a handler. Essentially, performing an effect just evaluates to a function call
on whatever handler the code is running under.

A continuation-based approach would probably lead to a more powerful, robust, extensible system for algebraic effects, but this was the idea I had and it works well for this
small language, which is pretty cool!

# [Reactive](https://github.com/quasarbright/Reactive)

an excel-like reactive programming language. Similar to excel, you can define "cells" which contain formulas referring to other cells. Cells' values are reactively updated
when cells they depend on are updated. There is also support for differential variables. For example, if you define `x = 1`, `dx = .1`, you can perform a step in which variables
are incremented by their differentials, which will result in `x = 1.1`, `dx = .1`. This makes it useful for numerically solving simple differential equations

# [MiniML](https://github.com/quasarbright/MiniML)

An small OCaml subset with type inference, arithmetic, boolean logic, and functions written in OCaml.
I'm pretty sure I wrote this while reading Simon Peyton Jones' "the implementation of functional programming languages"

<!-- # [A Dependently Typed Language](https://github.com/quasarbright/learn-racket/blob/master/dependent-types.rkt)

A tiny dependently typed language based on [this paper](https://www.andres-loeh.de/LambdaPi/LambdaPi.pdf). In a dependently typed language, the return type of a function can depend on the _value_ of the argument passed to it. But it's still "statically" type checked. There is a little bit of evaluation during type checking. -->

# [Counter Machine Language](https://github.com/quasarbright/learn-racket/blob/master/counter-machine.rkt)

An assembly-like language for [counter machines](https://en.wikipedia.org/wiki/Counter_machine) inspired by [this computerphile video](https://www.youtube.com/watch?v=PXN7jTNGQIw&ab_channel=Computerphile).

# [Delimited Continuations](https://github.com/quasarbright/learn-racket/blob/master/delimited-continuations.rkt)

This isn't really a standalone programming language. It's a pre-processor for racket which transforms the program to continuation passing style supporting delimited continuations, then invokes racket's interpreter to evaluate the program. Delimited continuations allow control flow constructs like exceptions, generators, multi-threading, and non-determinism to be implemented as libraries. That's right, libraries! In a language with delimited continuations, these constructs don't have to be baked into the implementation of the language. For examples of what you can do with delimited continuations, see [my implementation of algebraic effects](https://github.com/quasarbright/learn-racket/blob/master/algebraic-effect-2.rkt).

# [Pattern Matcher](https://github.com/quasarbright/learn-racket/blob/master/match.rkt)

Pattern matching allows you to do case analysis and de-structure data based on its shape. See [the wikipedia page](https://en.wikipedia.org/wiki/Pattern_matching) for some examples. This is an extremely powerful and useful tool mostly from academic languages which is starting to [creep into the mainstream](https://peps.python.org/pep-0636/).

This is a domain-specific language, not a general purpose programming language.

# [Miniclass](https://github.com/quasarbright/miniclass)

This is an implementation of a basic class system for Racket. It is a collection of macros that compiles class syntax to struct usages with some book-keeping. This repository actually contains a few implementations. It was developed to compare a few different methods of developing a macro-based DSL in Racket as part of a PL research paper I helped with.

# [Sytax Spec](https://github.com/michaelballantyne/syntax-spec)

This is a meta language for creating sophisticated domain specific languages in Racket. This is Michael Ballantyne's project, not mine. I was his research assistant and helped him develop it.

It is a useful tool for developing the frontend (expander, binding checking, etc.) of a macro-based compiler for a DSL in Racket. Syntax spec makes it super easy to create sophisticated macro-extensible DSLs in racket without having to implement your own DSL expander.

# [Functional Reactive Programming](https://github.com/quasarbright/frp/blob/master/main.rkt)

A tiny functional reactive programming library/DSL. Reactive programming is like a spreadsheet with formulas. When You change a value, formula cells that depend on that value are updated automatically, and only when they need to be. This sort of system can be embedded in a general purpose programming langauge as a library, like React.js.

# [Pi Calculus](https://github.com/quasarbright/pi-calculus/blob/master/main.rkt)

An implementation of the [pi calculus](https://en.wikipedia.org/wiki/%CE%A0-calculus) as a DSL. The pi calculus is like the lambda calculus, but for concurrent programming instead of functional programming. It is a very tiny langauge that captures the essence of concurrency and is important to the theory of concurrent programming.

# [Mini Kanren](https://github.com/quasarbright/mike-kanren/blob/master/main.rkt)

An implementation of mini kanren, a logic programming language like prolog.

Repository includes a [type checker implemented using mini kanren](https://github.com/quasarbright/mike-kanren/blob/master/private/type-checker.rkt)

# [ZFC++](https://github.com/quasarbright/learn-racket/blob/master/zfcpp.rkt)

An implementation of [ZFC++](https://github.com/quasarbright/learn-racket/blob/master/match.rkt), an esoteric language that operates on sets.

# [Async Await](https://github.com/quasarbright/learn-racket/blob/master/async-await.rkt)

An implementation of a language with JavaScript-style promises and the desugaring of async/await to using promises.
Similar to delimited continuations, the desugaring is a pre-processing step, and the program that invokes the Racket interpreter afterwards. I'm not going to make a whole end-to-end language every time I want to do something interesting! With Racket, it's easy to make languages without having to implement the whole language. I trivially leveraged Racket's parser and evaluator, and was able to focus solely on the runtime features and the pre-processing.

