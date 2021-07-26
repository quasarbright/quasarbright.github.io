---
title: Programming Languages
---

These are programming langauges I have made alone and with friends. Programming languages is currently my favorite area of computer science. There are so many great ideas in PL
and I really love learning more about them and implementing them myself. I love interpreters, type systems, and compilers

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
with syntax inspired by JavaScript. It was originally created as a scripting langauge for a game engine he was developing for a class on game engines. As such, it has
a foreign-function-interface for C/C++.

The language was developed in Haskell. Currently, the language is directly interpreted by monadically evaluating an AST,
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
