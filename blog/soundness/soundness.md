# Why You Want Sound Gradual Typing, and Why You Won't See it for a While

## Introduction

Sound gradual typing is something very useful that may one day make it into the mainstream. Currently, it's only in niche and research languages like Racket. There are still many problems that need to be worked out before most people would go anywhere near it. But one day, we may wonder how we lived without it. Anyway, let's find out what sound gradual typing is and why it's so useful.

## Gradual Typing

Gradual typing is a mixture of static and dynamic typing in a single language. In a gradually typed language, some parts of code can be typed and others can be left untyped. TypeScript and Python with type hints are examples of this. You can have totally untyped javascript files and typed TypeScript files in the same project and it works fine. Being to do this is very helpful.

Gradual typing gives us the best of both worlds from static and dynamic typing. Dynamic typing is good for early prototyping and exploration where a static type checker isn't necessary and just gets in the way. Certain parts of code like glue code don't benefit much from static typing anyway. In contrast, systems level programming and large, complex projects in general can be very difficult to maintain without the safety and guarantees of static typing. It helps you keep everything together. With gradual typing, you don't have to choose between one or the other. This also means it is easy to migrate from untyped to typed code.

You might want to start out a project dynamically typed so you can easily prototype and explore. Then, when things get complicated, you might want to migrate to static typing. Without gradual typing, you'd have to migrate your whole project at once between two completely different languages. This is error prone because it's not as simple as adding type annotations. Different languages have different idioms and ways of expressing programs. Type systems also restrict you to write code in a type-checkable way. It's very easy to accidentally introduce a bug in this migration. For example, let's consider this snippet of JavaScript code:

```js
function sayHello(name) {
    if (typeof name === 'string') {
        return `hello ${name}!`
    } else {
        return `hello ${name.first} ${name.last}!`
    }
}
```

Imagine trying to migrate this to Java. You'd probably have to make some sort of `Name` interface with a `sayHello` method that returns a `String`. For the string case, you'd also need to make a `StringName` class that wraps a string. Each case of this function would live in a separate class. And you'd need to make sure you wrap and unwrap `StringName`s properly all over your codebase where names are passed around. It'd be very easy to mess up somewhere. In contrast, let's see how we'd translate this to TypeScript:

```ts
type Name = {first: string, last: string} | string
function sayHello(name: Name): string {
    if (typeof name === 'string') {
        return `hello ${name}!`
    } else {
        return `hello ${name.first} ${name.last}!`
    }
}
```

All we had to do was add annotations. We didn't have to change the logic at all. This is because TypeScript is designed to accommodate JavaScript idioms and make migration non-intrusive. But enough about TypeScript which we all know and love. What is soundness?

## Safe and Sound

Have you ever had to work with untyped JavaScript code from TypeScript? If you have, you probably either migrated the JavaScript to TypeScript or added a declaration file. If you're working with an untyped library, you'd probably have to go with the latter. Figuring out what types to declare for a library's functions is non-trivial. What if you make a mistake or the library has a function that may return the wrong type? Let's consider an example:

```js
// in indexOf.js
function indexOf(str, char) {
    const i = str.indexOf(char)
    if (i === -1) {
        return undefined
    } else {
        return i
    }
}
```

Let's say that's your untyped library. You make a mistake in your declarations by thinking `idexOf` works like the usual `Array.indexOf` and returns -1 on failure:

```ts
// in indexOf.d.ts
declare module "indexOf" {
    export function indexOf(str: string, char: string): number
}
```

```ts
// in main.ts
/// <reference path="indexOf.d.ts">
import {indexOf} from "indexOf"

// finds the letter "p" in `str` and prints its 1-indexed location or "not found"
function findP(str: string): void {
    const i = indexOf(str, "p")
    if (i === -1) {
        console.log("not found")
    } else {
        console.log(i + 1)
    }
}
```

If `"p"` is not in `str`, the code will print `NaN`. If you're lucky, this kind of mistake will result in a runtime type error somewhere close to where this function was imported. If you're less lucky, this might result in a very confusing type error deep in your type-checked code, which should be impossible. Or worse, there are no errors and a correct-looking answer is returned that is, in fact, nonsense. TypeScript doesn't actually check to make sure the function always returns numbers as the declaration states. It just believes the declaration. A sound gradual type system would insert runtime checks to make sure the untyped function is of the declared type. In this case, it would check the return value every time the function is called. Let's see what this issue looks like in a language with sound gradual typing like Racket:

```scheme
; index-of.rkt
#lang racket

(provide index-of)

(define (index-of str char) #f)
```

```scheme
; main.rkt
#lang typed/racket

(require/typed "./index-of.rkt"
               [index-of (-> String Char Integer)])

(index-of "hello" #\p)

```

We get this error message:
```
index-of: broke its own contract
  promised: exact-integer?
  produced: #f
  in: (-> any/c any/c exact-integer?)    
  contract from: (interface for index-of)
  blaming: (interface for index-of)      
   (assuming the contract is correct)
  at: G:\GitHub\quasarbright.github.io\blog\soundness\examples\main.rkt:5:16
  context...:
   G:\Racket\collects\racket\contract\private\blame.rkt:346:0: raise-blame-error
   G:\Racket\share\pkgs\typed-racket-lib\typed-racket\utils\simple-result-arrow.rkt:39:12
   body of "G:\GitHub\quasarbright.github.io\blog\soundness\examples\main.rkt"
```
This error is more informative, points directly to the mistake in code (the `require/typed`), and occurs immediately when the function is called. You can also use this to perform a runtime-validated cast for any type:

```
> (cast 3 String)
broke its own contract

  promised: string?

  produced: 3

  in: string?

  contract from: cast

  blaming: cast

   (assuming the contract is correct)

  at: eval:55:0
```

This mechanism could be used to validate and type JSON:

```ts
const obj : any = JSON.parse(jsonStr)
interface Person {
    name: string
    age: number
}
const person : Person = obj as Person
```

In normal TypeScript, this "cast" on the last line wouldn't actually do anything at runtime. It would just tell the type checker to "trust you" at compile-time and carry on type checking as if `person` has the correct fields. In a sound gradually typed language like Racket, the fields would actually be checked.

There is a caveat: For functions, these runtime checks can validate that values of an unexpected type are not returned. However, they don't help you with arguments. If you declare that a function can take in any type of number, but it can only take in integers, passing in a float will result in the same kind of runtime error as if you called it from untyped code.

These runtime checks make using untyped code from typed code much safer. There are also runtime checks that make the other direction safe:

Let's say you're writing a library in TypeScript. JavaScript users can use your library just fine, but the type information is lost at runtime. If you write a TypeScript function that takes in a number, there is nothing stopping a JavaScript program from passing in a string. Again, you're lucky if you get a type error. If you want to avoid this, you might perform input validation, checking inputs of exported functions. But what if your library is used by TypeScript and these checks are unnecessary? And what if your library exports functions which take in other functions as arguments? Soundness solves these problems by inserting runtime checks when values flow from typed code to untyped code, but not when they flow from typed code to other typed code. For functions, a wrapper is inserted which checks arguments on the way in.

