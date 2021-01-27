---
title: How to implement type inference Part 2 - Implementation Boogaloo
---
* TOC
{:toc}


In this part, we will walk through implementing algorithm J in Haskell. You don't a lot of experience with Haskell to Follow, but some experience with functional programming in general is recommended, since we will be using algebraic data types, pattern matching, and recursion. We will also be using monads. I will explain everything as I go, so don't worry if you don't have a lot of familiarity with Haskell or functional programming or monads.

All the code is [here](https://github.com/quasarbright/TypeInferenceTutorial)

If you want to follow along, your file structure should look like this:

```
src
- AST.hs
- UnionFind.hs
- Typing.hs
```
and you'll need to add the `containers` package as a dependency.
# Introduction

Last part, we created a rule system for inferring the types of expressions based on which kind of expression we were looking at. As a refresher, Let's review:

We have mono types, which are either Type variables, primitive types $nat,bool$, and function types $a \rightarrow b$. We also have type schemes, which are mono types with foralls on the outside, like $\forall a. a \rightarrow a$. Our typing judgements say that under a context $\Gamma$ (which maps expression variable names to type schemes), an expression $e$ has type $\tau$, written as $\Gamma \vdash e : \tau$. These rules also have requirements above them which also have to be "ran" or "satisfied" before inferring the expression. These requirements can be other expression inferences, instantiating type schemes to mono types, generalizing mono types to type schemes, unifying two types using a union find, and making new type variables.

Here are all the rules we created for type inference:

$$
\frac
{(x:\sigma) \in \Gamma \qquad \tau=instantiate(\sigma)}
{\Gamma \vdash x : \tau}
Var
$$

$$
\frac
{}
{\Gamma \vdash n : nat} Nat
$$

$$
\frac
{}
{\Gamma \vdash true : bool} True
$$

$$
\frac
{}
{\Gamma \vdash false : bool} False
$$

$$
\frac{\Gamma \vdash e_1 : \tau_1 \qquad\Gamma\vdash e_2 : \tau_2 \qquad \tau' = newvar \qquad unify(\tau_1,\tau_2 \rightarrow \tau')}
{\Gamma \vdash e_1\ e_2 : \tau'}App
$$

$$
\frac
{\tau = newvar \qquad \Gamma,(x:\tau) \vdash e : \tau'}
{\Gamma \vdash \lambda x.e : \tau \rightarrow \tau'}
Abs
$$

$$
\frac
{\Gamma \vdash e_1 : \tau \qquad \sigma = \bar{\Gamma}(\tau) \qquad \Gamma,(x:\sigma) \vdash e_2 : \tau'}
{\Gamma \vdash \textrm{ let } x = e_1 \textrm{ in } e_2 : \tau'}
Let
$$

$$
\frac
{\Gamma \vdash e_1 : \tau_1 \quad unify(\tau_1,bool) \quad \Gamma \vdash e_2 : \tau_2 \quad \Gamma \vdash e_3 : \tau_3 \quad unify(\tau_2,\tau_3)}
{\Gamma \vdash \textrm{ if } e_1 \textrm{ then } e_2 \textrm{ else } e_3 : \tau_2} If
$$

Using these rules, we can implement type inference for our little language such that we can infer the type of any well-typed expression, as well as report type errors.

Let's get to it!

# Abstract Syntax Tree

The first thing we need to do is represent our language in Haskell. We will do this using algebraic data types.

Before that, let's write our module header and some imports:

```haskell
module AST where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
```

We name our module `AST` and import the `Map` and `Set` modules. This `qualified` business is so all the map functions, like `lookup` are under the map namespaces, so we'd write `Map.lookup`. This avoids name clashing. But we also specially import the type name `Map`, so we don't have to say `Map.Map` when we're talking about the `Map` type.

These imports require the `containers` package to be installed, so make sure you add it to your `package.yaml` file if you're using stack:

```yaml
dependencies:
- base >= 4.7 && < 5
- containers
```

First, let's define what an expression is:

```haskell
data Expr = Var String
          | Nat Integer
          | Bool Bool
          | Lambda String Expr
          | App Expr Expr
          | Let String Expr Expr
          | If Expr Expr Expr
          deriving(Eq, Ord, Show)
```

If you're not familiar with Haskell or functional programming in general, this may seem strange to you, so let's unpack what's going on here. In languages like Java, you'd define a class if you want to make a new data type. For union data, which is where there are different kinds of a value, like different kinds of expression, you'd make an `Expr` interface and each case would have its own class implementing the interface. But in Haskell, defining union data is very easy, you just write something like that. And rather than write each method in each case class, we write each function once, and handle all the cases in the function using pattern matching.

Each of these cases in the definition has a constructor like `Lambda`, and specifies its "fields", which is the data each `Lambda` will have. In the case of `Lambda`, each lambda has a `String` for its variable name and an `Expr` for its body.

That `deriving(Eq,Ord,Show)` business at the bottom just tells Haskell to automatically define structural equality, comparison and conversion to a string for our Expr types. We can define these instances ourselves, but the default will be fine for us.

We have one constructor for each kind of expression. Lambdas `Lambda "x" (Var "x")` have their argument name and body expression. Application is between two expressions, so for multi-argument application `f x y`, we do `App (App (Var "f") (Var "x")) (Var "y")`. For let expressions `let x = 1 in x`, we have the variable name, the right-hand-side expression, and the body expression `Let "x" (Nat 1) (Var "x")`. And for if expressions, we have the condition, the then branch, and the else branch `If (Bool True) (Nat 1) (Nat 0)`. We use Integers instead of natural numbers, but it doesn't really make a difference for typing.

Let's make the identity function example we looked at last time at the end in terms of these constructors:

$$\textrm{ let } id = \lambda x . x \textrm{ in } id\ 1$$

```haskell
Let "id" (Lambda "x" (Var "x")) (App (Var "id") (Nat 1))
```

Now, let's define what a mono type is:

```haskell
data Mono = TVar String
          | TNat
          | TBool
          | TArr Type Type
          deriving(Eq, Ord, Show)
```

Again, we tell Haskell to implement equality, comparison, and conversion to a string for us.

So if we wanted to represent $nat \rightarrow nat \rightarrow bool$, we'd use `TArr TNat (TArr TNat TBool)`

Now let's define type schemes:

```haskell
data Scheme = SForall String Scheme
            | SMono Mono
            deriving(Eq, Ord, Show)
```

This structure guarantees that a type scheme can only be a bunch of foralls and then a mono type on the inside. For example, let's define the type scheme of `applyFunction f x = f x`

$$ \forall a. \forall b.(a \rightarrow b) \rightarrow a \rightarrow b $$

```haskell
SForall "a" (SForall "b" (SMono (TArr (TArr (TVar "a") (TVar "b"))
                                      (TArr (TVar "a") (TVar "b")))))
```

Remember, function types are right associative, so if we drew the parentheses in, we'd have written

$$ \forall a. \forall b.(a \rightarrow b) \rightarrow (a \rightarrow b) $$

Now, let's define what a context is:

```haskell
type Context = Map String Scheme
```

What's up with this `Map String Scheme` business? If you're not familiar, Haskell has higher order types, similar to Java generics. Instead of writing `Map<String,Scheme>` like you would in Java, you write `Map String Scheme`. You could think of the `Map` type as a "type function", or as we call it in Haskell, a "type constructor". And it takes in types as its arguments.

This is definition is different from our other ones. We are defining a `type`, not `data`. `type` is what you use when you define a type alias. In this case, we use a `Map` from `String` to `Scheme`. Everywhere we see `Context`, we're really talking about `Map String Scheme`, but this type alias make sure we never forget that it's a context. We don't define any constructors like `Lambda`, since this is just a type alias.

Now lets define some functions for operating on our AST.

One thing we'll need to do a lot is finding the free variables of mono types, schemes, and contexts. So let's write those functions here:

```haskell
freeMonoVars :: Mono -> Set String
freeMonoVars (TVar x) = Set.singleton x
freeMonoVars TNat = Set.empty
freeMonoVars TBool = Set.empty
freeMonoVars (TArr arg ret) = Set.union (freeMonoVars arg) (freeMonoVars ret)
```

This is an example of pattern matching. It's kind of like we are defining different versions of the function for each constructor of the `Mono` data type. We have patterns, which include constructors and variables. If the argument matches the pattern, the variables are in scope on the right-hand-side of the `=`.

In a mono type, all variables are free, so we're really just recurring on child types and unioning. The base cases are type variables and primitive types, for which we return a set containing just the type variable, and an empty set respectively.

If you're not used to recursion, make sure you think about why this works and go through an example like `nat -> (a -> b)`, which should return `{a,b}`

Here is how we get the free variables of a scheme:

```haskell
freeSchemeVars :: Scheme -> Set String
freeSchemeVars (SForall x s) = Set.delete x (freeSchemeVars s)
freeSchemeVars (SMono t) = freeMonoVars t
```

If we have `forall a . a -> (b -> c)`, we want it to return `{b,c}`, not `{a,b,c}`. So in the `SForall` case, we have to delete the quantified variable `x` from the set that `freeSchemeVars s` recursively evaluates to, which in this case would be `{a,b,c}`. The base case is the `SMono` case, where we just return the free variables of the mono type using `freeMonoVars`. So what ends up happening is we get all the free variables of the inner mono type, without the variables quantified by foralls. Which is exactly what we want for a scheme's free variables.

Here is how we get the free variables of a context:

```haskell
freeCtxVars :: Context -> Set String
freeCtxVars ctx = 
    let schemes = Map.elems ctx
        schemeVarSets = fmap freeSchemeVars schemes
    in Set.unions schemeVarSets
```

Remember, the definition of a context's free variables is the union of all of its schemes' free variables.

There's a lot going on here. First, let's remember that `Context` means `Map String Scheme`.
The first thing that happend is that we define a local variable `schemes`, which is `Map.elems ctx`. `Map.elems` takes in a map and returns its values in a list. Its signature is `Map.elems :: Map k a -> [a]`. So since `Context` is `Map String Scheme`, this is `[Scheme]`. By the way, we write list types like `[Scheme]` for a list of schemes.

Then, we define `schemeVarSets` to be `fmap freeSchemeVars schemes`. `fmap` basically applies a function (its first argument) to each element of its second argument. Since schemes is a list of schemes `[Scheme]` and `freeSchemeVars` converts a `Scheme` to a `Set String`, `schemeVarSets` must be a `[Set String]`, or a list of sets of strings. The signature for `fmap` for lists is `(a -> b) -> [a] -> [b]`. `fmap` actually works for things other than lists, but we'll get to that later.

Finally, we call `Set.unions`, which just computes the set union of a bunch of sets.

So all this function does is union all the schemes' free variables in the context. Which follows our definition.

Now we need to define substitutions. This is how we'll do our replacements when we instantiate schemes. A substitution replaces a type variable with some other type. So these functions will be of type `String -> Type -> a -> a`, where `a` is either `Mono` or `Scheme`.

Sometimes, we want to perform a bunch of substitutions simultaneously, so let's actually define functions with the signature `Map String Mono -> a -> a` instead, and we'll define our single substitution functions using our multi substitution functions. The map represents all the substitutions we want to make.

```haskell
subsMono :: Map String Mono -> Mono -> Mono
subsMono subs (TVar x) =
    case Map.lookup x subs of
        Nothing -> TVar x -- not a target
        Just t -> t -- needs to be replaced
subsMono _    TNat = TNat
subsMono _    TBool = TBool
subsMono subs (TArr arg ret) = TArr (subsMono subs arg) (subsMono subs ret)
```

The interesting case is `TVar x`. Here, we call `Map.lookup x subs`. Map.lookup takes in a key and a map and returns a `Maybe a`, where `a` is the value type. Here is the definition of `Maybe a`:

```haskell
data Maybe a = Nothing | Just a
```

A `Maybe a` is like an `Optional<T>` in Java if you're familiar. It's Haskell's way of handling a value that might not exist, instead of using something like `null` like other languages. Since a key might not be in a map, it might not find a value in `lookup`. So lookup returns a `Maybe a`. It returns `Nothing` when the key does not exist, and `Just value` when the key is in the map.

Now what about `case`? case is just a pattern match expression. Sort of like what we do in our function definitions, but inlined to a single expression.

So if `x` isn't a key in `subs`, we don't need to replace it, so we reach the `Nothing` case and just return `TVar x` as it was. Otherwise, if it is a key, we instead return its replacement in the `Just t` branch.

In the `TNat,TBool` cases, we don't even need to look at `subs` since we know we won't be substituting, so we replace the argument with `_`, which basically means "I don't care".

For the function type case, we just recursively substitute the child types.

Now we can implement single substitution in terms of this:

```haskell
subMono :: String -> Mono -> Mono -> Mono
subMono target replacement t = subsMono (Map.singleton target replacement) t
```

Now scheme substitution:

```haskell
subsScheme :: Map String Mono -> Scheme -> Scheme
subsScheme subs (SForall x s) =
    let subs' = Map.delete x subs
    in SForall x (subsScheme subs' s)
subsScheme subs (SMono t) = SMono (subsMono subs t)
```

We don't want to substitute quantified variables, so we delete them from the substitution when recurring in the `SForall` case. And again, the basecase just delegates to mono type substitution.

Scheme single substitution:

```haskell
subScheme :: String -> Mono -> Scheme -> Scheme
subScheme target replacement s = subsScheme (Map.singleton target replacement) s
```

All together, we have:

```haskell
module AST where

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data Expr = Var String
          | Nat Integer
          | Bool Bool
          | Lambda String Expr
          | App Expr Expr
          | Let String Expr Expr
          | If Expr Expr Expr
          deriving(Eq, Ord, Show)

data Mono = TVar String
          | TNat
          | TBool
          | TArr Mono Mono
          deriving(Eq, Ord, Show)

data Scheme = SForall String Scheme
            | SMono Mono
            deriving(Eq, Ord, Show)

type Context = Map String Scheme

freeMonoVars :: Mono -> Set String
freeMonoVars (TVar x) = Set.singleton x
freeMonoVars TNat = Set.empty
freeMonoVars TBool = Set.empty
freeMonoVars (TArr arg ret) = Set.union (freeMonoVars arg) (freeMonoVars ret)

freeSchemeVars :: Scheme -> Set String
freeSchemeVars (SForall x s) = Set.delete x (freeSchemeVars s)
freeSchemeVars (SMono t) = freeMonoVars t

freeCtxVars :: Context -> Set String
freeCtxVars ctx = 
    let schemes = Map.elems ctx
        schemeVarSets = fmap freeSchemeVars schemes
    in Set.unions schemeVarSets

subsMono :: Map String Mono -> Mono -> Mono
subsMono subs (TVar x) =
    case Map.lookup x subs of
        Nothing -> TVar x -- not a target
        Just t -> t -- needs to be replaced
subsMono _    TNat = TNat
subsMono _    TBool = TBool
subsMono subs (TArr arg ret) = TArr (subsMono subs arg) (subsMono subs ret)

subMono :: String -> Mono -> Mono -> Mono
subMono target replacement t = subsMono (Map.singleton target replacement) t

subsScheme :: Map String Mono -> Scheme -> Scheme
subsScheme subs (SForall x s) =
    let subs' = Map.delete x subs
    in SForall x (subsScheme subs' s)
subsScheme subs (SMono t) = SMono (subsMono subs t)

subScheme :: String -> Mono -> Scheme -> Scheme
subScheme target replacement s = subsScheme (Map.singleton target replacement) s
```

Ok, now we're done with our basic type operations. Now let's implement the union find.

# Union Find

Recall that a union find is a data structure containing a collection of objects which are grouped together. It supports two operations: union, which merges two groups together, and find, which returns a representative to identify an objects group.

We will implement a `UnionFind a` as a `Map a a`, which will map each element to its representative.

Let's start with module header and imports:

```haskell
module UnionFind where

import qualified Data.Map as Map
import Data.Map(Map)
```

Now let's define our `UnionFind a` type:

```haskell
type UnionFind a = Map a a
```

Again, this is a type alias. But this time, it takes in a type `a`.

now let's implement find:

```haskell
find :: Ord t => UnionFind t -> t -> t
find m k =
    case Map.lookup k m of
        Just rep
            | k == rep -> rep -- we are our own representative, done. 
            | otherwise -> find m rep -- follow the chain of representatives 
        Nothing -> k
```

It is possible that there is a chain of "pointers" in our map. It is not always the case that every element points to its group's representative directly. There may be some indirection. But we know that the true representative of a group is its own direct representative, so we just have to keep following the pointers until we reach that case.

What does `Ord t =>` mean? It basically just says that `t` has to be comparable, since the implementation of `Map k a` uses comparisons between keys. `Ord` is a type class for comparable types, which is like an interface in Java. We will get into type classes a little more later, but the important thing for us is that our keys are `String`s, which are comparable. In other words, they have an `Ord` instance.

So we try to find `k`'s direct representative. If it has none, we say that it is its own representative. That just means it hasn't been unioned with anything yet, so it is in its own group by itself. If it has a representative, there are two possibilities. We use guard syntax here, which is haskell's way of nicely handling if-else if-else if-else in case expressions. If `k == rep`, that means `k` is its own representative, so we return it. Otherwise (else), we need to recur, so we find `rep`'s representative.

Ok, now union:

```haskell
union :: Ord k => UnionFind k -> k -> k -> UnionFind k
union m a b =
    let rep = find m a
        m' = Map.insert a rep (Map.insert b rep m)
    in m'
```

Union's job is to make sure that both elements end up with the same representative, and therefore are in the same group afterwards. But this is functional programming, so rather than mutate the unionfind, we return an updated unionfind.

The union find should follow this law for all `uf,a,b`:

```haskell
find (union uf a b) a == find (union uf a b) b
```

In other words, after unioning `a,b`, they should both have the same true representative.

We start by finding `a`'s representative and calling it `rep`. Then, we update `a,b` to both have `rep` as their direct representative. The order of arguments matters here, since we specifically use `a`'s representative, not `b`'s. We need to keep this in mind when we're doing unification.

All together, we have:

```haskell
module UnionFind where

import qualified Data.Map as Map
import Data.Map(Map)

type UnionFind a = Map a a

find :: Ord t => UnionFind t -> t -> t
find m k =
    case Map.lookup k m of
        Just rep
            | k == rep -> rep -- we are our own representative, done. 
            | otherwise -> find m rep -- follow the chain of representatives 
        Nothing -> k
    
union :: Ord k => UnionFind k -> k -> k -> UnionFind k
union m a b =
    let rep = find m a
        m' = Map.insert a rep (Map.insert b rep m)
    in m'
```

Now we're ready to start doing some type inference!

# Type Inference

Let's start with the module header and imports:

```haskell
module Typing where

import UnionFind
import AST

import Data.Map(Map)
import qualified Data.Map as Map
import Data.Set(Set)
import qualified Data.Set as Set
import Data.List (nub)
```

Type inference may result in a type error if the program is not well-typed. So let's define a data type for type errors:

```haskell
data TypeError = Mismatch Mono Mono
               | OccursError String Mono
               | UnboundVar String
               deriving(Eq, Ord, Show)
```

A mismatch occurs when we try to unify a `TNat` and a `TBool` for example. It's the basic type error. an occurs error happens when we try to unify two types in a way that would construct an infinite type. For example, if we tried to unify `t, int -> t`, that would lead to the type `t = int -> int -> int -> ...`, which we don't want to allow. And unbound variables are simply when an exprssion uses a variables that it hasn't defined, like in `let x = 2 in y`, `y` is an unbound variable.

Our type checker needs global, mutable state. We want the union find to be updated globally and stay consistent throughout the entire algorithm. We also need global mutation to generate globally unique names for `newvar`. But we don't do mutation in functional programming, so the best we can do is to just simulate it ourselves. If a function needs to "mutate" a value, it instead takes it in and returns the new value. But if we're doing this as part of some other computation that returns something of its own, we need to return the result, along with the new state.

So let's say we're doing type inference for an expression. We want to have a function

```haskell
inferExpr :: Expr -> Mono
```

But if we want to mutate the state, we also need to take in the state and return it. So let's update the signature:

```haskell
inferExpr :: Expr -> CheckerState -> (CheckerState, Mono)
```

We return a 2-tuple, or a pair containing the new state and the inferred type. This is how you return multiple values in Haskell. We'll figure out what `CheckerState` is soon.

Is the context part of the state? No, actually. The state needs to globally mutate, so we need to keep track of it going in and out of our computations. But the context "only goes in". Our computations are going to be branching all over the place, and the context is going to be locally, temporarily modified as we recur into sub-expressions. Think about the rule for lamdba inference:

$$
\frac
{\tau = newvar \qquad \Gamma,(x:\tau) \vdash e : \tau'}
{\Gamma \vdash \lambda x.e : \tau \rightarrow \tau'}
Abs
$$

When we infer the type of the body, we add $(x:\tau)$ to the context. This corresponds to a `Map.insert` in haskell. After we "come back out of" that recursive `inferExpr` call, we want to keep the changes to the state, but we don't really care what else is added to the context inside of that computation. So we will only take in a context, but we won't return it since we don't care about what happens to it. In fact, it's as if nothing "happens" to it at all, rather, we are just locally modifying it before passing it to another computation that takes in a context.

So now we have a new signature:

```haskell
inferExpr :: Expr -> Context -> CheckerState -> (CheckerState, Mono)
```

But what about type errors? In Haskell, there is no exception system with try/catch like other languages. Instead, we treat errors as values themselves. We have a type `Either a b` for managing errors:

```haskell
data Either a b = Left a | Right b
```
So if a computation may result in an error of type `a` or some value of type `b`, we return an `Either a b` and use pattern matching to handle the possible error.

Notice the similarity with `Maybe a`. We needed `Maybe` to talk about computations which may not return a value, and we need `Either` for computations that may result in an error or return a value. There is an abstraction over these kinds of "wrapper" types, or rather, these types of computations, called monads. We will get into this soon and define our own monad for type checking.

Since inference can result in a type error, we want to return `Either TypeError Mono`. But we still need to keep track of the state, so we'll return `Either TypeError (CheckerState, Mono)`.

Now, our final signature:

```haskell
inferExpr :: Expr -> Context -> CheckerState -> Either TypeError (CheckerState, Mono)
```

Think about how we'd use the output of this function. Let's say we took in a `Context ctx` and a `CheckerState s`

```haskell
case inferExpr e ctx s of
    Left err -> Left err -- propagate error
    Right (s',t) -> ...something using t... in (s'',a) -- s'' is whatever the state is after ...
```

This is a lot of machinery when we really just care about the `Mono`. We have to pass this context around, thread the state, propagate errors, it's a huge pain. And very prone to error, since we might accidentally use an old state for something if we're doing a lot of operations in sequence. But it's necessary to simulate the behaviors that we want, like local context, mutable state, and error handling. In other languages, this stuff is built in (except for local context).

If only we could have the best of both worlds in Haskell: pure functions and immutability and all of our nice guarantees, and the ability to simulate special computational contexts and data flows Haskell doesn't natively have without all of this book keeping and pattern matching. As a matter of fact, we can have the best of both worlds! We can use an abstraction called monads.

# Monads

In the same way abstracting common functionality into an abstract helper function or an abstract class in Java allows you to avoid repeating yourself and copy-paste-modify, monads let you abstract this machinery of pattern matches and book keeping for sequencing special computations. `Monad` is a type class (like an interface) that captures this notion of sequencing special computations. In fact, a lot of the things we work with are monads! `Maybe` has a `Monad` instance (that's how we say it "implements the interface"), for example. You could also say `Maybe` _is a_ monad, but keep in mind that what it really means is that it has an instance of the `Monad` type class. I'll say "is a Monad" from now on.

When I say `Maybe` is a monad, I don't mean `Maybe a`. Monad is a higher-order type class. Types like `Maybe`, which take in a type parameter are higher-order or parametric types, and only higher order types are monads. If you have a bunch of computations that may not return an answer and you want to do them one after another, you'd need a lot of pattern matching, just like we were thinking about with our type checking computations. That's annoying

`Either a` is also a monad. Recall that `Either a b` is either an error `a` or a value `b`. So `Either a` is "waiting for" another type just like how `Maybe` is "waiting for" one other type. So `Either a` is a monad. If you want to sequence a bunch of computations that may result in errors, you'd have to do a lot of pattern matching boilerplate, just like maybe.

Our computation that needs to keep track of mutable state is a monad too. It's called  `State`. Here is the definition of `State`:

```haskell
newtype State s a = State { runState :: s -> (s, a) }
```

`newtype` is just `data` for when there is just one constructor with one field. this `{runstate :: s -> (s, a) }` business is called record syntax, and it's just like a normal `data` definition, but it also defines a field accessor function `runState`.

And `State s` is a monad! This definition captures the notion of a computation which mutates state and returns a value of its own.

Taking in a context that transforms locally, rather globally like our `Context` is also a monad called `Reader`:

```haskell
newtype Reader r a = Reader { runReader :: r -> a }
```

You might think "isn't that just a fancy way of saying `r->a`?" And yes, it is. The reader monad just "automates" or abstracts passing the `r`, or in our case, `Context` around.

But what is in the `Monad` type class, how do I implement it, and how does it help me get rid of all this boilerplate?

First, let's look at the class:

```haskell
class Applicative m => Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
```

Ok, a lot going on here, let's unpack it.
First, remember that `m` is some higher-order type constructor like Maybe. So let's look at these signatures for `m = Maybe` so it's easier to think about:

```haskell
class Applicative Maybe => Monad Maybe where
    return :: a -> Maybe a
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
```

Ok, starting to make a little more sense, but still weird. First off, what does `Applicative Maybe =>` mean? `Applicative` is another type class. `Monad` is actually a subclass of `Applicative`, which is a subclass of `Functor`. So all `Monad`s are `Applicative`s, and all `Applicative`s are `Functor`s. We'll go over all that in a minute.

What does the syntax `(>>=) :: ...` mean? In Haskell, you can define infix operators. That is the syntax for specifying the signature of an infix operator. You'd use it like this: `Just 2 >>= (\n -> Just (n + 1))`.

Now let's figure out what these functions do (no pun intended).

`return` just takes an `a` and outputs a `Maybe a`. For `Maybe`, `return a = Just a`. `return` in general just takes in a value and produces the "simplest" computation which will "result in" that value. For `Either`, it just calls `Right`. For `State`, it just returns the value and doesn't mutate the state.

`>>=` is tricky. It takes in a `Maybe a`, a function from `a -> Maybe b`, and outputs a value of type `Maybe b`. In a sense, `>>=` is like a semicolon. You have the result of a `Maybe` computation (a `Maybe a`) on the left, and how you'd like to use its value (if there is one) on the right. For example, let's say I want to do a `Map.lookup` and then use its result, if it exists. First, the old way:

```haskell
doubleOfValue :: String -> Map String Integer -> Maybe Double
doubleOfValue k m =
    case Map.lookup k m of
        Nothing -> Nothing -- can't do anything
        Just n -> Just (fromIntegral n) -- we have a value, cast it to double
```

Now, with `>>=` and `return`:

```haskell
doubleOfValue :: String -> Map String Integer -> Maybe Double
doubleOfValue k m =
    Map.lookup k m >>= (\n -> return (fromIntegral n))
```

So we perform some computation `Map.lookup k m` that results in a `Maybe Integer`. Then, we want to cast the result to a double if there is one. Otherwise, we can't really do anything, so we just propagate the `Nothing`. This behavior of "if Nothing, output Nothing. Otherwise do this with the value in the Just" is what defines the `Maybe` monad and how the sequencing of `Maybe` computations works. That's the common pattern. In this case, our usage of `>>=` was `Maybe Integer -> (Integer -> Maybe Double) -> Maybe Double`.

Now that we know what we want `>>=` to do, Let's write out the definition of `>>=` for `Maybe`:
```haskell
Nothing >>= f = Nothing
Just a >>= f = f a
```

And there we have it. We have successfully abstracted the sequencing of `Maybe` computations. If the initial computation results in `Nothing`, we propagate the `Nothing`. Otherwise, we call the "monadic function" `f` on the result of that computation, which outputs some `Maybe`. That's the pattern abstracted.

You might wonder "why does `f` have to output a `Maybe`? We often want to do something to the result of a `Maybe` that doesn't output a maybe. Like casting an `Integer` to a `Double`." Well, any operation `a -> b` like that can be transformed into `a -> Maybe b` by `return`ing the `b` result. So expecting a `a -> Maybe b` is strictly more general than `a -> b`, since the monadic function might actually want to produce a `Nothing`. And as we'll see soon, `Functor` is all about the `a -> b` stuff instead.

You can sequence many computations with `>>=`

```haskell
safeDiv :: Integer -> Integer -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = fromIntegral a / fromIntegral b

twiceRatioOfValues :: String -> Map String (Integer,Integer) -> Maybe Double
twiceRatioOfValues k m =
    Map.lookup k m >>= (\(a,b) -> safeDiv a b >>= (\x -> return (2.0 * x)))
```

So we want to find the value of the key (if there is one), which is a pair of integers. Then, we want to find their ratio using `safeDiv`, but that won't give an answer if the denominator is 0. Then, we want to multiply the ratio by 2. That would have required deep pattern matching if we didn't use monad operations.

We can clean it up by removing some parentheses and changing the layout:

```haskell
safeDiv :: Integer -> Integer -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = fromIntegral a / fromIntegral b

twiceRatioOfValues :: String -> Map String (Integer,Integer) -> Maybe Double
twiceRatioOfValues k m =
    Map.lookup k m >>=
        \(a,b) -> safeDiv a b >>=
            \x -> return (2.0 * x)
```

It's almost like writing in an imperative language:

```
(a,b) = Map.lookup k m;
x = safeDiv a b;
return (2.0 * x);
```

Except in an imperative language, those things would throw exceptions if they failed. Here, we can choose how the "semicolon" works and we get an actual value at the end, with no crashing to worry about.

In fact, Monads are so special, they have their own syntax in haskell called `do` notation. Here is how we'd write that function using it:

```haskell
safeDiv :: Integer -> Integer -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv a b = fromIntegral a / fromIntegral b

twiceRatioOfValues :: String -> Map String (Integer,Integer) -> Maybe Double
twiceRatioOfValues k m = do
    (a,b) <- Map.lookup k m
    x <- safeDiv a b
    return (2.0 * x)
```

Beautiful, isn't it? All that nasty pattern matching abstracted away. We can just focus on what's important. And this single syntax works for all monads. If you implement an instance of the `Monad` class for your type, you can use `do` notation with it!

`do` notation just translates to `>>=`s and lambdas under the hood in a process called desugaring. You can see exactly how it works [here](https://en.wikibooks.org/wiki/Haskell/do_notation).

By the way, `>>=` is pronounced "bind" or "monadic bind". This is a good name because it "binds" the result of the computation to the argument of the lambda to the right of the `>>=`.

Now I don't want to spend too much time in the monad rabbit hole, so I'll briefly go over `Functor` and `Applicative`:

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```
Hey, our old friend `fmap`! I said before that you can use it to apply a function to every element of a list: `a -> b -> [a] -> [b]`. So list is a functor. In fact, it's an applicative and a monad too! In the same way `Maybe` is no result or a result, list is no result, one result, or many results. But the list monad can also be thought of as as managing collections, not just thinking about computational contexts. For example, list's monad instance can be used in [list comprehensions](https://wiki.haskell.org/List_comprehension). So what a monad is, or what a monad does can be interpreted in many ways. The same abstraction that sequences computations can also be used to manage collections. And even more exotic things like IO and concurrency can be framed in terms of monads. In fact, Haskell handles IO via the IO monad.

So a functor is just something that can be mapped over.

```haskell
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

instance Functor State where
    fmap f m = State (\s -> let (s',a) = runState m s in (s', f a))
```

I include `State` since it's sort of a different "flavor" of monad. `Maybe` is like a pattern-matching-abstracting monad, and `State` is like a bookkeeping monad.

For `Maybe`, if just applies the function in the just case. `State` is a little weirder.
Remember `State`s have a field called `runState :: s -> (s, a)`. So `fmap` "runs" `m`, which may change the state. So we use the new state it outputs, `s'` in the result and call `f` on its result, `a`. So it just does the mutation of `m` and applies the function to the resulting value. Cool.

There is an infix operator, `<$>`, which is an alias for `fmap`:

```haskell
doubleOfValue :: String -> Map String Integer -> Maybe Double
doubleOfValue k m = fromIntegral <$> Map.lookup k m
```

It looks like we're just calling `fromIntegral`. This syntax is kind of like overloaded whitespace that does all the lifting for us.

```haskell
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b
```

Let's replace `f` with `Maybe`:

```haskell
class Functor Maybe => Applicative Maybe where
    pure :: a -> Maybe a
    (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
```

`pure` is just `return`.
If we look at `<*>` without `Maybe`, we get `(a -> b) -> a -> b`, which is just function application. So it's like we are applying a maybe of a function to a maybe of an argument, and getting a maybe of an answer. This is why it's called `Applicative`. Because it describes how a contextualized function can be applied to a contextualized argument to get a contextualized answer. And the instance implementation defines what that context is.

Let's look at the signature of `<*>` with parentheses:

```haskell
(<*>) :: Maybe (a -> b) -> (Maybe a -> Maybe b)
```

So it takes a `Maybe` of a function and outputs a function of maybes. That's another way to think about applicative. Functor was `(a -> b) -> (f a -> f b)`, and applicative is `f (a -> b) -> (f a -> f b)`. Since you can just use `pure` to convert a normal function to a `f (a -> b)` wrapped function, this is strictly more general than `fmap`.

```haskell
instance Applicative Maybe where
    pure a = Just a

    Just f  <*> Just a  = Just (f a)
    Nothing <*> ma      = Nothing
    mf      <*> Nothing = Nothing

instance Applicative State where
    pure a = State (\s -> (s,a))
    sf <*> sa = State (\s ->
        let (s',f) = runState sf s
            (s'', a) = runState sa s'
        in (s'', f a))
```

So maybe `pure` is `Just`, and you can only "apply" a `Maybe (a -> b)` to a `Maybe a` if they are both `Just`, and you output `Just (f a)`. For state, you extract the function `f` by running `sf`, then extract `a` from `sa`, and output `f a`, threading the state through the whole way, using the output state of `sf` as the input state to `sa` and so on. It is important to notice that `sf`'s mutations run first.

You can use applicatives like this:
```haskell
Just (+) <*> Just 1 <*> Just 2
```

or with `<$>`
```haskell
(+) <$> Just 1 <*> Just 2
```

Again, these infix operators are like overloaded whitespace that performs the necessary lifting to make everything work out.

This is a contrived example, but there are situations where it is very useful to be able to do things like this.

Now monad:
```haskell
instance Monad Maybe where
    return = pure
    Nothing >>= f = Nothing
    Just a >>= f = f a

instance Monad State where
    return = pure
    sa >>= f = State (\s -> let (s',a) = runState sa in runState (f a) s')
```

We've already seen maybe, let's look at state: We run `sa` to get `(s',a)`. Remember `f` has the signature `a -> State b`. So `f a` has type `State b`. So we run `f a` with `s'`. Note that `sa`'s mutations run first.

Let's examine the signature of `>>=` with arguments flipped, along with `fmap,<*>`. There is actually a name for flipped `>>=`, it's called `=<<`:
```haskell
(<$>) :: (a -> b)   -> (m a -> m b)
(<*>) :: m (a -> b) -> (m a -> m b)
(=<<) :: (a -> m b) -> (m a -> m b)
```

These are all useful for lifting types when performing computations that involve applying functions involving monads.

Each function is more general than the last. In fact, each in terms of the more general ones:

```haskell
fmap f ma = pure f <*> ma
fmap f ma = ma >>= \a -> return (f a)
mf <*> ma =
    mf >>=
        \f -> ma >>=
            \a -> return (f a)
```

Pretty cool.

Monads are __really__ hard to wrap your head around. I remember one day, I decided "I'm going to understand what a monad is" and I spend an entire week reading all these explanations and tutorials, hoping to develop some intuition. I thought about them non-stop, and eventually, it started to click. It didn't seem very useful at first, and it seemed weird to me that we were working in terms of these weird `a -> m b` functions when something like `m a -> m b` felt more natural, but it eventually clicked. I recommend implementing monad instances for some common types like `Either`, `List`, `Reader`, and maybe redoing the ones we did. What made me finally get it was implementing a monadic parsing library (that's right, parsers are monads!). Seeing them in action and figuring it out from the ground up and using it in my own way was really good. I think it was [this tutorial](http://dev.stephendiehl.com/fun/002_parsers.html) that I followed.

I call the journey of understanding monads "climbing monad mountain". It's weird and tough, but once you get to the summit, the view is great. After monads, there are monad transformers like in the `transformers` package, and monad transformer stacks like in the `mtl` library. I don't of any abstractions higher than that currently, and I work with monad stacks all the time. When I do something, I figure out what my monad stack should be based on the semantics of my operations, then get to work. I eventually learned category theory because of this classic category theory explanation:

> All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor.

This is by Saunders Mac Lane in _Categories for the Working Mathematician_. That sentence haunted me, so I learned category theory to demystify it and also since it's the foundation for Haskell's design. It was very interesting and gave me some good perspective and understanding of Haskell, but it is not necessary for your everyday Haskell. I think I learned it from [this blog](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).

Ok, enough rambling.

# Back to Type Inference

## Monads tl;dr

Monad is a type class, which is like an interface in Java. If you implement an instance of the Monad class for your data type, you can use `do` notation, which looks like you're writing in an imperative language, with the semantics of the "semicolon" customized according to your implementation of `Monad`.

## Defining our Monad

Monads let us abstract pattern matching boilerplate and book-keeping for sequencing computations. Can we get the power of monads to abstract our type inference computations? You bet we can. Recall the final signature of our `inferExpr` function:

```haskell
inferExpr :: Expr -> Context -> CheckerState -> Either TypeError (CheckerState, Mono)
```

That looks like a few of the monads we talked about. We have the `Context` going in like a `Reader`, we have the `CheckerState` getting threaded through the computations like a `State`, and we have the error behavior of `Either`. So We have this big composite monad. There is a way to easily compose monads and combine their behaviors, but we've already gone deep enough into the rabbit hole. If you want to learn more, research monad transformers and monad transformer stacks. If we were doing that, we'd use `RWST Context () CheckerState (ExceptT TypeError) a`. But we're going to do it ourselves.

First we have to define `CheckerState`. We will use an `Integer` to keep track of the next type variable to generate, and we'll just make `"t1","t2","t3",...`

```haskell
data CheckerState = (UnionFind Mono, Integer)

newtype Checker a = Checker { runChecker :: Context -> CheckerState -> Either TypeError (CheckerState, a) }

instance Functor Checker where
    fmap f ma = Checker (\ctx s ->
        case runChecker ma ctx s of
            Left err -> Left err
            Right (s',a) -> Right (s',f a))

instance Applicative Checker where
    pure a = Checker (\_ s -> Right (s,a))
    mf <*> ma = Checker (\ctx s ->
        case runChecker mf ctx s of
            Left err -> Left err
            Right (s',f) -> case runChecker ma ctx s' of
                Left err -> Left err
                Right (s'',a) -> Right (s'',f a))

instance Monad Checker where
    return = pure
    ma >>= k = Checker (\ctx s ->
        case runChecker ma ctx s of
            Left err -> Left err
            Right (s',a) -> runChecker (k a) ctx s')
```

These instances just combine the behavior of all the monads involved in checking. If you look at the instances for `Reader,State,Either` you'll see each one is a part of these instances.

## Monad Utility Functions

First, let's make some utility functions for working with our monad.

Here's how we throw an error:

```haskell
throwError :: TypeError -> Checker a
throwError err = Checker (\_ _ -> Left err)
```
To throw an error, we make a checker that ignores the state and context and outputs `Left err`. This outputs a `Checker a` regardless of what `a` is, since the value is never read.

getting the context:
```haskell
getContext :: Checker Context
getContext = Checker (\ctx s -> Right (s,ctx))
```
To get the context, we just return it as the value of the computation. Now, in do notation, we could write
```haskell
do
    ...
    ctx <- getContext
    ...
```
locally modify the context:
```haskell
local :: (Context -> Context) -> Checker a -> Checker a
local f ma = Checker (\ctx s -> runChecker ma (f ctx) s)
```
This runs the given checker under a modified context.

Adding variable annotations:
```haskell
withVarAnnot :: String -> Scheme -> Checker a -> Checker a
withVarAnnot x s = local (Map.insert x s)
```
This runs the given checker with the given annotation $(x:s)$ added to the context.

Adding many annotations:
```haskell
withVarAnnots :: [(String, Scheme)] -> Checker a -> Checker a
withVarAnnots pairs = local (Map.union (Map.fromList pairs))
```

Get the current state:
```haskell
get :: Checker CheckerState
get = Checker (\_ s -> Right (s,s))
```
The returned value is the state itself

Set the current state:
```haskell
put :: CheckerState -> Checker ()
put s = Checker (\_ _ -> Right (s,()))
```
Note that the output type is `Checker ()`. `()` is the empty tuple, and there is only one value of that type: `()`. When you see `m ()`, that means the computation is only executed for a side effect, since `()` contains no usable information. It's like `void` in Java.

Get the current union find:
```haskell
getUnionFind :: Checker (UnionFind Mono)
getUnionFind = do
  (uf,_) <- get
  return uf
```

Set the union find:
```haskell
setUnionFind :: UnionFind Mono -> Checker ()
setUnionFind uf = do
    (_,n) <- get
    put (uf,n)
```

That does it for general monad utilities. Now lets get to the auxiliary typing operations:

## Auxiliary Typing Operations

newvar:
```haskell
newvar :: Checker Mono
newvar = do
  (uf,n) <- get
  let t = TVar ("t"++show n)
  put (uf,n+1) -- ensure the next newvar is different
  return t
```

We get the current type index, create a type variable `"tn"` from it, and increment the type index globally. Then, we just return the type _after_ the mutation.

instantiate:
```haskell
instantiate :: Scheme -> Checker Mono
instantiate (SForall x s) = do
    x' <- newvar
    let s' = subScheme x x' s
    instantiate s'
instantiate (SMono t) = return t
```
We "peel off" the foralls one by one and replace the quantified variables with `newvar`s

generalize:
```haskell
generalize :: Mono -> Checker Scheme
generalize t = do
    ctx <- getContext
    let monoFrees = freeMonoVars t
        ctxFrees = freeCtxVars ctx
        frees = Set.difference monoFrees ctxFrees
    return (foldr SForall (SMono t) frees) -- forall all the free variables
```
We take the free variables of the mono type minus the free variables of the context, and quantify all of them with foralls. We don't have to worry about name capture since these variables are totally free, and they will be replaced with `newvar`s anyway upon instantiation.

unify:
```haskell
unify :: Mono -> Mono -> Checker ()
unify ta tb = do
    uf <- getUnionFind
    -- we want to use the "most solved" versions of these types
    let ta' = find uf ta
        tb' = find uf tb
    let unifyTVar x t
            | elem x (freeMonoVars t) = throwError (OccursError x t)
            | otherwise = setUnionFind (union uf t (TVar x))
            -- the order here is important. We want x's representative to be t, not the other way around
    case (ta',tb') of
        (TArr arg ret,TArr arg' ret') -> do
            unify arg arg'
            unify ret ret'
        (TNat, TNat) -> return () -- all good
        (TBool, TBool) -> return () -- all good
        (TVar x, t) -> unifyTVar x t
        (t, TVar x) -> unifyTVar x t
        (TArr _ _,_) -> throwError (Mismatch ta' tb')
        (TNat,_) -> throwError (Mismatch ta' tb')
        (TBool,_) -> throwError (Mismatch ta' tb')
```
We find both types. Then, we recur on child types if applicable, union in the case of a type variable and do an occurs check, or mismatch if the types have different constructors. In the `(TNat,TNat)` case, we don't need to do anything, so we just `return ()`, which does nothing.

## Type Inference!

This is what it's all been for. We're finally implementing the rules for type inference! Here they are:

```haskell
inferExpr :: Expr -> Checker Mono
```

$$
\frac
{(x:\sigma) \in \Gamma \qquad \tau=instantiate(\sigma)}
{\Gamma \vdash x : \tau}
Var
$$
```haskell
inferExpr (Var x) = do
    ctx <- getContext
    case Map.lookup x ctx of
        Nothing -> throwError (UnboundVar x)
        Just s -> instantiate s
```

If we don't find the variable in the context, it must not have been bound by any lambda or let expression. So we throw an `UnboundVar` error.

$$
\frac
{}
{\Gamma \vdash n : nat} Nat
$$
```haskell
inferExpr (Nat _) = return TNat
```
`Nat`s have type `TNat`. Crazy, right?

$$
\frac
{}
{\Gamma \vdash true : bool} True
$$
$$
\frac
{}
{\Gamma \vdash false : bool} False
$$

```haskell
inferExpr (Bool _) = return TBool
```
And `Bool`s have type `TBool`.

$$
\frac{\Gamma \vdash e_1 : \tau_1 \qquad\Gamma\vdash e_2 : \tau_2 \qquad \tau' = newvar \qquad unify(\tau_1,\tau_2 \rightarrow \tau')}
{\Gamma \vdash e_1\ e_2 : \tau'}App
$$
```haskell
inferExpr (App f arg) = do
    tF <- inferExpr f
    tArg <- inferExpr arg
    tRet <- newvar
    unify (TArr tArg tRet) tF
    return tRet
```
We switch the order of arguments in the `unify` because we want the expected type on the left and the actual type on the right for our errors. It's an arbitrary convention though.

This one very clearly shows how to translate the clauses of the typing rule into monadic computations, since we made functions for our auxiliary operations. Notice that we don't explicitly mention the context, union find, or next type index since it's all abstracted away by our monad and helper functions. It's like we made our own little imperative sub-language in Haskell! Monads are great.

$$
\frac
{\tau = newvar \qquad \Gamma,(x:\tau) \vdash e : \tau'}
{\Gamma \vdash \lambda x.e : \tau \rightarrow \tau'}
Abs
$$
```haskell
inferExpr (Lambda x body) = do
    tArg <- newvar
    tRet <- withVarAnnot x (SMono tArg) (inferExpr body)
    return (TArr tArg tRet)
```
We use `withVarAnnot` to locally insert $(x:\tau)$ into the context for inferring the body's type. You might be thinking "but won't `inferExpr body` evaluate before getting the annotation? How does that work?" Remember, `inferExpr` outputs a `Checker Type`, not a `Type`. A `Checker Type` is a computation, not a finished value. It is waiting for a context and state, and we are making a local change to the context before running the computation. Namely, we are inserting the variable and its type into the context. After inferring the body type, we return the function type.

$$
\frac
{\Gamma \vdash e_1 : \tau \qquad \sigma = \bar{\Gamma}(\tau) \qquad \Gamma,(x:\sigma) \vdash e_2 : \tau'}
{\Gamma \vdash \textrm{ let } x = e_1 \textrm{ in } e_2 : \tau'}
Let
$$
```haskell
inferExpr (Let x rhs body) = do
    tRhs <- inferExpr rhs
    sRhs <- generalize tRhs
    withVarAnnot x sRhs (inferExpr body)
```
Again, a pretty straightforward translation of the rule into code. We generalize the inferred mono type of the `rhs` to make it polymorphic, if possible, and put it in scope in the body when we infer its type. we don't need to bind that to a variable and return it, because the last computation in a `do` block is the "answer". You don't need to end in a `return ...`, you just need to end in a `Checker Mono`, which `withVarAnnot x sRhs (inferExpr body)` is.

$$
\frac
{\Gamma \vdash e_1 : \tau_1 \quad unify(\tau_1,bool) \quad \Gamma \vdash e_2 : \tau_2 \quad \Gamma \vdash e_3 : \tau_3 \quad unify(\tau_2,\tau_3)}
{\Gamma \vdash \textrm{ if } e_1 \textrm{ then } e_2 \textrm{ else } e_3 : \tau_2} If
$$
```haskell
inferExpr (If cnd thn els) = do
    tCnd <- inferExpr cnd
    unify TBool tCnd
    tThn <- inferExpr thn
    tEls <- inferExpr els
    unify tThn tEls
    return tThn
```
Again, we reverse the arguments of `unify` for our error convention.

We ensure that the condition `cnd` is a boolean, and that the two branches have the same type. Then, we return `tThn`. We could just as well have returned `tEls`, since they were just unified.

That's it! We have successfully written a type checker for our little language without the program mentioning types at all! Beautiful.

Just to wrap things up, let's write some functions for actually running this monad and cleaning up the types:

## Cleaning the Types and Running

### Solving Mono Types
```haskell
findMono :: Mono -> Checker Mono
findMono (TVar x) = do
    uf <- getUnionFind
    let x' = find uf (TVar x)
    if TVar x == x' then return (TVar x) else findMono x'
findMono TNat = return TNat
findMono TBool = return TBool
findMono (TArr arg ret) = do
    arg' <- findMono arg
    ret' <- findMono ret
    return (TArr arg' ret')
```
`findMono` uses the union find to recursively `find` away all the type variables to compute the type's "most solved" form. If we ever want to show the user a type (like if we did a repl or type-info-on-hover), they don't want to see a meaningless `t47` which has some solved form sprawled across the union find. So we solve the types with a bunch of `find`s. We recur until we hit a type variable. And the type variable's representative itself might be further solvable, so we recur on that too unless we hit a dead end where a type variable really is fully solved. We replace type variables with their representatives until we can't anymore.

### Simplifying Type Variables

```haskell
-- | compute the infinite list of words producible by the given alphabet in shortlex order.
shortlex :: [a] -> [[a]]
shortlex xs = [[x] | x <- xs] ++ [xs' ++ [x] | xs' <- shortlex xs, x <- xs]

-- | a,b,c,d,...,y,z,aa,ab,ac,...zy,zz,aaa,aab,...
names :: [[Char]]
names = shortlex ['a'..'z']

-- | Get the free variables of a mono type as they occur in left-to-right order
orderedFreeMonoVars :: Mono -> [String]
orderedFreeMonoVars (TVar x) = [x]
orderedFreeMonoVars TNat = []
orderedFreeMonoVars TBool = []
orderedFreeMonoVars (TArr arg ret) = nub (orderedFreeMonoVars arg ++ orderedFreeMonoVars ret)

-- | replace type variables such that the resulting type has variables appearing in alphabetical order
simplifyVars :: Mono -> Mono
simplifyVars t =
    let frees = orderedFreeMonoVars t
        subs = Map.fromList (zip frees (fmap TVar names))
        t' = subsMono subs t
    in t'
```
`simplifyVars` will take a type like `(t31 -> t44) -> (t20 -> t31) -> t20 -> t44`, which are gibberish, into `(a -> b) -> (c -> a) -> c -> b`, which we can now see is function composition if we think about it for a minute. That'll make the types look nice and pretty for the user. We just get the list of free variables in the order that they appear in the type, and substitute them with a list of all the letters in alphabetical order. The list of "letters" is infinite and goes into `aa,ab,ac,...,zy,zz,aaa,aab,aac,...` if necessary, but if you have a type big enough to need to go double letters, simplifying won't save you from the horror of understanding that type!

### Putting it All Together

```haskell
-- | solve the type and make its variables nice
finalizeMono :: Mono -> Checker Mono
finalizeMono t = do
    solved <- findMono t
    return (simplifyVars solved)
```

This function just solves and simplifies a mono type, doing all that's necessary for taking a raw, unsolved type spit out by `inferExpr` and making it presentable to a person.

### Running the Checker Monad

```haskell
-- | Run type inference on an expression under an empty context and state
runInferExpr :: Expr -> Either TypeError Mono
runInferExpr e = case runChecker (finalizeMono =<< inferExpr e) Map.empty (Map.empty,1) of
    Left err -> Left err
    Right (_,t) -> Right t
```
We create a helper function for running inference on an Expr with default state and context and ignoring the final state.

# Closing and Tips for Expansion

Now we're really done. Pat yourself on the back, because it's been a long journey. We learned what type inference is, how to talk about type systems formally, how to write a type system's rules, how to write rules conducive to an algorithm, and finally, how to implement these algorithmic rules in code.

I hope this shed some light on the black magic of type inference and gave some intuition for thinking about type systems in general. Knowing how it works will help you understand type errors better and appreciate the work that goes into writing a type checker with good error messages. Our error messages are pretty bare-bones and have no indication about where the error occurred or what caused them. But it's possible with some book-keeping. If you write a parser, you could keep track of source position information by adding a field to each constructor of expressions and types and such.

I think type inference is really cool, and I love the way the rules are so concisely implemented with our monad and helper functions. A nice translation from theory to code.

## Advice for Adding Language Features

This language isn't very useful, but it's good for illustrating the process of type inference. In a real language, you'll want top-level declarations, algebraic data types, pattern matching, and other nice features.

I may do full tutorials on this stuff in the future once I've successfully implemented them a few more times, but here is some quick advice:

Top-level declarations are pretty straightforward to add: You just treat them like let bindings, pretty much. 

`let rec` isn't too bad either. You just create `newvar`s for each binding, put the variables you're about to define in scope when checking the right-hand-sides so they can be used as they're being defined recursively, and unify the inferred type of each right-hand-side expression with its variable's `newvar` type variable. Then, you `findMono` and `generalize` the variables' `newvar` mono types, put the annotations in scope, and infer the body type. In fact, you might as well `findMono` inside `generalize` anyway. That'll save you some headaches if you add ADTs and pattern matching, since adding those features breaks some assumptions our algorithm made, which made `findMono` unnecessary internally.

With algebraic data types and pattern matching, there are a few gotchas. You need to be very careful when you generalize, and you need to `findMono` during inference sometimes. For pattern matching, you infer the target expression's type, and have a function `checkPattern :: Type -> Pattern -> Checker (Map String Type)` which returns the type annotations that should be in scope on the right-hand-side of the pattern match branch. You do this by creating type variables for child types according to the pattern, unifying with the type of a value this pattern should match with, and recurring, adding an annotation when you hit a variable pattern. Here is a snippet from an OCaml subset I implemented:

```haskell
inferMatchCase :: Type -> Pattern -> Expr -> Checker Type
inferMatchCase t p rhs = do
    vars <- checkPattern t p
    let varsPairs = Map.toList vars
    varsPairsGen <- mapM (\(x,t') -> (x,) <$> (SMono <$> findMono t')) varsPairs
    annots varsPairsGen (inferExpr rhs)

checkPattern :: Type -> Pattern -> Checker (Map String Type)
checkPattern t = \case
    PVar x -> return $ Map.singleton x t
    PTuple ps -> do
        ts <- fmap TVar <$> freshNames (length ps)
        let t' = TTuple ts
        unify t t'
        maps <- zipWithM checkPattern ts ps
        return $ Map.unions maps
    PCon con mP -> do
        tycon <- getTyConOfCon con
        (argnames, cases) <- getTyConInfo tycon
        argts <- fmap TVar <$> freshNames (length argnames)
        let t' = TCon tycon argts
        unify t t'
        let replacements = zip argnames argts
        case lookup con cases of
            Nothing -> throwError (UnboundVar con)
            Just tCon -> case (tCon, mP) of
                -- constructors either expect a tuple type or nothing
                -- maybe constructor arg type vs maybe arg pattern
                (Nothing,Nothing) -> return mempty
                (Just argT, Just p) -> checkPattern (subs subMono replacements argT) p
                (Nothing, Just{}) -> throwError (BadPConArity con 0 1)
                (Just{}, Nothing) -> throwError (BadPConArity con 1 0)
    PWild -> return mempty
```

The important cases are `PVar` and `PTuple` for seeing how it works. You create `newvar`s for each sub-pattern of the tuple pattern, make sure this the target type is consistent with such a tuple type, 
You'll have to look at the code ([here it is](https://github.com/quasarbright/OCovid/blob/master/src/OCovid/Static/Typing.hs)) to fully understand what's going on, but you might be able to get a general idea from skimming this. `checkPattern` gives you the typed variable bindings that should be in scope if the pattern match passes, and you call `findMono` on all of them before you put them in scope to infer the right-hand-side. Like a lambda's argument, we don't generalize, so bound variables have monomorphic types. It doesn't work if you don't fully solve the types before inferring the right-hand-side due to certain quirks of our auxiliary functions. When inferring a pattern match expression's type, you do this for each branch, and then you unify all the right-hand-side types and return one of them as the whole expression's type.

Here is the `Match` expression's case in `inferExpr`:

```haskell
Match e cases -> do
    tE <- inferExpr e
    ts <- mapM (uncurry (inferMatchCase tE)) cases
    zipWithM_ unify ts (tail ts)
    return $ head ts
```
We infer the target expression's type, call `inferMatchCase` on all the match cases, unify those types, and return the first one as the whole expression's type.

I know this code is unclear since you don't know my data definitions and auxiliary functions, but hopefully you can get some intuition from looking at it.

---

I hope this tutorial was useful. Thanks for reading!

Mike Delmonaco
