---
title: How to implement type inference Part 2.5 - Monads
---
* TOC
{:toc}

This is an extra sub-part for those unfamiliar with monads. This is not meant to stand alone as its own separate tutorial, since it assumes you have the context of what's going on in the type inference implementation up to this point.

# Monads

In the same way abstracting common functionality into an abstract helper function or an abstract class in Java allows you to avoid repeating yourself and copy-paste-modify, monads let you abstract this machinery of pattern matches and book keeping for sequencing special computations. `Monad` is a type class (like an interface) that captures this notion of sequencing special computations. In fact, a lot of the things we work with are monads! `Maybe` has a `Monad` instance (that's how we say it "implements the interface"), for example. You could also say `Maybe` _is a_ monad, but keep in mind that what it really means is that it has an instance of the `Monad` type class. I'll say "is a Monad" from now on.

When I say `Maybe` is a monad, I don't mean `Maybe a`. Monad is a higher-order type class. Types like `Maybe`, which take in a type parameter are higher-order or parametric types, and only higher order types are monads. If you have a bunch of computations that may not return an answer and you want to do them one after another, you'd need a lot of pattern matching, just like we were thinking about with our type checking computations. That's annoying.

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

# The Monad Type Class

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

## Usage Example

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

We have the classic pattern of doing something that results in a `Maybe`, and wanting to process the value, if it exists. This is what `>>=` is for.

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

Except in an imperative language, those things would throw exceptions if they failed. Here, we can choose how the "semicolon" works and we _always_ get an actual value at the end, with no crashing to worry about. The "crash" is represented by a value we can work with directly.

## `do` Notation

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

## Functor and Applicative

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

It looks like we're just calling `fromIntegral`. This syntax is kind of like overloaded whitespace that does all the lifting for us in function applications involving contextual computations.

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

For a real-world example, here's a snippet from my [Haskell C-compiler](https://github.com/quasarbright/HCC/blob/master/src/Parse.hs#L153). It is the parser for a `while` statement:

```haskell
pWhile :: Parser Statement
pWhile = pKeyword "while" *> (While <$> parens pExpr <*> pSingleOrBlock)
```

`Parser` is an Applicative, and `fa *> fb = const <$> fa <*> fb` just "runs" the first argument, discards the result, runs the second argument, and outputs its result.

`While` takes in an `Expr` and a `[Statement]` and makes a `Statment`. `parens pExpr` is a `Parser Expr`, and `pSingleOrBlock` is a `Parser [Statement]`. The expression `While <$> parens pExpr <*> pSingleOrBlock` is a `Parser Statement`. We want to apply the constructor `While` to these arguments, but their values are inside `Parser`s, so our operators `<$>, <*>` "grab them" and wrap the whole thing in `Parser`.



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

Each function is more general than the last. In fact, you can implement each in terms of the more general ones:

```haskell
fmap f ma = pure f <*> ma
fmap f ma = ma >>= \a -> return (f a)
mf <*> ma =
    mf >>=
        \f -> ma >>=
            \a -> return (f a)
```

Pretty cool.

## Laws

I should mention that there are more requirements for being considered a Functor, Applicative, or a Monad than just implementing the functions on the type class. Your implementations should follow some laws. These aren't strictly enforced, but breaking these laws may cause some strange behaviors and bugs, so you should make sure your implementations follow them. These laws prevent silly things like `Maybe`'s `return` being `Nothing`, and also some more subtle things involving sequencing of computations.

### Functor Laws

Identity:
```haskell
fmap id == id
```

where `id` is `id x = x` the identity function. In other words, `fmap`ing `id` over something shouldn't change it.

Composition:
```haskell
fmap (f . g) == fmap f . fmap g
```

Where `.` is `(f . g) x = f (g x)` function composition. In other words, `fmap`ing the composition of two functions is the same as applying the `fmap`ed second function, then the `fmap`ed first function.

All of these laws are documented in the same place as the type class. [Here](https://hackage.haskell.org/package/base-4.14.1.0/docs/Prelude.html#t:Functor) is the documentation for functor, where you can see the laws.

### Applicative Laws

Identity:
```haskell
pure id <*> v == v
```
Here, `v` is of type `f a`, where `f` is an `Applicative`. Remember, `fmap f v = pure f <*> v`. So this is the same as the Functor Identity law: `fmap id v == v`, or equivalently, `fmap id == id`. Actually, `fmap f v = pure f <*> v` assumes that the Applicative instance follows the Applicative laws, so it's really the other way around.

Composition:
```haskell
pure (.) <*> f <*> g <*> x == f <*> (g <*> x)
```

`<*>` is left-associative, so let's write that with parentheses:

```haskell
((pure (.) <*> f) <*> g) <*> x == f <*> (g <*> x)
```

Let's look at the types:
```haskell
f :: f a
g :: f (a -> b)
x :: f (b -> c)
f <*> (g <*> x) :: f c
```

Remember how I said that stuff like `<$>, <*>, =<<` were like overloaded whitespace that does the necessary lifting for function application? Let's remove the `pure` and the `<*>` and see what we get:

```haskell
((pure (.) <*> f) <*> g) <*> x == f <*> (g <*> x)
((     (.)     f)     g)     x == f     (g     x)
(((.) f) g) x == f (g x)
(.) f g x == f (g x)
```

That's just the definition for function composition! This law just ensures that lifted composition `pure (.)` works as we'd expect in the "elevated world" of Applicatives and `<*>`.

Homomorphism:
```haskell
pure f <*> pure x == pure (f x)
```

This ensures that `pure` works as we expect. It puts the value into the Applicative as simply as possible, without throwing it away.

Interchange:
```haskell
f <*> pure x == pure ($ x) <*> f
```

Where `$` is function application `f $ x = f x`.

When Applicatives have "effects", like state mutation or erroring out, the argument on the left of `<*>` "runs" first. But `pure x` shouldn't have any effects, so this just says that The order of arguments doesn't matter if one of them is `pure`.

If `f` is also a Monad, the following laws should hold:

```haskell
pure == return
mf <*> mx == mf >>= (f -> mx >>= (x -> return (f x)))
```

That is just saying that our implementation of the Applicative class' functions in terms of `return, >>=` are equivalent to the implementation you use when you define an instance. You could directly use this as the implementation and the law would definitely hold, but sometimes a direct implementation may be more efficient.

### Monad Laws

Left Identity:
```haskell
return x >>= f == f x
```

Remember `x :: a, f :: a -> m b`. This just says that binding a monadic function `f :: a -> m b` to a `return`ed argument is the same as directly applying it. This ensures that `return` has no effects.

Right Identity:
```haskell
m >>= return == m
```
Binding `return` should have no effect.

Associativity:
```haskell
mx >>= (\x -> k x >>= h) == (mx >>= k) >>= h
mx :: m a
k :: a -> m b
h :: b -> m c
```

We would like to say `mx >>= (k >>= h) == (mx >>= k) >>= h`, but the types wouldn't make sense, so wee need that lambda. But this just says that sequencing is associative, which makes sense.

The monad laws can be stated in terms of monadic composition `<=<`.

```haskell
k :: b -> m c
h :: a -> m b
m :: m a
(<=<) :: (b -> m c) -> (a -> m b) -> a -> m c
(<=<) k h m = k =<< (h =<< m)
(.)   f g x = f     (g     x)
```

```haskell
return <=< k == k
k <=< return == k
(k <=< h) <=< l == k <=< (h <=< l)
```

These laws are equivalent to the above Monad laws. They are actually the laws for a Category in Category Theory, but I'm not going down that rabbit hole. All I'll say is that the Monad laws are the Category laws in disguise.

# Final Thoughts

Monads are __really__ hard to wrap your head around, and it'll take a while to understand them. I remember one day, I decided "I'm going to understand what a monad is" and I spend an entire week reading all these explanations and tutorials, hoping to develop some intuition. I thought about them non-stop, and eventually, it started to click. It didn't seem very useful at first, and it seemed weird to me that we were working in terms of these weird `a -> m b` functions when something like `m a -> m b` felt more natural, but it eventually clicked. I recommend implementing monad instances for some common types like `Either`, `List`, `Reader`, and maybe redoing the ones we did. What made me finally get it was implementing a monadic parsing library (that's right, parsers are monads!). Seeing them in action and figuring it out from the ground up and using it in my own way was really good. I think it was [this tutorial](http://dev.stephendiehl.com/fun/002_parsers.html) that I followed.

I call the journey of understanding monads "climbing monad mountain". It's weird and tough, but once you get to the summit, the view is great. After monads, there are monad transformers like in the `transformers` package, and monad transformer stacks like in the `mtl` library. I don't know of any abstractions higher than that currently, and I work with monad stacks all the time. When I do something, I figure out what my monad stack should be based on the semantics of my operations, then get to work. I eventually learned category theory because of this classic category theory explanation of a monad:

> All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor.

This is by Saunders Mac Lane in _Categories for the Working Mathematician_. That sentence haunted me, so I learned category theory to demystify it and also since it's the foundation for Haskell's design. It was very interesting and gave me some good perspective and understanding of Haskell, but it is not necessary for your everyday Haskell. I think I learned it from [this blog](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/).

Ok, enough rambling. I hope this tutorial was helpful in understanding monads in the context of the type checker.
