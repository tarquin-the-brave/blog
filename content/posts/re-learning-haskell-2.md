+++
title = "Re-Learning Haskell with Advent of Code - Part 2"
date = 2020-04-01T16:36:28+01:00
images = []
tags = []
categories = []
draft = true
+++

_In [Part 1][part1], I skipped the day 2, 5, & 7 problems that get you
to build and use a Intcode computer.  Each problem provides an Intcode
program to run: A series of integers that can each either represent an
instruction or data, and can mutate itself.  Looking forward, every
odd number day from day 5 onwards uses the Intcode program.  So I
decided to come back to them, and make a concerted effort at a few of
them in a row._

# Intcode computer - Stateful Computation

# the problems

state monad for when we care about the evolution of state.

intcode program didn't end up wanting a state monad because it didn't care
about it's evolution really.  It churn some numbers then stops when it fails
or exits of needs more input.

intcode module describes states and transformation between them, but doesn't
need to "track state".

# Other Things That Came Up

## Recursion

In [my last post][part1] I said:

> I’m not sure why, but Haskell make recursion feel like a natural way to
  solve problems. I use recursion in other languages, but it always feels
  like I’ve done something a bit clever.

I think I know why now:

> Functional purity allows you to recurse with confidence.

It comes down to: how much information you have to hold in your head while
following the code.

In procedural languages, where methods have side effects, or at least are
not guaranteed not to, you need to keep in mind all the state that
can be affect when following a method.  When a method recurses,
you then have to consider all the state that the method can affect on
that recursion. You can't treat each recursion symmetrically, because each
recursion adds to the number of things you need to consider.

With functions[^functions], you only need to consider their inputs.
And importantly you only need to consider their inputs as data, not as
"either representing data or a place to put data"[^plop].  When a function
recurses, you can leave all context that wasn't passed directly to the
function behind when reasoning through the recursion. You can
descend into the recursion carrying with you only what you
carried into the last one.

Also as functions return the same output given the same input, you
generally only need to reason through the recursion once.

By this same reasoning, I'd argue that a fold is simpler[^simple] than
a for loop that iterates over a collection (for languages that support
both and you have the choice).

When reasoning through a fold you only need to consider two values:
the accumulator; and the item taken from the collection.  It's almost
like being in a bubble, where only these two values matter.

In a for loop however, you're exposed to the context outside the loop.
You can't forget about any variables in scope, as you might change them
while looping.  You also have control flow to consider.  You might hit
a `break` or `continue` on a particular loop which means you then have
to skip over the code blow it that you would have otherwise reasoned
through (a `goto` in sheep's clothing).  Like with recursion in procedural
languages the number of things you need to consider grows as you iterate.
With functional purity that number of things to consider stays steady.

## Compiler Warnings

I turned on a bunch of compiler warnings, as per the advise of [this
article][opinionatedguide], by adding this to my `~/.stack/config.yaml':

```yaml
ghc-options:
  "$locals": -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints
```

I had initially thought that I wanted to use Haskell as "out of the box"
as possible, and not "depend on" extra lints to tell me what to do, but
then remembered that's a terrible argument.

In reality, when people employ extra linting and static analysis they
don't become dependent on it, they learn from it and get better at
writing better code first time, and continue to write better code when
the linting is turned off.

When you work in a language with a strong type system and have a compiler
that kicks your ass when you get it wrong, you become better at writing
correct code first time in all languages.

So I turned the compiler warnings on.  It did make ghci a bit spammy.
I'll see if there's a nice way to turn the warnings off for ghci.

## Failing Functions being Generic over MonadFail

There was a few time in some of the problems where I had a type
that could represent failure, and if an operation fails we want
to return that type representing failure.

As an example, say we want to return a list where an empty list
represents failure.  In this simple example a negative input
results in failure.

```haskell
maybePos :: Int -> Maybe Int
maybePos x
  | x < 0 = Nothing
  | otherwise = Just x

thing :: Int -> [Int]
thing x = case maybePos x of
  Nothing -> []
  Just posX -> [posX]
```

`thing` has to match on the `Maybe Int` given by `maybePos` and
return either the `Int` in a list or an empty list.

I found myself repeating this patten:
* Model the operation that can fail with a function that returns `Maybe`,
* Match on the `Nothing` and return my failure type.

I was doing a lot of manually converting one type's failure representation
to another type's failure representation.  This seemed to me like there was
something I could use to do this automatically, as I was expressing the same
thing over and over again.

In the example above, both `Maybe` and `List` are monads.  I'm converting
from one monad's failure representation to another.  After a bit of looking
around I found the [`MonadFail` typeclass][monadfail].  As `Maybe` and `List`
both are instances of `MonadFail` we can redefine `maybePos` to be generic
over `MonadFail` and cut out the conversion between failure representations.

```
maybePosM ::  MonadFail m => Int -> m Int
maybePosM x
  | x < 0 = fail "woops"
  | otherwise = return x

thing' :: Int -> [Int]
thing' = maybePosM

posIntoMaybe :: Int -> Maybe Int
posIntoMaybe = maybePosM

posIntoIO :: Int -> IO Int
posIntoIO = maybePosM
```

## Breaking Functions Into Lots of Pieces

I sometimes fall into the trap of:
* thinking procedurally
* having a complex thing to "do"
* splitting that up into lots of "functions"
* non of which make any real sense on their own.
* code is an unreadable mess.
* happens when I'm rushing/world outside my head put demands on me (girlfriend)
`where` keyword is actually fixing this as it breaks the calculation down
without exposing all the sub functions to the rest of the program.

## Packages, Modules & Namespacing

In [my last post][part1] I talked about: not knowing how to find out
what package to install to get a certain library; and not being totally
comfortable with the namespacing when modules are imported and sufferring
from "where did that function come from?" syndrome.

The first of these was me just being dumb as it turns out the
package containing a module is written in the top left hand corner
of the module documentation's web page.  Spot `mtl-` in [the
docs][statemonaddocs] for `Control.Monad.State.Lazy`.

On the second of these: I've got reasonably comfortable with either
doing a qualified import to preserve namespacing, or being explicit
about which functions I'm importing if I'm importing them into the
module's namespace.

```haskell
import qualified Foo.Bar as FB
import Baz
  ( foo
  , fooBar
  , fooBarBaz
  )
```

I've found using a combination of these: qualified import and
explicitly listing the functions is a bit over the top.

I also practiced a pattern of having a module which renames
functions from another module when the names of functions and
types didn't make as much sense in the context I was using them,
or were just down right silly names in the first place.

```haskell
module Sensible
  ( foo
  , bar
  , Baz (..)
  , Silly.okName
  )

import Very.Silly.Named.Module as Silly

foo = Silly.longFooName
bar = Silly.otherBarName

type Baz = Silly.BazBaz
```

Using a combination of qualified imports and explicitly naming
functions I'm importing into the module's namespace, I've found
I can completely prevent the "where did that function come from"
syndrome.

In [my last post][part1] I made a lot of comparisons between how Rust
does things and how I was finding things are done in Haskell.
I made a general point that Rust is more explicit.  This is an example
of where Haskell can be more explicit.  In Rust, a lot of functionality
is provided via traits.  If a type implements a trait, you can call
the methods from that trait on that type by importing the trait.  The
methods themselves don't need to be explicitly imported.  The result is
that you can read some code that deals with a type you know (or at least
know where to find the docs for), that then calls a method on that type
that doesn't appear in the type's documentation.  You're left wondering
"where did that method come from?".  This is the part where I'd use
a "go to definition".  Without that you'd need to search through the
traits that are imported in scope as any one of them might have been
implemented for the type and try to find the method.  I try to avoid
this problem in Rust by keeping imports of traits as tightly scoped
with the code that uses them as I can so it's as obvious as possible
where the method came from.  This is probably one of the rare examples
where Rust is not [readable as text][idepost], where for the most part
it is pretty good at being.

# TODO

- read more about Monad Transformers
- As I said last time - "I could probably use the tools I've got and grind away to solve all
  the problems" - Learning more Haskell, along with data structures, mathematics, and patterns
  that can help in all languages is the goal.
- Loop back through on

[valueofvalues]: https://www.youtube.com/watch?v=-6BsiVyC1kM
[simplemadeeasy]: https://www.youtube.com/watch?v=oytL881p-nQ
[opinionatedguide]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
[part1]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell/
[idepost]: https://tarquin-the-brave.github.io/blog/posts/ide-read-code/
[statemonaddocs]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html


[^functions]: I'm careful to not call something a function if it isn't a pure function.
              The ship sailed a bit on that one as many languages call their inpure methods
              "functions", but I find the distinction useful, not least to help context
              switch when moving between procedural and functional frames of mind.

[^plop]: [A great talk by Rich Hickey][valueofvalues] "The Value of Values" talks
         about PLOP "Place Oriented Programming".

[^simple]: [Another great talk by Rich Hickey][simplemadeeasy] "Simple Made Easy"
           talks about what simplicity is and how we can think objectively about it
           rather than it being a stand in for what people are most familiar with.
