+++
title = "Re-Learning Haskell with Advent of Code - Part 3"
date = 2020-04-01T16:35:26+01:00
images = []
tags = []
categories = []
draft = true
+++

At the end of [Part 2][part2] I set out my next goals as:
- gain a more complete understanding of monad transformers and property based
  testing,
- cycle back around and improve the performance of my solutions, then
- carry on with a more complete toolkit at my disposal.

So I started search around to see what resources I could find.

[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/

# (Re-)[^re]Learning Haskell with(out) Advent of Code

After searching around a little, I stumbled upon [the Haskell learning
resources shared by FP Complete][fpch]. There's so many goodies here!

[fpch]: https://tech.fpcomplete.com/haskell/learn

Ran through tutorials on:
- Applicative Syntax
- String Types
- Strictness
- Monad Transformers - todo exercises
- Safe Exception handling - todo
- Vectors - to finish

loads more there.

Let's see how I can improve the AoC solutions so far.

# Refactoring AoC Solutions

## Small Improvements

Some small improvements I picked up as handy tips or on a 2nd look saw as
obvious and trivial.

### Writing Stack Scripts

For the simpler problems, I didn't need the overhead of generating a full
project with `stack new` and opted to replace that with a single `dayX.hs`
script which starts like:

```haskell
#!/usr/bin/env stack
-- stack --resovler lts-15.4 script

```

I'd gone with the full project structure given by the default template used by
`stack new` to get used to importing and exporting and to find the namespacing
control I was happy with.  I've done that now, so I can reduce some of the
problems to simple scripts and "get things done!".

### Using `where` More

As I elucidated on in [Part 2][part2], my earlier solutions were plagued with
the proliferation of small functions that don't really mean a huge amount on
their own and are only called as part of another function.  `where` helps to
clean this up by putting them inside the namespace of the function they're
really part of and de-cluttering the namespace of the module.

### Use `foldl'` Over `foldl`

"For strict application of the operator" - [the docs say][folddoc].

[folddoc]:
https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Foldable.html#v:foldl-39-

### Where a List Was Very Slow

[Day 3][day3], which was the first real problem I tackled in this exercise to
re-learn Haskell, has you drawing wires on a grid and finding where they cross.

I was using a long `List` for each "wire" to store the coordinates it passed
through, then finding where two wires had crossed by putting a function that
matches the coordinates in an applicative functor across the two lists.  This
ends up scaling like n<sup>2</sup> as the wires get longer.

Essentially `List` was a terrible choice of data structure for this.  Lists
provide an ordering of their data, and allow you to express duplicates.  We
want the answer to the question: "what points has a path visited?", we don't
care about the answers to "what order did the path visit those points in?", and
"were any points visited more than once?".  But by choosing `List` we pay for
the answers to those questions.

A simple refactor to store this data in [`HashSet`s][hashset], and finding the
[intersection][intersec] to get the points where the wires crossed reduced the
runtime of my solution from over 8 minutes, to 0.8 seconds.

[day3]: https://adventofcode.com/2019/day/3
[hashset]: https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/Data-HashSet.html#t:HashSet
[intersec]: https://hackage.haskell.org/package/unordered-containers-0.2.10.0/docs/Data-HashSet.html#v:intersection

### TODO

* deriving instances

## Big Changes

### Stop Matching Maybes - More Monad Transformers

Use monad transformers instead.

### Alternatively, Use MonadFail

In [Part 2][part2] I discussed a pattern I found of defining functions who's
result could represent a failure, as being generic over [`MonadFail`][mfail]
and allowing the calling code to choose the type, that is an instance of
[`MonadFail`][mfail], to represent the failure.

My solution to [day 6][day6] involved loading some data into a Tree, and at a
one point, finding the path to a given node. Trouble was, the code assumed the
given node existed in the tree, and if it didn't, the code didn't crash, it
gave a nonsensical answer - which is often worse.

Using this pattern, this code was surprisingly easy to fix.  My function
`orbitalHops` which told you how many hops there were between two nodes of a
tree was fixed up to look like:

```haskell
import qualified Data.Tree as T

orbitalHops :: MonadFail m => T.Tree String -> String -> String -> m Int
orbitalHops orbs node1 node2 = do
  _ <- orbs `contains` node1
  _ <- orbs `contains` node2
  return $ // previous logic that assumed nodes were in tree
  where
    contains :: MonadFail m => T.Tree String -> String -> m ()
    contains tree node = if node `elem` T.flatten tree then return () else fail $ "Could not find element: " ++ node
```

In my example of using this pattern with `MonadFail` in [Part 2][part2], there
was a top level function who's type gave `m` a concrete type.  This time, I
used the fact that `IO` implements `MonadFail` and passed the non-concrete `m`
up to `main` to let the script fail with the error above if a node given was
not in the data.

In `main` I had:

```haskell
orbitalHops orbitTree "YOU" "SAN" >>= print
```

Causing the script to either print the number of orbital hops between `"YOU"` &
`"SAN"` or fail with the error above, provided by the call to `fail`.

[day6]: https://adventofcode.com/2019/day/6

### No More Strings

The wisdom appears to be:

> Don't use `String`, use `Text` (or `ByteString` for raw data)

which a quick Google will provide pretty solid justification for, so I'll not
repeat it here.

[Day 6][day6] was also the first problem where the input data remained as a
string throughout the program and some string manipulation was performed.

I was bracing myself for a bit of a fight to update the solution from using
`String` to using `Text`, but actually found I could do an almost like for like
replacement.

I made some new imports:

```haskell
import qualified Data.Text.IO as TIO
import qualified Data.Text as Txt  // I was already importing Data.Tree as T, so went with Txt
import Data.Monoid ((<>))
```

and removed another: `import Data.List.Split (splitOn)`, then:

- Swapped `String` for `Txt.Text` where it appeared in type statements,
- Prepended `Txt.` to all the functions acting on strings, and
- Replaced usages of `++` with `<>`.

The compiler picked up a couple of places I'd missed and voilà, the script ran
as it did before.

I was able to completely remove `String` from the program, with one exception:
the [`fail`][fail] function from [`MonadFail`][mfail] takes a `String`.  This
required a call to [`unpack`][unpacktxt] to turn `Text` onto `String`.  So:

```haskell
fail $ "Could not find element: " ++ node
```

became:

```haskell
fail . Txt.unpack $ "Could not find element: " <> node
```

[fail]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:fail
[mfail]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:MonadFail
[unpack]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html#v:unpack

TODO: Does the rest of String -> Text conversion go this smoothly?

### Strictness

Day 1 solution before adding bang patterns:

### Lenses

### Vectors over lists

### Mutable Vectors to Model Intcode

### Making Types Generic

### Making Types Specific

### No Prelude

### RIO - maybe going forward...

all these improvements can be seen in this Monster PR...

# general observations

## Safety

## Not 1 way to do things

This is good.  I wonder how far Rust will get... TBF this is
reflected in Rust too. Just different things.


[^re]: By this stage, and really by the time I got to [Part 2][part2], I'm no longer "Re-learning"
       Haskell as I've gone far beyondthe level I got to when I learned some Haskell a few years
       ago.  I started this blog series with "re-learning", so for continuity's sake I'll keep
       the title as it is.
