+++
title = "Re-Learning Haskell with Advent of Code - Part 1"
date = 2020-03-29T20:40:05Z
images = []
tags = []
categories = []
draft = false
+++

A few years ago I learned myself a Haskell for greater good from a book "Learn
you a Haskell for greater good"[^1] and the first few chapters of another book
"Haskell from first principles"[^2].

It was fun to learn.  I didn't go onto use Haskell to build any real
applications, beyond coding up some exercises and mini programs to learn
things.

I believe I got a lot from it.  At the very least it's joyful to solve problems
in Haskell.  When I started writing Rust a year later I liked the strong type
system and the features inspired by functional languages: maps, folds, etc.  It
gave me an appreciation of the benefits of immutability and pure functions.  I
believe that's fed into improving how I build things in other languages.

A colleague told me about how they did Advent of Code[^3] to learn Rust and a
wider group of colleagues have decided to do a couple of exercises every week,
to learn a variety of languages, and meet fortnightly to discuss approaches.  I
saw this as a good opportunity to "pick up Haskell again".  So I did.

_This post, along with following parts, will be my log of my experiences and
the things I'm learning while going through this exercise. It'll be littered
with wild theories, opinions, inefficient uses of code, hopefully some good
learnings, and sometimes things that are just plain wrong.  I'm keen to hear
any thoughts people have or anything they can add or ask in comments.  So don't
be afraid to drop a comment or three!_

# Week 1

## Build Tooling

First thing's first.  Build tooling.  I didn't really bother with this the last
time around.  I spent most of the time playing in GHCi, the interactive Haskell
terminal, and importing most of the Haskell modules I wrote into that, rather
than compiling and running an executable.

Over the last few years I've learned: first thing to do after cloning a
codebase: build it! And if you can't rebuilt at the touch of a button, do the
work immediately to make sure you can.  I've spent time faffing on with linking
issues with C++ applications.  I've spent time faffing on with Python's
dependency management, or lack thereof[^4].  It's not time well spent.  It's
time lost.  I've been writing Rust at work for about a year and a half.  Rust
has [cargo][cargo].  Cargo is phenomenal.  It's a great bit of tooling.  I'll
not lie to you: when facing technical decisions, especially around tooling, I
quite often find myself asking "WWCD?", "What Would Cargo Do?".

I'm picking up a language, having build tooling that's rock solid, and
available at the touch of a button is paramount.  So before writing a line of
code, or even reading the first day's challenge, I went looking for build
tooling.

I did a bit of googling with phrases like "Haskell toolchain", "Haskell build
tools comparison", and found various resources ranging from blog posts to
official documentation.  I found [this post][opinion-guide-haskell] quite
informative.  It purports to be "An opinionated guide".  That's good.
Developers having opinions is good.  Especially when you consider the
alternative: _not having thoughts_.  It's a long document.  I didn't read
nearly half of it.  But funnily enough the first section in this "guide to
Haskell", was on build tooling.  It touched on three options of cabal-install,
stack, and nix, and proceeded to do fairly good job of selling stack.  Combined
with a friend telling me that "stack is the closest thing to cargo that Haskell
has got", I was sold on stack, at least for the meantime.  Nix is on my bucket
list, just not today.

So I found and bookmarked the [official documentation][stack-docs] and ran
`stack new day1` to create a project for the problems in "day 1" of the Advent
of Code.

## Retracing the first steps

The problems on [day 1][day-1] involved summing the result of applying a
function to list of integers, and then do the same with a different function.
The halves of this exercise are almost identical except for the function to
apply to each element of the list before summing the results.  This sounds like
an opportunity for function currying.

Using `stack new` sets up a `src/Lib.hs` and a `app/Main.hs`.  While not having
read up on the conventional use of these files, my guess is that `Lib.hs` is
for any functionality that might become a library, keeping things as generic as
possible, and `Main.hs` is for code specific to this application: where to read
the data from, error handling, etc.  Although, especially for earlier problems,
this separation will be somewhat artificial.  I'm not going to get too worried
about what's in what file, but use it to get used to exporting and importing.

I ended up with a `Lib.hs`:

```haskell
module Lib
    ( tot_1
    , tot_2
    ) where

f :: Integral a => a -> a
f x = x `div` 3 - 2

tot_1 :: Integral a => [a] -> a
tot_1 = tot f

g :: Integral a => a -> a
g x
  | f x >= 0 = f x + g(f x)
  | otherwise = 0

tot_2 :: Integral a => [a] -> a
tot_2 = tot g

tot :: Integral a => (a -> a) -> [a] -> a
tot f = sum . fmap f
```

, and a `Main.hs`:

```haskell
module Main where

import Lib
import Data.List

main :: IO ()
main = do
  contents <- readFile "inputs.txt"
  let inputs = fmap read . lines $ contents
  print . Lib.tot_1 $ inputs
  print . Lib.tot_2 $ inputs
```

, having saved the inputs Advent of Code gave me to `inputs.txt` (each person
gets given their own problem input).

The function `tot` takes a function and applies it to each element of a list
and sums the results.  To fulfil each half of the problem I then only needed to
define the two functions to pass to `tot`.

This [day 1 problem][day-1] gave a quick revision of:
- modules,
- do notation,
- functions composition (with `.` & `$`),
- function currying, and
- recursion.

Recursion ends up featuring heavily in my solutions to the first few days. I'm
not sure why, but Haskell make recursion feel like a natural way to solve
problems.  I use recursion in other languages, but it always feels like I've
done something a bit clever.

[Day 2][day-2] asks you to write a basic Intcode computer which is then
extended and used in all odd day problems from day 5 onwards.  I decided to
come back to this and went on to problems from days [3][day-3], [4][day-4],
[6][day-6], and [8][day-8].

[Day 3][day-3] involved drawing lines on a grid and working out where they
cross, my solution involved reminding myself of how to make use of:

- Defining my own types: type constructors, sum types, record syntax,
- Generating lists from infinite lists,
- `filter`,
- `foldl`, and
- Using lists as applicative functors,

without too much hassle.  The solution wasn't very efficient, but
it worked.

## Fun With Folds

[Day 4][day-4] introduced a simple problem of: work out the number of integers
between two numbers that:
* Have two adjacent digits that are the same, and
* Who's digits never decrease.

In my solution I used function currying again. A function took a
function that applies a rule between two characters as an argument, and folded
over the digits in the numbers with `foldl` to apply the rules.

```haskell
ruleX :: Char -> Char -> Bool
```

In two respects, `foldl` was an awkward fit:
1) I didn't care about the initial value,
1) The rules can be confirmed as being broken or obeyed without having to iterate
   through every element.

### No Initial Value

The first of these points is equivalent to saying that I don't care about applying
this fold to an empty list.  Using `foldl`, each time I applied a rule to the
string, I had to provide an initial character that wouldn't cause the rule to
fail in any case.  While I "got away with it" for this problem, I should find
an alternative pattern to use here to avoid a reasonably crass source of error.

Funnily enough, I had this exact same problem when doing some work in Rust in
the week, and found [`fold1`][fold1-rs] from the [itertools
crate][itertools-rs] with type:

```rust
fn fold1<F>(self, f: F) -> Option<Self::Item> where
    F: FnMut(Self::Item, Self::Item) -> Self::Item,
    Self: Sized,
```

If an empty list has passed in,you get a `None`[^5] back.

Given how much Rust has borrowed from Haskell I was confident this will exist
in Haskell too.

I found [`foldl1`][foldl1-prelude] in Prelude.

```haskell
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
```

It does a fold without needing an initial value but doesn't handle empty lists.

```
$ stack ghci
...
*Main Lib> :t foldl1
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a
*Main Lib> foldl1 (+) [1,2,3]
6
*Main Lib> foldl1 (+) []
*** Exception: Prelude.foldl1: empty list
*Main Lib> :q
$
```

I tried searching in [Hoogle][hoogle] for a type like the `fold1` from Rust:

```haskell
foldable t => (a -> a -> a) -> t a -> Maybe a
```

[`foldl1May`][foldl1may] looks to be what I want.  I'll keep the `safe` package
in mind when doing further problems.  Coming from Rust I don't have much of a
taste for these exceptions.

```
$ stack ghci
...
*Main Lib> :m + Safe.Foldable
*Main Lib Safe.Foldable> :t foldl1May
foldl1May :: Foldable t => (a -> a -> a) -> t a -> Maybe a
*Main Lib Safe.Foldable> foldl1May (+) []
Nothing
*Main Lib Safe.Foldable> foldl1May (+) [1,2,3]
Just 6
*Main Lib Safe.Foldable> :q
$
```

It's good to know the nomenclature of:

> "Safe" means: "won't throw an exception"

### Bailing Early from a Fold

The second of these points, where we know the rule is either met or broken
before we've finished iterating over the digits of the number, basically
means that we'd like to quit iterating once we have a solid answer.

In procedural code performing a "for loop", a "break" statement would do
this job.

The way I approached this was to wrap the accumulator in a `Maybe` and have
the fold function "pass through" when the accumulator is `Nothing`.

```haskell
foldFunction :: (a -> b -> Maybe a) -> Maybe a -> b -> Maybe a
foldFunction _ Nothing _ = Nothing
foldFunction f (Just acc) x = f acc x
```

So as soon as `f` produces a `Nothing`, the fold will produce `Nothing`.

I used `Maybe` because I didn't care about the data being accumulated, I only
cared whether the fold got to the end of the list.  If I cared about the data
in an "early exit" of the fold I could have used `Either`:

```haskell
foldFunction' :: (a -> c -> Either a b) -> Either a b -> c -> Either a b
foldFunction' _ Left x _ = Left x
foldFunction' f (Right acc) x = f acc x
```

I was hoping the compiler would optimise out passing through `Nothing` and bail
as soon as the accumulator becomes `Nothing`. Then this pattern could be used
to fold over an infinite list... sadly not.  Say we want to quit with `Nothing`
as soon as an element is found greater than `10`:

```
*Main Lib> :t foldFunction
foldFunction :: (a -> b -> Maybe a) -> Maybe a -> b -> Maybe a
*Main Lib> foldl (foldFunction (\_ x-> if x > 10 then Nothing else Just x)) (Just 0) [0..10]
Just 10
*Main Lib> foldl (foldFunction (\_ x-> if x > 10 then Nothing else Just x)) (Just 0) [0..11]
Nothing
*Main Lib> foldl (foldFunction (\_ x-> if x > 10 then Nothing else Just x)) (Just 0) [0..]
^CInterrupted.
*Main Lib>
```

UPDATE: I've been pointed at [`foldM`][foldm] from [`Control.Monad`][control-monad].
The above can be written as:

```
*Main Lib> :m + Control.Monad
*Main Lib Control.Monad> :t foldM
foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
*Main Lib Control.Monad> foldM (\_ x -> if x > 10 then Nothing else Just x) 0 [0..10]
Just 10
*Main Lib Control.Monad> foldM (\_ x -> if x > 10 then Nothing else Just x) 0 [0..11]
Nothing
*Main Lib Control.Monad> foldM (\_ x -> if x > 10 then Nothing else Just x) 0 [0..]
Nothing
*Main Lib Control.Monad>
```

Interestingly [`foldM`][foldm] has this "early exit", as you can see it returned
`Nothing` when ran on the infinite list `[0..]`.

In my case, I wasn't actually bothered about the accumulator value whether the
fold bailed early or not.  In that case I could have used [`foldM_`][foldm-underscore].

```
*Main Lib Control.Monad> foldM_ (\_ x -> if x > 10 then Nothing else Just x) 0 [0..10]
Just ()
*Main Lib Control.Monad> foldM_ (\_ x -> if x > 10 then Nothing else Just x) 0 [0..11]
Nothing
*Main Lib Control.Monad> foldM_ (\_ x -> if x > 10 then Nothing else Just x) 0 [0..]
Nothing
*Main Lib Control.Monad>
```

Looking at our `foldFunction` above, it's actually a reimplementation of `>>=`
from the [implementation of the Monad typeclass for Maybe][maybe-monad].

It's interesting that `foldM` was able to do this "early exit" but `foldl`
wasn't.  I'll have to look at the source code and see if I can work out why.

Having a fold that can bail is a powerful pattern when combined with infinite
lists.  I'll remember this one.

## Trees

[Day 6][day-6] was all about tree structures.

The problem is presented as: "you're given a bunch of data on which planetary bodies
orbit which", with an implication of there being an orbiter and orbitee.  In Physics
this would be the model for when one body is much more massive than the other.
We think of the Moon orbiting the Earth, when really they're a binary pair orbiting
a common centre of mass.  You're then asked to count the total number of direct and
indirect orbits (orbiting something via orbiting something that's orbiting it).
This problem was going to be about finding the data
structure that best describes the overall system and using it.

My first thought was "Tree or DAG[^6]?".  A generic Graph could be ruled out as:
C orbiting B, orbiting A, orbiting C, is nonsense. Try and draw it on a piece of
paper.  To decide "Tree or DAG" I though about if a "diamond shape" made sense.
I.e. D orbiting C & B, which are both orbiting A.  Trying to draw this on a piece
of paper showed that a "diamond shape" wouldn't work in general, as in the model,
a body can't orbit two other bodies. Tree it is!

Any number of bodies can be orbiting a body, so we want a tree where a node can
point to any number of subtrees.  [`Data.Tree`][data-tree] fits the bill.

The input data was a file listing all the orbits.  So for B & C orbiting A, and D
orbiting C the input data would look like:

```
A)B
A)C
C)D
```

`)` means "is orbited by" so:

> A is orbited by B.  A is orbited by C. C is orbited by D.

In the nomenclature of this problem, D is "indirectly orbiting" A.  A could be the Sun,
B could be Venus, C could be the Earth, D would then be the Moon.

From the giant input I was given, I didn't know whether there was going to be more
than one "root"/"orbital centre", so I'd need to use a [forest][forest], which in
`Data.Tree` is just a type alias for a list of trees.

```haskell
type Forest a = [Tree a]
```

In my solution I was able to put the input data into a forest with
[`unfoldForest`][unfold-forest].

```haskell
unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
```

`b` is the type of the "seed values".  A "seed value" is a value from which a node
can be generated.  To build the `Forest`: `unfoldForest` (and similarly `unfoldTree`),
take a function that takes a seed value and produces the node data and a list of
seed values for all subtrees from the node. `unfoldForest` then takes a list of seed
values for the roots of the trees in the forest, whereas `unfoldTree` take a single
seed value.

I'd split the input data into a list of tuples of `String`s where the first element
of the tuple was the orbitee and the second was the orbiter.

```haskell
parseOrbit :: String -> (String, String)
```

In this case, both types `a` and `b` in `unfoldForest` will find the concrete
type of `String`.  Both the seed value and the node data will just be the name
of the body given in the data.

First I needed to find what the initial seed values, the roots, were in the data.

```haskell
-- Which bodies do not orbit anything?
roots :: Eq a => [(a,a)] -> [a]
roots orbits = [x | (x, y) <- orbits, notElem x . fmap snd $ orbits]
```

Then, given the orbit data, I was able to describe the "unfold function".

```haskell
-- Given the orbit data, what orbits a given body?
forestBuilder :: Eq a => [(a,a)] -> a -> (a,[a])
forestBuilder orbits body = (body, [snd orbit | orbit <- orbits, body == fst orbit])
```

`forestBuilder orbits` would give our "unfold function".

```haskell
orbits :: [(String, String)]

unfoldFunction :: String -> (String, [String])
unfoldFunction = forestBuilder orbits
```

Stringing that together, we can load the input data into a forest:

```haskell
main = do
  contents <- fmap lines . readFile $ "input.txt"
  let orbits = fmap parseOrbit contents
  let myForest = unfoldForest (forestBuilder orbits) (roots orbits)
```

Now the data is loaded into a `Forest`, I want to walk through the forest and
count how many steps from the root each node is.  Summing all those will give
us the number of direct and indirect orbits.

I was hoping to, symmetrically, use [`foldTree`][fold-tree] to get the answer.

I couldn't see how to do it with `foldTree`.  I had `Tree String` where the
string data was wholly uninteresting when counting the number of orbits
as I wanted to sum the depths of each node. The type signature of
`foldTree` appears to want a function that uses the node values:

```haskell
foldTree :: (a -> [b] -> b) -> Tree a -> b
```

I ended up splitting the `Tree` into its "levels" with [`levels`][tree-levels]
and then folding over that.

```haskell
levels :: Tree a -> [[a]]

sumDepthsTree :: Integral b => Tree a -> b
sumDepthsTree = snd . foldl foldFunc (0, 0) . levels

foldFunc :: Integral a => (a,a) -> [b] -> (a,a)
foldFunc (depth, acc) x = (depth + 1, acc + depth * genericLength x)
```

I used a tuple accumulator to record the depth (i.e. the index within `levels
mytree` and the accumulating sum of the depths).

That was enough to give me the answer to the first part of [Day 6][day-6].  In
writing this I've just realised there's a second half to the problem that I
missed before which looks like it's asking you to calculate how many steps it
takes to get from one tree node to another.  I'll come back to this at some
point and add to my solution.

## No Editor Integration Yet

Interestingly, I didn't setup any editor integration for Haskell in this first
week.

While my applications are only being split between `src/Lib.hs` &
`app/Main.hs`, and I can search on [Hoogle][hoogle], I've not felt like I need
it.

The syntax is fairly minimal, so I haven't needed an editor to write code for
me, and it is readable as text.  NeoVim gives me Haskell syntax highlighting
out of the box and I'm yet to need any of the IDE level functionality.

I've blogged before about how [you shouldn't need an IDE to read
code][no-ide-read] as code should be "readable as text", and I'm pleased that
Haskell is living up to that.

I'll keep rolling with syntax highlighting and nothing else for now.  If I need
to get some better integration I'll look at installing a Haskell language
server[^7] and hooking NeoVim's [language server
client][language-server-neovim] to it.

## Packaging and Modules

The simplest way to import functions from a module is to do:

```haskell
import Lib
import Data.Sequence
```

This pulls in the functions exported by those modules into the current
namespace.

Importing like this makes code harder to "read as text" as when you see a
function, where it's come from is not explicit in code.  You'd have to look
through what each module you import exports to find the function you're using.

This can be fixed by importing only specific functions:

```haskell
import Lib (someFunc)
```

Now only the `someFunc` function will be imported from `Lib`.

Collisions between functions imported from different libraries and Prelude can
be resolved with qualified imports:

```haskell
import quailfied Lib as L
```

Then every function from `Lib` is callable as `L.someFunc`.

These can be combined to do qualified imports and be explicit about the
functions imported:

```haskell
import qualified Lib as L (someFunc)
```

, again resulting in `someFunc` being callable as `L.someFunc`.

In order to keep my code as explicit as possible, and thus as "readable as
code" as possible, I'm going to use this as a convention from now on.  I'll
also try to read some open source Haskell to see if the wider community has a
convention on this.

This may end up being laborious.  In Rust, importing is _explicit_ in this way,
but a large amount of functionality is held in methods implemented on
structures and in traits.  You only have to import the `struct` or `trait` to
get the methods. In Haskell, as everything is a function, I'll need to import
each function explicitly.

If I end up giving up on this as a convention I'll look into integrating a
Haskell language server[^7] into my editor to give me the "go to definition"
and all those goodies.

## Summary

Doing some of the first few [Advent of Code][aoc] problems was good for
revising and refining some of the basics.  I reckon I could churn through all
of the problems using the basic tools I know how to use: list manipulation,
recursion, functors, applicative functors, etc.  But, I want to go away and do
some wider reading before continuing so I can really level up my Haskell.

My general feelings about Haskell thus far, learning it for the second time:
* I'm enjoying the purity and clean syntax.
* I'm enjoying using the word "function" without feeling a little guilty.
* Compared to Rust, Haskell is more implicit.  It appears there's ways of being
  totally explicit about things and I'll look to do that.  Maybe with a bit
  more experience I'll relax a little bit.
* I haven't fully learned how the packaging works yet, I'll feel more
  comfortable when I have.  One thing I don't know is how you find out what
  "package" a module is found in.
* It seems to be the way to search for "I need a thing that does this" is to
  know what type signature you're looking for is, and type that into the search
  bar on [Hoogle][hoogle].
* There seems to be a lot of different ways to do the same thing, and I'm not
  sure yet where to find advice on what good approaches are.

I'll go do some reading, then come back to these problems.

[**Re-learning Haskell with Advent Of Code - Part 2**][part2]

[opinion-guide-haskell]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
[cargo]: https://doc.rust-lang.org/cargo/
[stack-docs]: https://docs.haskellstack.org/en/stable/README/
[aoc]: https://adventofcode.com/
[day-1]: https://adventofcode.com/2019/day/1
[day-2]: https://adventofcode.com/2019/day/2
[day-3]: https://adventofcode.com/2019/day/3
[day-4]: https://adventofcode.com/2019/day/4
[day-6]: https://adventofcode.com/2019/day/6
[day-8]: https://adventofcode.com/2019/day/8
[aoc-cal]: https://adventofcode.com/2019
[hoogle]: https://hoogle.haskell.org/
[poetry]: https://python-poetry.org/
[pip]: https://pypi.org/project/pip/
[pipenv]: https://pipenv-fork.readthedocs.io/en/latest/
[fold1-rs]: https://docs.rs/itertools/0.9.0/itertools/trait.Itertools.html#method.fold1
[itertools-rs]: https://docs.rs/itertools/0.9.0/itertools/
[foldl1-prelude]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:foldl1
[foldl1may]: https://hackage.haskell.org/package/safe-0.3.18/docs/Safe-Foldable.html#v:foldl1May
[data-tree]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html
[forest]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#t:Forest
[unfold-forest]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#v:unfoldForest
[fold-tree]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#g:3
[tree-levels]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Tree.html#v:levels
[no-ide-read]: https://tarquin-the-brave.github.io/blog/posts/ide-read-code/
[language-server-neovim]: https://github.com/autozimu/LanguageClient-neovim
[hls]: https://github.com/haskell/haskell-language-server
[maybe-monad]: http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#line-854
[foldm]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#v:foldM
[foldm-underscore]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html#v:foldM_
[control-monad]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Monad.html
[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/

[^1]: Learn you a Haskell for greater good. http://learnyouahaskell.com/chapters
[^2]: Haskell from first principles. https://haskellbook.com/
[^3]: Advent of code provided a coding challenge on each day of December.
[^4]: [Poetry][poetry] does quite a good job of Python dependency management.
      It's not totally watertight, but it's a huge improvement on [pip][pip] & [Pipenv][pipenv]
      to which my derision is directed.  And Poetry has a very pretty website.
[^5]: Rust's `Option<T>` (with variants `Some<T>` & `None`) enum is equivalent to Haskell's `Maybe T`
      with `Just T` and `Nothing`.
[^6]: DAG: Direct Acyclic Graph: https://en.wikipedia.org/wiki/Directed_acyclic_graph
[^7]: I've found [Haskell language server][hls], but as the README says, it's in "early stages".
