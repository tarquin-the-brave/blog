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

For me, the biggest difference coming back to my solutions was more down to
having more experience and confidence in the language, and coming at the code,
with a view to cleaning up the expressions and abstractions rather than wanting
to get the answer to plug into Advent of Code so it'll give me a gold star.
:star:

Looking past the general code tidying, there were some specific things I'd
learned from doing some of [FP Complete's tutorials][fpch] that I was able to apply to
my refactored solutions.

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
import qualified Data.Text as Txt  // I was already importing Data.Tree as T, so went with Txt
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B
import Data.Monoid ((<>))
```

and removed another: `import Data.List.Split (splitOn)`, then:

- Swapped `String` for `Txt.Text` where it appeared in type statements,
- Prepended `Txt.` to all the functions acting on strings,
- Swapped the use of Prelude's `readFile` with `fmap TE.decodeUtf8 . B.readFile`, and
- Replaced usages of `++` with `<>`.

The compiler picked up a couple of places I'd missed and voilà, the script ran
as it did before.

I was able to completely remove `String` from the program, with one exception:
the [`fail`][fail] function from [`MonadFail`][mfail] takes a `String`.  This
required a call to [`unpack`][unpacktxt] to turn `Text` into `String`.  So:

```haskell
fail $ "Could not find element: " ++ node
```

became:

```haskell
fail . Txt.unpack $ "Could not find element: " <> node
```

[fail]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:fail
[mfail]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:MonadFail
[unpacktxt]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html#v:unpack

TODO: Does the rest of String -> Text conversion go this smoothly?

- Day 8: Had to dig out `Text.map` as `Text` is not a member of `Functor` :cry: ...not really that bothered.

### Strictness

#### Text over String

One of the reasons touted for `String`, a singly linked lazy list of `Char`s,
being a bad representation of textual data is that it's lazy.

[Day 8][day8] was a good problem to experiment with as my solution involved
manipulating lists of characters, `[Char]`, a.k.a: `String`.

[day8]: https://adventofcode.com/2019/day/8

After refactoring the solution into a once page script and having a bit of a
tidy up, but otherwise leaving the substance of the code the same, I built
and ran the solution, in file `day8string.hs`, like so:

```
$ stack --resolver lts-15.4 ghc -- day8string.hs -O2 && ./day8string +RTS -s
[1 of 1] Compiling Main             ( day8string.hs, day8string.o )
Linking day8string ...
...
[solution output]
...
       4,892,080 bytes allocated in the heap
         848,552 bytes copied during GC
         394,736 bytes maximum residency (2 sample(s))
          36,256 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0         3 colls,     0 par    0.000s   0.001s     0.0002s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0003s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.000s  (  0.001s elapsed)
  GC      time    0.000s  (  0.001s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.000s  (  0.002s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    0 bytes per MUT second

  Productivity 100.0% of total user, 55.5% of total elapsed
```

This gets some statistics out of the garbage collector. [These docs][rts]
tells you what they all mean. The memory usage stats at the top are what's
interesting to us here.

[rts]: https://downloads.haskell.org/~ghc/8.4.2/docs/html/users_guide/runtime_control.html#rts-flag--s%20[%E2%9F%A8file%E2%9F%A9]

[As with my Day 6 solution][nostrings], I went through and replaced all
the usages of `[Char]` with `Text`, and functions acting on them with their
counterparts from [Data.Text][textlib], using [`Data.ByteString.readFile`][bsreadfile]
to read the data from file.

[nostrings]: #no-more-strings
[textlib]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text.html
[bsreadfile]: https://hackage.haskell.org/package/bytestring-0.10.10.0/docs/Data-ByteString.html#v:readFile

```
$ stack --resolver lts-15.4 ghc -- day8.hs -O2 && ./day8 +RTS -s
...
       1,464,056 bytes allocated in the heap
          10,264 bytes copied during GC
          44,512 bytes maximum residency (1 sample(s))
          29,216 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)
```

These numbers appear to significantly reduced, which is good.  I was interest
how much of this was attributable to laziness alone and not other reasons
why `Text` might be more efficient than `String`. I made another copy of the
script, this time using the lazy counterparts of the `Text` libraries:
[Data.Text.Lazy][lazytextlib], and [Data.Text.Lazy.IO][lazytextiolib].

```
$ stack --resolver lts-15.4 ghc -- day8lazy.hs -O2 && ./day8lazy +RTS -s
...
       3,566,680 bytes allocated in the heap
          34,224 bytes copied during GC
          86,824 bytes maximum residency (2 sample(s))
          36,056 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)
```

The numbers went back up a bit, but no where near thew level of the solution
using `String` and the [`readFile` from Prelude][preludereadfile].

I imagine there's a few ways in which `Text` has been made to be more efficient than
`String`.  It's good to know that in the rarer case where you might want to evaluate
text lazily, lazy `Text` is still _that much more efficient_ than `String`.

[lazytextlib]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Lazy.html
[lazytextiolib]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Lazy-IO.html
[preludereadfile]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:readFile

#### TODO Other

TODO: More examples where strictness made a difference.

### Lenses

### Replacing Lists with Vectors

My solution to [Day 10][day10] involved _a lot_ of manipulating lists.  There were
`fmap`s, zips, folds, concats, all over the place.  Chances are, as with the rest
of the solutions I've revisited, this logic could be simplified somewhat.  But
with it as it was, I saw it as a good exercise to convert to using Vectors and
see what the result was.

[day10]: https://adventofcode.com/2019/day/10

Firstly: which Vector? The elements are of type:

```haskell
data Point = Point
  { px :: !Int
  , py :: !Int
  , pAst :: !Bool
  }
  deriving(Show, Eq, Ord)
```

which isn't a member of `Prim` or `Storable`, so we need a [boxed Vector][vec].

Importing boxed vectors:

```haskell
import qualified Data.Vector as V
```

I set out making a copy of each function, `functionName'`, which worked
on Vectors instead of Lists, and once they all compiled: switching `main`
over to using Vectors.

As the boxed `Vector` is a member of a lot of the same typeclasses, `Funtor`,
`Monad`, `Foldable`, etc, a fair amount of the code could stay the same.

For the cases where it couldn't, there was generally a function from [Data.Vector][vec]
that did the job.

Where I was previously providing some data with coordinates by zipping the List
with integers:

```haskell
zip [0..]
```

I was able to instead call [`indexed`][vecidx]:

```haskell
V.indexed
```

[vec]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html
[vecidx]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:indexed

There was another place where I was putting a single value, `p`, in a List:

```haskell
[p]
```

I went with [`singleton`][singleton] for the vector case:

[singleton]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:singleton

```haskell
V.singleton p
```

I could have also leverage `Vector`'s `Applicative` or `Monad` instances with
`pure p` or `return p` respectively.

There were a few places where I'd made use of [List comprehensions][listcomp].
Replacing their usage with `fmap` and `filter` was fairly straight forward.
In once case it made the code much simpler.  I had a function:

[listcomp]: https://wiki.haskell.org/List_comprehension

```haskell
(\ps -> [p|p<-ps, pAst p])
```

which instead could be:

```haskell
(filter pAst)
```

When I learned a bit of Haskell a few years ago I absolutely lived off List
comprehensions.  Perhaps because I was coming from Python at the time, and it
was a point of familiarity.  These days Rust is my primary language and I'm
much more comfortable with maps and filters and the like.  I've hardly used
List comprehensions this time around and I haven't really missed them.  In fact
when I first started writing Rust I came across the [cute][cute] library that
lets you write Python style list comprehensions in Rust via a macro.  I liked
this, but after some code review feedback saying "just get good at the Rust way
of doing it", I realised I was only holding onto this as a safety blanket, and
so let it go.

I was thinking that I was mostly over List comprehensions and wouldn't use them
a huge amount going forward.  But then I found this on the internet: [Monad
comprehensions][monadcomp]! I've got to give these a spin at some point!

[cute]: https://crates.io/crates/cute
[monadcomp]: https://gitlab.haskell.org/ghc/ghc/-/wikis/monad-comprehensions

As with [converting usages of `String` to `Text`][stt], this went fairly
smoothly.  A lot of things work the same and for what isn't the same [the
documentation][vec] is pretty good. My one complaint with the docs are that I
wish related modules' docs linked to each other by default. I found myself
re-finding the package page on [Hackage][hackage] and following the link to the
other module's docs from there.

[hackage]: https://hackage.haskell.org/
[stt]: #no-more-strings

#### Performance

Let's see what effect this change had on memory usage.  Running the solution
using Lists:

```
$ stack --resolver lts-15.4 ghc --package diagrams-lib --package statistics -- day10.hs -O2 && ./day10 +RTS -s
...
problem output
...
     344,585,704 bytes allocated in the heap
      22,319,112 bytes copied during GC
         333,584 bytes maximum residency (2 sample(s))
          29,216 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       329 colls,     0 par    0.015s   0.015s     0.0000s    0.0002s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.121s  (  0.121s elapsed)
  GC      time    0.015s  (  0.015s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.136s  (  0.136s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    2,849,888,381 bytes per MUT second

  Productivity  88.8% of total user, 88.9% of total elapsed
```

And then the solution refactored to use Vectors:

```
$ stack --resolver lts-15.4 ghc --package diagrams-lib --package statistics -- day10vec.hs -O2 && ./day10vec +RTS -s
...
problem output
...
     509,421,528 bytes allocated in the heap
      38,949,152 bytes copied during GC
         298,760 bytes maximum residency (4 sample(s))
          29,320 bytes maximum slop
               0 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       487 colls,     0 par    0.027s   0.029s     0.0001s    0.0008s
  Gen  1         4 colls,     0 par    0.001s   0.001s     0.0003s    0.0008s

  INIT    time    0.000s  (  0.000s elapsed)
  MUT     time    0.166s  (  0.172s elapsed)
  GC      time    0.028s  (  0.030s elapsed)
  EXIT    time    0.000s  (  0.000s elapsed)
  Total   time    0.195s  (  0.203s elapsed)

  %GC     time       0.0%  (0.0% elapsed)

  Alloc rate    3,065,812,448 bytes per MUT second

  Productivity  85.3% of total user, 84.9% of total elapsed
```

So it performed worse with Vectors.  Taking a look at the code, I saw two
reasons why this might be.

[The problem][day10] deals with line of sight between between points on a
grid.  Part of my solution sorted the points of interest into the rays they
are on from a certain point, defined by their angle from the vertical.  It
did this by folding over a `Set` of the points of interest to construct
a `Map` of points on each ray.

```haskell
raysFromPoint :: Point -> Set.Set Point -> Rays
raysFromPoint p0 = foldl (\rays p -> Map.insertWith (++) (angleFromPoints p0 p) [p] rays) Map.empty
```
and in the Vector implementation:

```haskell
raysFromPoint :: Point -> Set.Set Point -> Rays
raysFromPoint p0 = Set.foldl (\rays p -> Map.insertWith (V.++) (angleFromPoints p0 p) (V.singleton p) rays) Map.empty
```

where `angleFromPoints` gives the angle between the vertical and the line between
two points.

```haskell
angleFromPoints :: Point -> Point -> Angle Float
```

In the List case the `(++)` is quite efficient.  The left hand side is always
a list with a single element, so Haskell only needs to allocate a new element
which points to the existing list as its tail.

For Vectors, `(++)` is [of order O(m + n)][consorder] as the whole new combined
vector has to be allocated.  Granted, `m` is `1`
in this case, but that leaves us with an operation of order `n` where previously
it was constant complexity.

[consorder]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:-43--43-
[tolist]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:toList

I also had to convert the Vector to a List in two places using [`toList`][tolist].

* I couldn't see a way to directly turn a Vector into a Set so resorted to:

```haskell
Set.fromList V.toList
```

* I wanted to flatten a `Vector Vector Points` to `Vector Points`, but [`V.concat`][vconcat]
  has signature `[ Vector a ] => Vector a`.  I couldn't find another way of doing
  that so went with:

```haskell
V.concat V.toList
```

Looking at the input data, non of the rays are going to have more than around 10
points on them, so these Vectors aren't going to ever be that large, but
both of these things are work that isn't being done in the solution with Lists
and will come at a cost.

Are there any real savings? There might be some savings from Vector's strictness,
but from what I can tell, Vector's really out perform Lists when they're being
indexed.  This solution wasn't doing any indexing.

#### Unboxed Vectors

Out of curiosity, I had a go at refactoring the solution to use [unboxed Vectors][unboxed].

[unboxed]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Unboxed.html

First I refactored the implementation to be [generic over vector types][genericvec].

[genericvec]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Generic.html

TODO

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


[^re]: By this stage, and really by the time I got to [Part 2][part2], I'm no
  longer "Re-learning" Haskell as I've gone far beyond the level I got to when I
  learned some Haskell a few years ago.  I started this blog series with
  "re-learning", so for continuity's sake I'll keep the title as it is.
