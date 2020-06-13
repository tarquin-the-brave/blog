+++
title = "Re-Learning Haskell with Advent of Code - Part 3"
date = 2020-06-13T12:35:26+01:00
images = []
tags = []
categories = []
draft = true
+++

After [Part 2][part2], I wanted to go and do some reading. I felt like I could
crack on with the Advent of Code problems with the tools I had at my disposal,
but felt if I learned more Haskell I could be doing better.  I wanted to learn
some more about monad transformers and anything else that could improve my
solutions.

So I started search around to see what resources I could find.

[part2]: https://tarquin-the-brave.github.io/blog/posts/re-learning-haskell-2/

# (Re-)[^re]Learning Haskell with(out) Advent of Code

After looking around a little, I stumbled upon [the Haskell learning
resources shared by FP Complete][fpch]. There's so many goodies here!

[fpch]: https://tech.fpcomplete.com/haskell/learn

I ran through tutorials on:
- Applicative Syntax,
- String Types,
- Strictness,
- lenses,
- The RIO library,
- Monad Transformers, and
- Vectors

This only scratched the surface of what's available there.  It's definitely a
resource I will be revisiting.

Armed with these newly learnt learnings, I went back to my Advent of Code
solutions to see how I could improve them.

---

# Small Improvements

Some small improvements I picked up as handy tips or on a 2nd look saw as
obvious and trivial.

## Writing Stack Scripts

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

## Using `where` More

As I elucidated on in [Part 2][part2], my earlier solutions were plagued with
the proliferation of small functions that don't really mean a huge amount on
their own and are only called as part of another function.  `where` helps to
clean this up by putting them inside the namespace of the function they're
really part of and de-cluttering the namespace of the module.

## Use `foldl'` Over `foldl`

"For strict application of the operator" - [the docs say][folddoc].

As I understand it, a lazy fold would build up a giant chain of thunks which,
if the folding operation were cheap, would be more costly than strict
evaluation.

[folddoc]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Data-Foldable.html#v:foldl-39-

## Where a List Was Very Slow

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

## Deriving Instances

In Rust, I use the [`#[derive()]`][rustderive] attribute _a lot_. More often
than not, defining some `struct`s and `emun`s to declare what aggregates of
data matter becomes the cornerstone of any program I write.  Derive macros, via
[`serde`][serde] and [`structopt`][structopt], are "how we do" serializing and
deserializing data, and building CLIs.  I've even written my own derive macros
recently. It's fair to say: Rust would be a very different language without
derive macros.  I was going to say "completely fucking unusable", but I started
to imagine a world where defining your own types was significantly more
expensive, where people defined their data aggregates in a more anonymous, ad
hoc manner, using generally available collection types, with a smattering of
type aliases.  Perhaps there's some advantages to that style.  At least it
would stop people writing reams of code to build spaghetti-like systems of
entirely entangled "objects" that go right round the houses, dig holes, fill
them back up again, pull in some state from who knows where or when, all just
to "do a thing", espousing virtues of "encapsulation" and [DRY (at all
costs)][dry].  I've seen code like that in Rust.  Defining your own types
provides a lot of control and correctness to your program, as you can lean hard
on the type system, but perhaps that can be taken too far.

Anyway, while writing Haskell I've been trying to use `deriving` as much as
possible. One thing I picked up from [FP Complete's tutorials][fpch] was the
`DeriveFunctor` language extension. In [Part 2][part2] I refactored my Intcode
Computer solution to define and use my own Monad instance.  It was good to go
back and delete the 5 lines of code that defined the Functor instance.

```haskell
data Prog a = Running a | AwaitInput a | End a | Crashed String deriving(Show, Eq)

instance Functor Prog where
  fmap _ (Crashed e) = Crashed e
  fmap f (End a) =  End (f a)
  fmap f (Running a) = Running (f a)
  fmap f (AwaitInput a) = AwaitInput (f a)
```

became:

```haskell
{-# LANGUAGE DerivingFunctor #-}

data Prog a = Running a | AwaitInput a | End a | Crashed String deriving(Show, Eq, Functor)
```

Little victories. No code, no bugs. :bug:

[rustderive]: https://doc.rust-lang.org/reference/attributes/derive.html
[serde]: https://serde.rs/
[structopt]: https://docs.rs/structopt/0.3.14/structopt/
[dry]: https://tarquin-the-brave.github.io/blog/posts/dry-not-a-goal/

## Using Git Dependencies with Stack

As part of my refactored Intcode Computer solution in [Part 2][part2] I had to
fork and update the `base` version in [a library that provided testing of
Monad, Applicative, Functor, and Monoid Laws][tastylaws].  I then included my
fork as a [git submodule][gitsub] and pointed Stack at the dependency in the
local file system.  This was a good bit of practice with git submodules.  I've
found git submodules to be something that you never use, until you have to, and
then you get it all spectacularly wrong and end up very confused.  But as the
theme of this post is about tidying up and refactoring, I changed this to tell
stack to get my fork [from git][stackgit].

[tastylaws]: http://hackage.haskell.org/package/tasty-laws
[gitsub]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
[stackgit]: https://docs.haskellstack.org/en/stable/yaml_configuration/#extra-deps

---

# Big Changes

For me, the biggest difference coming back to my solutions was more down to
having more experience and confidence in the language, and coming at the code,
with a view to cleaning up the expressions and abstractions rather than wanting
to get the answer to plug into Advent of Code so it'll give me a gold star.
:star:

Looking past the general code tidying, there were some specific things I'd
learned from doing some of [FP Complete's tutorials][fpch] that I was able to apply to
my refactored solutions.

## Stop Matching Maybes - More Monad Transformers

_Not a great first example, as it's very much a "Big Not-Change", but I thought
it was worth a mention._

After going through some of FP Complete's tutorials on monad transformers, I
thought that I'd go back to my solutions finding vast swathes of code, matching
on `Maybe`s and rip it all out in favour of monad transformer stacks.

That didn't actually happen. There was actually relatively few cases where I
was matching `Maybe`s and they were quite tidily cleaned up by making the
function generic over `MonadFail` in stead, see below.

It could be that there weren't lots places where monad transformers could have
been used to really clean up my code.  Or perhaps the concept hasn't sunk in
enough for me to spot those places.  I had used `StateT` in one of my solutions
already so I could print out the state to terminal as it was evolving (covered
in [Part 2][part2]).  It might be easier to spot where they could be used when
I'm writing something fresh, rather than revisiting and refactoring code that
was to some extent written around not knowing to use them.

## Alternatively, Use MonadFail

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

## No More Strings

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

### Strictness

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

[As with my Day 6 solution][nostrings], I went through and replaced all the
usages of `[Char]` with `Text`, and functions acting on them with their
counterparts from [Data.Text][textlib], using
[`Data.ByteString.readFile`][bsreadfile] to read the data from file.

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
how much of this was attributable to laziness alone and not other reasons why
`Text` might be more efficient than `String`. I made another copy of the
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

The numbers went back up a bit, but no where near the level of the solution
using `String` and the [`readFile` from Prelude][preludereadfile].

I imagine there's a few ways in which `Text` has been made to be more efficient
than `String`.  It's good to know that in the rarer case where you might want
to evaluate text lazily, lazy `Text` is still _that much more efficient_ than
`String`.

[lazytextlib]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Lazy.html
[lazytextiolib]: https://hackage.haskell.org/package/text-1.2.4.0/docs/Data-Text-Lazy-IO.html
[preludereadfile]: https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#v:readFile

## Lenses

Lenses are awesome.

In my intcode computer code, which the building of and using was the focus of
[Part 2][part2], I had defined the core "intcode data" in its own module, that
consisted of an `Intcode` type:

```haskell
data Intcode = Intcode {
  input::[Int],
  code::[Int],
  -- ip: Instruction Pointer
  ip::Int,
  -- rb: Relative Base
  rb::Int,
  output::[Int]
} deriving(Show, Eq)
```

and some functions that edit the subfields, such as:

```haskell
moveIp :: Int -> Intcode -> Intcode
moveIp i ic = setIp (i + ip ic) ic

setIp :: Int -> Intcode -> Intcode
setIp i ic = Intcode{
    input = input ic,
    code = code ic,
    ip = i,
    rb = rb ic,
    output = output ic
}

changeRb :: Int -> Intcode -> Intcode
changeRb b ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = (rb ic) + b,
    output = output ic
}

consOutput :: Int -> Intcode -> Intcode
consOutput o ic = Intcode{
    input = input ic,
    code = code ic,
    ip = ip ic,
    rb = rb ic,
    output = o:(output ic)
}
```

With the exception of some like `moveIp` that called other functions,
they all followed the same pattern of "make a new `Intcode` with one
field different.

The full list of them could be seen in export statement of the module:

```haskell
module Intcode.Data
    ( Intcode (..)
    , newIC
    , moveIp
    , setIp
    , changeRb
    , updateCode
    , tailInput
    , setInput
    , consInput
    , appendInput
    , setOutput
    , consOutput
    , scrubOutput
    ) where
```

A lot of copy pasting and Vim macros were used when I first wrote this out.

I did later find out that Haskell has a built-in way for dealing with this
boiler plate as an instance of a record type can be referred to with respect to another,
only mentioning the records that change.  E.g. `consOutput` from above could have been
written:

```haskell
consOutput :: Int -> Intcode -> Intcode
consOutput o ic = ic { output = o:(output ic) }
```

And these can be chained, so you could have something like:

```haskell
ic { output = o:(output ic) } { ip = ip ic + 1 }
```

This would have massively reduced the boilerplate in this module on its own.  But
we can go one better with lenses.

Using the [`microlens-platform`][microlensplat] package, to also get auto generation
of lenses with Template Haskell as well as the functions from `microlens`, I was
able to refactor these functions to simple single liners such as:

```haskell
consInput :: Int -> Intcode -> Intcode
consInput i = over input (\inp -> i:inp)
```

And the "set" functions became entirely trivial:

```haskell
setIp :: Int -> Intcode -> Intcode
setIp = set ip
```

As the functions had become so trivial I decided to remove them entirely and in stead
export the lenses for calling code to use.

The previously 130+ lines of code module became:

```haskell
{-# LANGUAGE TemplateHaskell #-}
module Intcode.Data
    ( Intcode
    -- Intcode Lenses
    , input
    , code
    , ip
    , rb
    , output
    -- init
    , newIC
    ) where

import Lens.Micro.Platform (makeLenses, over, set)
import qualified Data.Sequence as S

--
-- Intcode data
--
data Intcode = Intcode
  { _input::[Int]
  , _code :: S.Seq Int
  -- ip: Instruction Pointer
  , _ip::Int
  -- rb: Relative Base
  , _rb::Int
  , _output::[Int]
  } deriving(Show, Eq)

makeLenses ''Intcode

newIC :: S.Seq Int -> [Int] -> Intcode
newIC newCode newInput = Intcode{
  _input = newInput,
  _code = newCode,
  _ip = 0,
  _rb = 0,
  _output = []
}
```

(I also [refactored the intcode data to be held in a Sequence][seq]).

[seq]: #using-sequence-to-model-intcode
[microlensplat]: http://hackage.haskell.org/package/microlens-platform

This basic use of lenses with: auto-generation, getting, setting, and modifying
seems an obvious win for whenever I'm defining my own data aggregate like this.
When I have types nested inside types, lenses' composability will come in
handy.  I think it's going to be my default approach in future.

I looked through a couple of my other solutions to see where I had defined some
non-trivial aggregation of data where I could clean up the code with lenses.
My solutions to [Day 11][day11] and [Day 13][day13] both followed the same
pattern:

* Defined the state that matters in a type, `s`,
* Write a "step function" to progress that state, of the form `s -> (a, s)`,
  where `a` is some output of the computation,
* Wrap that in [the State Monad][state],
* And recurse, until some condition is met.

[Part 2][part2] covers these solutions in more detail.

[day11]: https://adventofcode.com/2019/day/11
[day13]: https://adventofcode.com/2019/day/13
[state]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html

I started to look at refactoring these solutions to use lenses.  But the "step
functions" were pretty much the only places lenses could be applied, and these
functions did a "bulk update" of the state where every record is being changed.
In this cases, I think the record syntax looked clearer than a long chain of
lenses.

## Deriving Default

While I was [refactoring the intcode solution to use lenses](#lenses) to
access, fields in the `Intcode` type, I also wanted to refactor the "new"
function, so it wasn't calling the record functions, now prepended with an
underscore, directly.

In Rust I'd do:

```rust
#[derive(Default)]
struct SomeData {
    foo: usize,
    bar: String,
    baz: Vec<String>,
}

impl SomeData {
    fn new(bar: String) -> Self {
        Self {
            bar,
            ..Default::default()
        }
    }
}
```

([This code in Rust playground](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=b15e306d979834af9ca6fe13d48aacc4)).

I was hoping to achieve this with something similar in Haskell, like:

```haskell
--
-- DOES NOT COMPILE
--
{-# LANGUAGE TemplateHaskell #-}
import Lens.Micro.Platform (makeLenses, set)
import qualified Data.Sequence as S
import Data.Default

data Intcode = Intcode
  { _input::[Int]
  , _code :: S.Seq Int
  , _ip::Int
  , _rb::Int
  , _output::[Int]
  } deriving(Show, Eq, Default)

makeLenses ''Intcode

newIC :: S.Seq Int -> [Int] -> Intcode
newIC c i = set code c . set input i $ def
```

This failed to compile with:

```
error:
    • Can't make a derived instance of ‘Default Intcode’:
        ‘Default’ is not a stock derivable class (Eq, Show, etc.)
        Try enabling DeriveAnyClass
    • In the data declaration for ‘Intcode’
   |
29 |   } deriving(Show, Eq, Default)
   |                        ^^^^^^^
```

So I included the `DeriveAnyClass` language extension, then:

```
 error:
    • No instance for (GHC.Generics.Generic Intcode)
        arising from the 'deriving' clause of a data type declaration
      Possible fix:
        use a standalone 'deriving instance' declaration,
          so you can specify the instance context yourself
    • When deriving the instance for (Default Intcode)
   |
29 |   } deriving(Show, Eq, Default)
   |                        ^^^^^^^
```

`GHC.Generics.Generic`, I can "hoogle" that. Following a few links lead me to
the [Generics.Deriving.Default][gendef] module.

[gendef]: http://hackage.haskell.org/package/generic-deriving-1.13.1/docs/Generics-Deriving-Default.html

After some fighting with the compiler and adding a bunch of language extensions
that it was asking me to, one by one, I realised this isn't what I want.  This
module appears to be a way to derive a default implementation of some class
instances rather than deriving the ability to default the value of type
`Intcode`.

Going back to the last error I tried also deriving `Generic`, as that was shown
in the examples in [Generics.Deriving.Default][gendef], and examples on the
homepage of the [data-default-extra][defextra] which I also looked at while
googling around.

[defextra]: https://hackage.haskell.org/package/data-default-extra

```
error:
    • Can't make a derived instance of ‘Generic Intcode’:
        You need DeriveGeneric to derive an instance for this class
    • In the data declaration for ‘Intcode’
   |
32 |   deriving(Generic, Default, Show, Eq)
   |
```

I was anticipating another merry-go-round of the compiler telling me to add
language extensions one by one, but adding `DeriveGeneric` made this work.

The finished article looks like:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Lens.Micro.Platform (makeLenses, set)
import qualified Data.Sequence as S
import GHC.Generics (Generic)
import Data.Default (Default, def)

data Intcode = Intcode
  { _input::[Int]
  , _code :: S.Seq Int
  -- ip: Instruction Pointer
  , _ip::Int
  -- rb: Relative Base
  , _rb::Int
  , _output::[Int]
  }
  deriving(Generic, Default, Show, Eq)

makeLenses ''Intcode

newIC :: S.Seq Int -> [Int] -> Intcode
newIC c i = set code c . set input i $ def
```

So deriving a `Default` instance isn't quite as easy as in Rust, and took a bit
of playing around, but I'm happy that it was reasonably simple once I worked it
out.

## Replacing Lists with Vectors

My solution to [Day 10][day10] involved _a lot_ of manipulating lists.  There
were `fmap`s, zips, folds, concats, all over the place.  Chances are, as with
the rest of the solutions I've revisited, this logic could be simplified
somewhat.  But with it as it was, I saw it as a good exercise to convert to
using Vectors and see what the result was.

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

I set out making a copy of each function, `functionName'`, which worked on
Vectors instead of Lists, and once they all compiled: switching `main` over to
using Vectors.

As the boxed `Vector` is a member of a lot of the same typeclasses, `Funtor`,
`Monad`, `Foldable`, etc, a fair amount of the code could stay the same.

For the cases where it couldn't, there was generally a function from
[Data.Vector][vec] that did the job.

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
Replacing their usage with `fmap` and `filter` was fairly straight forward.  In
once case it made the code much simpler.  I had a function:

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

### Performance

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

In the List case the `(++)` is quite efficient.  The left hand side is always a
list with a single element, so Haskell only needs to allocate a new element
which points to the existing list as its tail.

For Vectors, `(++)` is [of order O(m + n)][consorder] as the whole new combined
vector has to be allocated.  Granted, `m` is `1` in this case, but that leaves
us with an operation of order `n` where previously it was constant complexity.

[consorder]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:-43--43-
[tolist]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:toList

I also had to convert the Vector to a List in two places using [`toList`][tolist].

I couldn't see a way to directly turn a Vector into a Set so resorted to:

```haskell
Set.fromList V.toList
```

I wanted to flatten a `Vector Vector Points` to `Vector Points`, but
[`V.concat`][vconcat] has signature `[ Vector a ] => Vector a`.  I couldn't
find another way of doing that so went with:

[vconcat]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:concat

```haskell
V.concat V.toList
```

UPDATE: It looks as if using [`V.concatMap`][vconcatmap] would have avoided the
need to use the second of these `V.formList`s.  What effect that would have on
performance, I don't know.

[vconcatmap]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector.html#v:concatMap

Looking at the input data, non of the rays are going to have more than around
10 points on them, so these Vectors aren't going to ever be that large, but
both of these things are work that isn't being done in the solution with Lists
and will come at a cost.

Are there any real savings? There might be some savings from Vector's
strictness, but from what I can tell, Vector's really out perform Lists when
they're being indexed.  This solution wasn't doing any indexing.

## Using Sequence to Model Intcode

From the offset, when I first implemented a solution to [Day 2][day2], which
introduces the problem of implementing an intcode computer, I knew storing the
intcode data in a List was going to be bad for performance as the intcode data
needed to be reference by index _a lot_ and have values at specific indices
updated.

I made a note to come back to it later, figuring that as the intcode computer
implementation is built up over days 2, 5, 7, and 9, after implementing them
I know all the things I need to do with the data, and choose a replacement for
List then.  This did mean that I'd built up a solution around using Lists,
but I backed myself to keeps things modular.

After considering a few options, I decided I liked what I saw in [`Data.Sequence`][sequence]:

* "Logarithmic-time access to any element",
* "Logarithmic-time concatenation",
* "Constant-time access to both the front and the rear" - appending and prepending,
* The ability to change a value at an index with `update`, and
* A List-like interface.

[sequence]: https://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Sequence.html
[day2]: https://adventofcode.com/2019/day/2

The refactor went pretty smoothly.  The List-like interface meant a lot of
functions could be replaced like-for-like and those that couldn't had a fairly
obvious replacement found in [the docs][sequence].  It having instances of
`Functor`, `Foldable`, and `Monad` meant a lot of the code could stay the same.

The part of the code that looked after updating values in the intcode,
according to the rules set in the problem, ended up looking a lot cleaner.

Where in the implementation with lists we had:

```haskell
replaceNth :: Integral a => MonadFail m => Int -> a -> [a] -> m [a]
replaceNth n newVal xs
  | n < 0 = fail "Cannot replace element with negative index"
  | n < length xs = return $ replaceNthInner n newVal xs
  | n == length xs = return $ xs ++ [newVal]
  | n > length xs = return $ xs ++ [0 | _ <- [1..(n - length xs)]] ++ [newVal]

replaceNthInner :: Int -> a -> [a] -> [a]
replaceNthInner _ _ [] = []
replaceNthInner n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNthInner (n-1) newVal xs
```

This was cleaned up, and made to match the sequence nomenclature:

```haskell
update :: Integral a => MonadFail m => Int -> a -> S.Seq a -> m (S.Seq a)
update n newVal xs
  | n < 0 = fail "Cannot replace element with negative index"
  | n < S.length xs = return $ S.update n newVal xs
  | n == S.length xs = return $ xs S.|> newVal
  | n > S.length xs = return . flip (S.|>) newVal $ xs S.>< (S.replicate (n - S.length xs) 0)
```

`S.update` brought a lot of this clean up as it removed the need for
`replaceNthInner`.  The first class ability to append, with `S.|>`, also got
rid of the `++ [newVal]` which I've always found a bit nasty.

I didn't examine what the performance effect was of this refactor. It's on
my bucket list to read up on proper performance profiling.  When I do, this
would be a good test case to come back to and benchmark.

# Some Failed Attempts

There were some things that I had a go at, but after realising I'd bitten off
more than I wanted to chew at that moment, left them as something to come back
to, or tried something else.

## Generalising over Vectors

After converting my [Day 10][day10] solution from using Lists to boxed Vectors,
and finding the [performance to be worse](#performance), I was going to see
what the effect of refactoring the solution to use [unboxed Vectors][unboxed]
would be.

[unboxed]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Unboxed.html

My plan for this was to refactor the solution to be generic over vector types
by writing it in terms of [Data.Vector.Generic][genericvec], and then swapping
the concrete vector type used, at (hopefully) the single point where that was
defined.

[genericvec]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Generic.html

I made a start on this, and I'm reasonably happy with a process of swapping out
the module behind the qualified import, and where there are compile errors,
using [the documentation][genericvec] to find an equivalent function.  The
only issue was there was a lot of this to do, and I lost the will to do it somewhat.
I think a better approach will be: next time I'm writing something to use
Vectors, try to write it with generic Vectors and see where that leads.

There was one specific thing I hit, that I worked around, but didn't really
understand. In the boxed Vector solution I defined the "rays" that different
grid points lived on with a map of angle from the vertical to a Vector of
grid points (`Point`).  I had the type alias:

```haskell
type Rays = Map.Map (Angle Float) (V.Vector Point)
```

In making this generic over vectors, I changed this to:

```haskell
type Rays v = V.Vector v Point => Map.Map (Angle Float) (v Point)
```

which worked just fine in all but 3 functions' type signatures. I had some functions
to wrap these "rays" in the [State monad][state], with one function providing
the `s -> (a, s)`, where `s` is "the rays" and another to wrap that in `state`,
and a third to recursively evolve the state until a condition is met.

Take a look at the "`s -> (a, s)` function", called `shootInner`, the compiler
complained about the type signature:

```haskell
shootInner ::  V.Vector v Point => Int -> Rays v ->  ((Point, Int), Rays v)
```

```
error:
    • Illegal qualified type:
        V.Vector v Point => Map.Map (Angle Float) (v Point)
      GHC doesn't yet support impredicative polymorphism
    • In the expansion of type synonym ‘Rays’
      In the type signature:
        shootInner :: V.Vector v Point =>
                      Int -> Rays v -> ((Point, Int), Rays v)
   |
66 | shootInner ::  V.Vector v Point => Int -> Rays v ->  ((Point, Int), Rays v)
   |                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

The same complaint was levied against the other two functions with signatures:

```haskell
shoot :: V.Vector v Point => Int -> State (Rays v) (Point, Int)
shootUntil :: V.Vector v Point => Int -> Int -> State (Rays v) Point
```

(you can read [the problem description][day10] for the context behind the names,
basically you spin round and shoot asteroids that are located on the rays).

I reduced the script around the error to it's simplest form to try and
isolate the source of the error:

```haskell
#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package containers --package vector
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import qualified Data.Map.Strict          as Map
import qualified Data.Vector.Generic      as V

main :: IO ()
main = print "Hello"

type Foo v = V.Vector v Int => Map.Map Int (v Int)

bar :: V.Vector v Int => Int -> Foo v -> Foo v
bar = undefined

baz ::  V.Vector v Int => Int -> Foo v ->  ((Int, Int), Foo v)
baz = undefined
```

I hit the same error about "[impredicative polymorphism][imppol]" here a well.

[imppol]: https://gitlab.haskell.org/ghc/ghc/-/wikis/impredicative-polymorphism

After a bit of changing things to see what happens, I found that removing
the constraint from the type alias allowed this to compile:

```
type Foo v = Map.Map Int (v Int)
```

I did a bit of googling around "impredicative polymorphism" and it looks like
I've got a bit of reading to do before I understand this one, and why it was
hit for these functions but not others that both take and return `Rays v`.

## Mutable Vector to Model Intcode

Before I settled on [choosing a Sequence to represent the intcode
program][seq], I was looking into representing it in a [mutable Vector][mutvec]
as elements have to be changed a lot, I thought that might be more efficient.

[mutvec]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html

But unlike with Sequence, where I was able to pretty much swap out Lists for
Sequences, changing a few function calls, but otherwise hardly changing the
surrounding functions or structure of the program, with mutable Vectors I'd
need a fair amount of refactoring.  The [functions that access
elements][mutvecelems] all return a monad that would need handling.

[mutvecelems]: https://hackage.haskell.org/package/vector-0.12.1.2/docs/Data-Vector-Mutable.html#g:10

In this case as well I thought a better approach would be to earmark mutable
vectors as something to try when attempting a future problem, rather than
trying to do a big refactor of an existing solution.

# Future Improvements

## No Prelude and RIO

Now that I've refactored a fair few of my solutions to not use `String` and
`List`, I'm hardly using Prelude at all in some of them.

I think in next Advent of Code problem I attempt, I'll start by putting

```haskell
{-# LANGUAGE NoImplicitPrelude #-}
```

at the top of the file and using [RIO][rio] as a Prelude replacement and
build the solution inside the RIO monad.

[rio]: https://hackage.haskell.org/package/rio

# What Next?

More Advent of Code I guess...

Now I've done a "big refactor" of my existing solutions, the next thing to do
would be to crack on with some new Advent of Code problems.  I'm not in any
particular rush to get through them.  I'm finding them a decent backdrop to
learn things with, and the sooner I finish them, the sooner I have to look
elsewhere for fresh problems.  I'm definitely in the state of mind of seeing
how much I can learn from doing a solution before moving onto the next one.

In the process of attempting more problems I definitely going to return to [FP
Complete's Tutorials][fpch], both for when I'm looking to learn something
specific that they cover, and for when I'm speculating on what might be useful
for an Advent of Code problem. I see they have a tutorial on performance
profiling which I'm keep to look into.  In this blog post I've not been
particularly rigorous when commenting on performance, and at some point I'd
like to do some proper benchmarking on the choice of types in some solutions.

_All the changes I've made are recorded in [this monster pull request][pr]
(which seems to have done a decent job of following when I changed entire Stack
project directories into single page scripts._ :dancer:

[pr]: https://github.com/tarquin-the-brave/aoc-19-haskell/pull/1/files

[^re]: By this stage, and really by the time I got to [Part 2][part2], I'm no
  longer "Re-learning" Haskell as I've gone far beyond the level I got to when I
  learned some Haskell a few years ago.  I started this blog series with
  "re-learning", so for continuity's sake I'll keep the title as it is.
