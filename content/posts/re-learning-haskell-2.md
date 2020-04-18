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

TODO

- Interesting pattern
- `MonadFail` seems pretty limited to `List` `Maybe` & `IO`.
-

# Breaking Functions Into Lots of Pieces

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
of the module documentation's webpage.  Spot `mtl-` in [the
docs][statemonaddocs] for `Control.Monad.State.Lazy`.

[statemonaddocs]: https://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State-Lazy.html

On the second of these: I've got reasonably comfortable with either
doing a qualified import to preseve namespacing, or being explicit
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

I've found using a combination of these: qwualified import and
explicitly listing the functions is a bit over the top.

I also practiced a pattern of having a module which renames
functions from another module when the names of functions and
types didn't make as much sense in the context I was using them,
or were just down right silly names in the first place.

```haskell
module sensible
  ( foo
  , bar
  , Baz (..)
  , Silly.okName
  )

import Very.Silly.Named.Module as Silly



```

because there's no objects, namespacing is a bit funky.
e.g. non-empty list has most of the list methods but needs
a `qualified` import.
* I suppose without objects you need some way of namespacing.
  That's all "objects" are really.
  + Procedures in the same namespace as data.
  + I'm always disappointed to find out how "not a thing" a thing is.

# general observations

Safety:
* I want to write "safe" code
* in Rust you have to be explicit both:
  + when you want to do something not "memory safe" - haskell is GC, not a problem
  + when you want to do something that "may crash/panic" - akin to haskell notion of safety"
    - e.g. you got to stick `unwrap()` or `expect()` around the place.
* I'm finding in Haskell
  + safe code comes with faff
  + so if you know your usage won't touch an unsafe case, should you still make the effort?
    - for the code itself, it's probably pointless,
    - but what about extensibility/maintainability?
    - should we just write safe code all the time and accept the cost of doing so as:
      * a) part and parcel of writing decent code,
      * b) worth it in the medium/short term?
    - a down side of learning via these problems is there's not the drive to make code maintainable,
      you don't necessarily get the lessons about maintenance.
    - you do have to extend code though, so I suppose extensibility comes into it.

generality
* generality is good right?
* but means breaking changes to library - e.g. `replaceNth` went from:
  + `Int -> a -> [a] -> [a]`, to:
  + `Num a => Int -> a -> [a] -> [a]`

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


[^functions]: I'm careful to not call something a function if it isn't a pure function.
              The ship sailed a bit on that one as many languages call their inpure methods
              "functions", but I find the distinction useful, not least to help context
              switch when moving between procedural and functional frames of mind.

[^plop]: [A great talk by Rich Hickey][valueofvalues] "The Value of Values" talks
         about PLOP "Place Oriented Programming".

[^simple]: [Another great talk by Rich Hickey][simplemadeeasy] "Simple Made Easy"
           talks about what simplicity is and how we can think objectively about it
           rather than it being a stand in for what people are most familiar with.
