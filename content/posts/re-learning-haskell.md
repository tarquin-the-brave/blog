+++
title = "Re-Learning Haskell with Advent of Code"
date = 2020-03-18T20:18:05Z
images = []
tags = []
categories = []
draft = true
+++

A few years ago I learned myself a Haskell for greater good from
a book "Learn you a Haskell for greater good"[^1] and the first few
chapters of another book "Haskell from first principals"[^2].

It was fun to learn.  I didn't go onto use Haskell to build any real
applications, beyond coding up some exercises and mini programs to
learn things.

I believe I got a lot from it.  At the very least it's joyful to solve
problems in Haskell.  When I started writing Rust a year later I liked
the strong type system and the features inspired by functional languages:
maps, folds, etc.  It gave me an appreciation of the benefits of
immutability and pure functions.  I believe that's fed into improving
how I build things in other languages.

A colleague told me about how they did Advent of Code[^3] to learn Rust and
a wider group of colleagues have decided to do a couple of exercises every
week, to learn a variety of languages, and meet fortnightly to discuss
approaches.  I saw this as a good opportunity to "pick up Haskell again".
So I did.

# Week 1

## Build Tooling

First thing's first.  Build tooling.  I didn't really bother with
this the last time around.  I spent most of the time playing in GHCi,
the interactive Haskell terminal, and importing most of the Haskell
modules I wrote into that, rather than compiling and running an
executable.

Over the last few years I've learned: first thing to do after cloning
a codebase: build it! And if you can't rebuilt at the touch of a button,
do the work immediately to make sure you can.  I've spent time faffing
on with linking issues with C++ applications.  I've spent time faffing
on with Python's dependency management, or lack thereof[^4].  It's not time
well spent.  It's time lost.  I've been writing Rust at work for about
a year and a half.  Rust has [cargo][cargo].  Cargo is phenomenal.  It's a great
bit of tooling.  I'll not lie to you: when facing technical decisions,
especially around tooling, I quite often find myself asking "WWCD?", "What
Would Cargo Do?".

I'm picking up a language, having build tooling that's rock solid,
and available at the touch of a button is paramount.  So before writing
a line of code, or even reading the first day's challenge, I went looking
for build tooling.

I did a bit of googling with phrases like "Haskell toolchain", "Haskell
build tools comparison", and found various resources ranging from blog
posts to official documentation.  I found [this post][opinion-guide-haskell]
quite informative.  It purports to be "An opinionated guide".  That's good.
Developers having opinions is good.  Especially when you consider the
alternative: _not having thoughts_.  It's a long document.  I didn't read
nearly half of it.  But funnily enough the first section in this "guide
to Haskell", was on build tooling.  It touched on three options of cabal-install,
stack, and nix, and proceeded to do fairly good job of selling stack.
Combined with a friend telling me that "stack is the closest thing to cargo
that Haskell has got", I was sold on stack, at least for the meantime.
Nix is on my bucket list, just not today.

So I found and bookmarked the [official documentation][stack-docs] and
ran `stack new day1` to create a project for the problems in "day 1" of
the Advent of Code.

## Retracing the first steps

functor -> computation applied in a context
applicative -> computation applied in in two instances of a context to be combined into
one instance of the context
monad -> chaining computations that create a context, keeping results within the context.

This was about as far as I got in understanding last time.

## TODO: Points to make

week 1 -> problems 1,3,4,6,8 -> went back to 2

I didn't setup _any_ editor integration!

packaging/importing/exporting
* `import Lib` or `import Data.Sequence` will bring all that a module exports
  flat into the current name space.
  * makes code harder to read "as text"
  * contrast to rust -> rust is explicit
* This can be fixed by doing X in haskell
* In Haskell's defence:
  * Rust imports most methods via `impl` and Traits, so importing the Data and the trait
    is sufficient.
  * Haskell you'd have to import all functions explicitly... maybe unwieldy, maybe not.
* I'll try this, if it becomes too much, I'll setup some editor integration.

Small things I learned -> code examples?
* trees

Big things I learned -> ideas?

General feelings about the language?
- less explicit that Rust

I'm solving all these problems with tools I already know how to use, lists, folds, etc.
partly because "the basic tools" are so powerful.

Coding problems sometimes mean spending most of the time solving the problem
and not learning new concepts and how to express things well in a language.

gonna do some reading, then come back to these.

# Week 2

* Reading week
* no problems
* Have AoC support meeting
* setup editor integration?

[opinion-guide-haskell]: https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
[cargo]: TODO
[stack-docs]: https://docs.haskellstack.org/en/stable/README/

[^1]: Learn you a Haskell for greater good. http://learnyouahaskell.com/chapters
[^2]: Haskell from first principals. LINK
[^3]: Advent of code provided a coding challenge on each day of December.
[^4]: [Poetry]() does quite a good job of Python dependency management.
      It's not totally watertight, but it's a huge improvement on [pip]() & [Pipenv]()
      to which my derision is directed.

